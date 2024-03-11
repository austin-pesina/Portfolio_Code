################################################################################################
# This scripts scrapes game log data for NBA players based on rosters listed
# on basketball-reference.com. The first function loops through each team's 
# page to pull their roster. The link to each player's page is also mined
# and used to pull their stats for each game played. To prevent getting 
# IP blocked, there is an 8 second sleep after each player. 
################################################################################################




library(rvest)
library(tidyverse)
library(stringi)
library(DBI)
library(odbc)
library(RMySQL)


# Create a trim function which uses regular expressions to clean white space
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

# List of all teams
teams <- c("ATL", "BOS", "BRK", "CHI", "CHO", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM",
           "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")


##### List of all Players #####

# Empty data set for for-loop
players_all <- data.frame(name=character(), 
                          team=character())  

player_scrape <- function(season){
  for(i in 1:length(teams)){
    # Website to scrape
    url <- paste0("http://www.basketball-reference.com/teams/",teams[i],"/",season,".html#all_roster") 
    
    # Read in the URL, rename the Player column and add the team name
    players <- read_html(url) %>% 
      rvest::html_nodes("#roster") %>% 
      rvest::html_table() %>% 
      .[[1]] %>% 
      dplyr::select(Player) %>% 
      dplyr::rename(name = Player) %>% 
      dplyr::mutate(team = teams[i])
    
    
    # Empty data set for for-loop
    all_links <- data.frame(html = character()) 
    
    for(j in 1:nrow(players)){
      # Player link url for scraping individual player pages
      get_links <- read_html(url) %>% 
        html_nodes( paste0("tr:nth-child(", j,") a")) %>% 
        html_attr("href") 
      
      link <- data.frame(get_links[1]) %>% 
        dplyr::rename(html=1)
      
      all_links <- rbind(all_links, link) 
      
      Sys.sleep(8)
    }
    
    # Column Bind player data and links
    players <- cbind(players, all_links)
    
    # Bind all players into one table
    players_all <- rbind(players_all, players)  
    # Progress bar
    print(paste0("Finished Team: ", teams[i])) 
  }
  
  return(players_all)
  
}


season_2023 <- player_scrape(2023)


# Split player name into First & Last and update html path for scraping stats
list_of_players <- season_2023 %>% 
  dplyr::mutate(name = str_replace_all(name, "\\(TW\\)", ""),
                first = trim((lapply(strsplit(as.character(name), split = " "),"[",1))),
                last = trim((lapply(strsplit(as.character(name), split = " "),"[",2))),
                new_html = trim(gsub(".html", "", html)))
player <- list_of_players$name
list_of_teams <- list_of_players$team

# Create list for html path
path <- list_of_players$new_html





##### Scraping All Stats & Ratings #####



stats_scrape <- function(season){
  
  # Empty data frame for stats
  stats <- data.frame()

  log_url <- paste0("http://www.basketball-reference.com",path,"/","gamelog/",season,"/")
  
  for(i in 1:nrow(list_of_players)){
    tryCatch({
      # Scrape data from game logs
      html <- read_html(log_url[i])
      # Data is in the #gpl_basic tag
      games <- html_node(html,"#pgl_basic")
      games_table <- html_table(games, fill=T) %>% 
        # Rename columns(place = home/away)
        dplyr::rename(place = 6,
                      result = 8) %>% 
        # Filter out empty cells or rows where the headings repeat
        dplyr::filter(G != "" & G != "G") %>% 
        # Convert Games (G) to numeric and add player/team columns
        dplyr::mutate(G = as.numeric(G),
                      player = list_of_players$name[i],
                      team = list_of_teams[i])
      
      Sys.sleep(6)
      
      # Combine all stats
      stats <- rbind(stats, games_table)
      
      print(paste0("Finished Player: ", list_of_players$name[i], " from ", list_of_players$team[i]))
      
    })
  }
  
  return(stats)
  
}
    
stats_2023 <- stats_scrape(2023)
  
# Create primary key for each player. Regex pulls the pattern
# from the html link which gives the key as 7-9 letters from
# the first/last names and a 2 digit number.

player_key <- list_of_players %>% 
  dplyr::mutate(player_key = str_extract(html, "[a-z]*[0-9]{2}"))

stats_to_sql <- dplyr::inner_join(stats_2023, player_key[,c("name", "player_key")], 
                                  by = c("player" = "name"),
                                  relationship = "many-to-many") 

# Separate player list df with just the name and key
nba_players_list <- player_key %>% 
  dplyr::select(name, first, last, player_key, html) %>% 
  dplyr::rename(player_link = html) %>% 
  dplyr::mutate(Season = '2022-23 Season')


con <- dbConnect(RMySQL::MySQL(),
                 dbname = 'nba',
                 host = key_get('Local_Host'),
                 port = 3306,
                 user = 'Austin',
                 password = key_get('local', 'Austin'))

dbWriteTable(con, "player_list", nba_players_list, append = T, row.names = F)
dbWriteTable(con, "player_stats", stats_to_sql, append = T, row.names = F)

dbDisconnnect(con)
