######################################################################
# This script crawls through every MLB team's roster page on 
# Baseball-Reference.com and scrapes their 40-man roster. The
# output is then written to a local MySQL instance.
######################################################################

library(rvest)
library(dplyr)
library(DBI)
library(odbc)
library(stringi)
library(RMySQL)


# Create a trim function which uses regular expressions to clean white space
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

# List of all teams. Note: Some pages use different team abbreviations.
# For example, the 40 man roster uses the current iteration. The franchise
# summary page uses ANA for the Angels instead of their current LAA.
teams <- c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", "HOU", "KCR", "LAA", "LAD", "MIA",
           "MIL", "MIN", "NYM", "NYY", "OAK", "PHI", "PIT", "SDP", "SFG", "SEA", "STL", "TBR", "TEX", "TOR", "WSN")


##### List of all Players #####

# Empty data set for for-loop
players_all <- data.frame(name = character(), 
                          team = character())  

for(i in 1:length(teams)){
  # Website to scrape
  url <- paste0("http://www.baseball-reference.com/teams/",teams[i],"/2023-roster.shtml")
  
  
  
  # Read in the URL, rename the Player column and add the team name
  players <- rvest::read_html(url) %>% 
    rvest::html_nodes("#the40man") %>% 
    rvest::html_table() %>% 
    .[[1]] %>% 
    dplyr::select(Name) %>% 
    dplyr::rename(name = Name) %>% 
    dplyr::mutate(team = teams[i])
  
  Sys.sleep(4)
  
  
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
    
    Sys.sleep(4)
  }
  
  # Column Bind player data and links
  players <- cbind(players, all_links)
  
  # Bind all players into one table
  players_all <- rbind(players_all, players)  
  # Progress bar
  print(paste0("Finished Team: ", teams[i])) 
}


# Split player name into First & Last and update html path for scraping stats
players_list <- players_all %>% 
  dplyr::mutate(first = trim((lapply(strsplit(as.character(name), split = " "),"[",1))),
                last = trim((lapply(strsplit(as.character(name), split = " "),"[",2))),
                new_html = trim(gsub(".html", "", html)),
                player_key = str_extract(html, "[a-z]*[0-9]{2}")) %>% 
  dplyr::filter(!is.na(last),
                nchar(player_key) > 3)

players_to_sql <- players_list[,-c(3,6)] %>% 
  dplyr::mutate(year = 2023)

player <- players_list$name
list_of_teams <- players_list$team

# Create list for html path
path <- players_list$html

# Export variables to be used in other scripts
save(teams, file = "data/teams.RData")
save(path, file = "data/player_key.RData")
save(players_list, file = "data/players_list.RData")

# Write player list to sql

con <- dbConnect(RMySQL::MySQL(),
                 dbname = 'mlb',
                 host = key_get('Local_Host'),
                 port = 3306,
                 user = 'Austin',
                 password = key_get('local', 'Austin'))

dbWriteTable(con, "player_list", players_to_sql, append = T, row.names = F)

dbDisconnect(con)


