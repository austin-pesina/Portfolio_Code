library(rvest)
library(tidyverse)
library(stringi)


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

for(i in 1:length(teams)){
  # Website to scrape
  url <- paste0("http://www.basketball-reference.com/teams/",teams[i],"/2022.html#all_roster") 
  
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
  }
  
  # Column Bind player data and links
  players <- cbind(players, all_links)
  
  # Bind all players into one table
  players_all <- rbind(players_all, players)  
  # Progress bar
  print(paste0("Finished Team: ", teams[i])) 
}


# Split player name into First & Last and update html path for scraping stats
list_of_players <- players_all %>% 
  dplyr::mutate(first = trim((lapply(strsplit(as.character(name), split = " "),"[",1))),
                last = trim((lapply(strsplit(as.character(name), split = " "),"[",2))),
                new_html = trim(gsub(".html", "", html)))
player <- list_of_players$name
list_of_teams <- list_of_players$team

# Create list for html path
path <- list_of_players$new_html





##### Scraping All Stats & Ratings #####

# Empty data frame for stats
stats <- data.frame(Player = character(), ID = character(), Year = character(), G = character(), 
                    Date = character(), Age = character(), Tm = character(), Place = character(), 
                    Opp = character(), Mp = character(), FG = character(), FGA = character(), 
                    'FG%' = character(), '3P' = character(), '3PA' = character(), '3P%' = character(), 
                    FT = character(), FTA = character(), ORB = character(), TRB = character(), 
                    AST = character(), STL = character(), BLK = character(), TOV = character(), 
                    'FT%' = character(), PF = character(), PTS = character(), 
                    GmSc = character(), '+/-' = character())

# Empty data set for NBA 2K Ratings
ratings <- data.frame(Player = character(), Overall = numeric(), "Inside Scoring" = numeric(), "Outside Scoring" = numeric(),
                      Athleticism = numeric(), Playmaking = numeric(), Rebounding = numeric(), 
                      Defending = numeric())

# Empty data sets to fill loops with
present <- data.frame(player=character(), 
                      team=character())
tab <- data.frame(playerid=character(), 
                  team=character())
# Empty data set for PER
per_all <- data.frame(PER = numeric(),
                      player = character(),
                      team = character())

# Create URL of Game Logs
log_url<-paste0("http://www.basketball-reference.com",path,"/","gamelog/2022/")

# Create list of player names for 2K link format
player_2k <- stri_trans_general(paste0(str_replace_all(list_of_players$first, "\\.", ""),"-",list_of_players$last), 'latin-ascii')
url_2k <- paste0("http://www.2kratings.com/",player_2k)


for(i in 1:nrow(players_all)){
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
    
    
    # Combine all stats
    stats <- rbind(stats, games_table)
    
    # Combine stats with players
    tab <- as.data.frame(player[i]) 
    present <- rbind(present,tab)
    
    # Scrape PER from the same URL as above
    games_per <- html_node(html,".stats_pullout")
    # Convert the data to text
    games_table_per <- html_text(games_per)  
    # Use regular expression to pull PER numbers then subset to only show the numbers
    PER <- data.frame(PER = as.numeric(str_sub(str_extract(games_table_per, "PER-?[0-9]+.[0-9]*"), 4)),
                      # Add player
                      player = list_of_players$name[i],
                      # Add team
                      team = list_of_teams[i])
    # Combine all PER
    per_all <- rbind(per_all, PER)
    
    
    # Scrape NBA 2k Ratings 
    html_2k <- read_html(url_2k[i])
    # Data is in the script tag
    games_2k_ <- html_elements(html_2k, 'script') %>% html_text()
    
    # Ratings are in the 15th item in the list
    ind_ratings<- data.frame(Player = player[i],
                             text_data = games_2k_[15]) %>% 
      # Regex to pull data that's stored as '[X, X, X, X, X, X, X] where X is a number
      mutate(numb_data = str_sub(str_extract(text_data, "([\\d,\\s]+)\\]"), end = -2)) %>% 
      # Separate into column
      separate(numb_data, c("Overall", "Inside Scoring", "Outside Scoring", "Athleticism", "Playmaking",
                            "Rebounding", "Defending"), sep = ",")
    
    
    # Combine ratings into a table
    ratings <- rbind(ratings, ind_ratings)
    
    print(paste0("Finished with: ", player[i]))
  }, error=function(e){cat(conditionMessage(e))})
  
}



### Aggregating Stats ###


total_stats <- stats %>% 
  # Remove columns and reorder the remaining
  dplyr::select(-c(Rk, GS, Date, Age, Tm, place, Opp, result, MP, `FG%`, `3P%`, `FT%`, GmSc, `+/-`, team),player, G:PTS) %>% 
  # Convert all columns except player to numeric
  dplyr::mutate(FG = as.numeric(FG),
                FGA = as.numeric(FGA),
                # Convert blank spaces to 0
                `3P` = as.numeric(ifelse(`3P` != '', `3P`, 0)),
                `3PA` = as.numeric(ifelse(`3PA` != '', `3PA`, 0)),
                FT = as.numeric(FT),
                FTA = as.numeric(FTA),
                ORB = as.numeric(ORB),
                DRB = as.numeric(DRB),
                TRB = as.numeric(TRB),
                AST = as.numeric(AST),
                STL = as.numeric(STL),
                BLK = as.numeric(BLK),
                TOV = as.numeric(TOV),
                PF = as.numeric(PF),
                PTS = as.numeric(PTS)) %>% 
  dplyr::group_by(player) %>% 
  # Sum up stats by player
  dplyr::mutate(G = max(G),
                total_FG = sum(FG),
                total_FGA = sum(FGA),
                total_3P = sum(`3P`),
                total_3PA = sum(`3PA`),
                # Calc percentage for three pointers
                `total_3P%` = total_3P/total_3PA,
                total_FT = sum(FT),
                total_FTA = sum(FTA),
                # Calc percentage for free throws
                `total_FT%` = total_FT/total_FTA,
                total_ORB = sum(ORB),
                total_DRB = sum(DRB),
                total_TRB = sum(TRB),
                total_AST = sum(AST),
                total_STL = sum(STL),
                total_BLK = sum(BLK),
                total_TOV = sum(TOV),
                total_PF = sum(PF),
                total_PTS = sum(PTS)) %>% 
  dplyr::ungroup() %>% 
  # Reorder columns and keep only player name, games played, and aggregated stats
  dplyr::select(player, G, total_FG:total_PTS) %>% 
  # Take 1 result per player
  dplyr::distinct(player, .keep_all = T) %>% 
  # Add in PER
  dplyr::left_join(per_all, by = c("player")) %>% 
  dplyr::rename(name = player) 



### Cleaning up Ratings ###

players_ratings <- ratings %>% 
  # Remove text_data column
  dplyr::select(-text_data) %>% 
  # Filter out players missing an overall rating
  dplyr::filter(!is.na(Overall)) %>% 
  # Rename Player column to `name`
  dplyr::rename(name = Player)



##### Salary Data #####

# URL for contract info
salary_url <- paste0("https://www.basketball-reference.com/contracts/players.html")


salaries <- read_html(salary_url) %>% 
  # Read the URL
  rvest::html_nodes("#player-contracts") %>% 
  # Convert to table
  rvest::html_table() %>% 
  .[[1]] %>% 
  # Rename rows to be more descriptive
  dplyr::rename(row = 1,
                name = 2,
                team = 3,
                `22-23 Salary` = 4,
                `23-24 Salary` = 5,
                `24-25 Salary` = 6,
                `25-26 Salary` = 7,
                `26-27 Salary` = 8,
                `27-28 Salary` = 9,
                guaranteed_salary = 10) %>% 
  # Filter out empty player names and repeated column names
  dplyr::filter(name != "" & name != "Player") %>% 
  # Remove row number
  dplyr::select(-row) 


# Remove , & $ from salaries and convert to numeric
salaries$`22-23 Salary` <- as.numeric(stri_replace_all_regex(salaries$`22-23 Salary`, c(",", "\\$") , replacement = c("", ""), vectorize_all = F))
salaries$`23-24 Salary` <- as.numeric(stri_replace_all_regex(salaries$`23-24 Salary`, c(",", "\\$") , replacement = c("", ""), vectorize_all = F))
salaries$`24-25 Salary` <- as.numeric(stri_replace_all_regex(salaries$`24-25 Salary`, c(",", "\\$") , replacement = c("", ""), vectorize_all = F))
salaries$`25-26 Salary` <- as.numeric(stri_replace_all_regex(salaries$`25-26 Salary`, c(",", "\\$") , replacement = c("", ""), vectorize_all = F))
salaries$`26-27 Salary` <- as.numeric(stri_replace_all_regex(salaries$`26-27 Salary`, c(",", "\\$") , replacement = c("", ""), vectorize_all = F))
salaries$`27-28 Salary` <- as.numeric(stri_replace_all_regex(salaries$`27-28 Salary`, c(",", "\\$") , replacement = c("", ""), vectorize_all = F))
salaries$guaranteed_salary <- as.numeric(stri_replace_all_regex(salaries$guaranteed_salary, c(",", "\\$") , replacement = c("", ""), vectorize_all = F))

 
  
  
# Join aggregated stats with NBA 2K ratings and salary data
players_salary <- total_stats %>% 
  dplyr::inner_join(players_ratings, by = "name") %>% 
  dplyr::left_join(salaries, by = c("name", "team")) %>% 
  # Filter out players with no contract data for the 22-23 season
  dplyr::filter(!is.na(`22-23 Salary`)) %>% 
  # Remove duplicates (i.e. player was traded midseason)
  dplyr::distinct(name, .keep_all = T) %>% 
  # Create a score for salary per PER
  dplyr::mutate(PER_score = `22-23 Salary`/PER) %>% 
  # Rearrange columns to have PER & Salary before stats
  dplyr::select(name, team, G, total_FG:PER_score)



##### List of players who made at least $10 Mil and played at least 62 games (3/4 of the season) #####
ten_million <- players_salary %>% 
  # Rename 22-23 Season salary
  dplyr::rename(Salary = `22-23 Salary`) %>% 
  # Filter to only show players making at least $10 Mil and played at least 62 games
  dplyr::filter(Salary >= 10000000 & G >= 62) %>% 
  # Sort by the PER score (lower number is better)
  dplyr::arrange(PER_score)
