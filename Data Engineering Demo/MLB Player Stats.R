################################################################################################
# This scripts scrapes batting and pitching stats for MLB players from the 
# `Player List.R` script. The main function loops through each web page in 
# the relevant stat url list. To prevent getting IP blocked, there is a 5 
# second sleep before each player and a 10 second sleep after each player. 
# Not every player is listed so the loop is put into a try/except block. 
# Repeated headers are filtered out as well as minor league season stats.
################################################################################################

library(rvest)
library(dplyr)
library(rvest)
library(DBI)
library(odbc)
library(stringi)
library(RMySQL)
library(keyring)


load("data/teams.RData")
load("data/player_key.RData")
load("data/players_list.RData")


stats_scrape <- function(type){
  
  # Empty data frame for stats
  final_df <- data.frame()
  
  # Create URL of every player (path var comes from full player list scrape)
  stats_url <- paste0("http://www.baseball-reference.com",path,"#all_",type,"_standard")
  
  
  # This loop goes through each web page in the pitching stats url list. To prevent
  # getting IP blocked, there is a 5 second sleep before each player and a 10 second
  # sleep after each player. Not every player is listed so the loop is put into a
  # try/except block. Repeated headers are filtered out as well as minor league
  # season stats.
  for(i in 1:nrow(players_list)){
    tryCatch({
      Sys.sleep(5)
      # Scrape data from game logs
      stats_df <- rvest::read_html(stats_url[i]) %>% 
        rvest::html_element(css = paste0("#",type,"_standard")) %>% 
                              rvest::html_table(fill = T) %>% 
                              dplyr::filter(nchar(Year) == 4,
                                            Lg %in% c("MLB", "AL", "NL")) %>% 
                              dplyr::mutate(path = path[i]) 
                            
                            Sys.sleep(7)
                            
                            
                            # Combine all stats
                            final_df <- rbind(final_df, stats_df)
                            
                            
                            
                            print(paste0("Finished with: ", players_list$name[i]))
    }, error=function(e){cat(conditionMessage(e))})
      
  }
  
  return(final_df)
  
}


pitcher_stats <- stats_scrape("pitching") %>% 
  dplyr::mutate_at(c(1,2,6:35), as.numeric) %>% 
  dplyr::inner_join(players_list[,c("name", "html", "player_key")],
                    by = c("path" = "html")) %>% 
  dplyr::select(-Awards) 

batter_stats <- stats_scrape("batting") %>% 
  dplyr::mutate_at(c(1,2,6:28), as.numeric) %>% 
  dplyr::inner_join(players_list[,c("name", "html", "player_key")],
                    by = c("path" = "html")) %>% 
  dplyr::select(-Awards, -Pos)



# Write player stats to sql

con <- dbConnect(RMySQL::MySQL(),
                 dbname = 'mlb',
                 host = key_get('Local_Host'),
                 port = 3306,
                 user = 'Austin',
                 password = key_get('local', 'Austin'))

dbWriteTable(con, "pitcher_stats", pitcher_stats, append = T, row.names = F)
dbWriteTable(con, "batter_stats", batter_stats, append = T, row.names = F)

dbDisconnect(con)


