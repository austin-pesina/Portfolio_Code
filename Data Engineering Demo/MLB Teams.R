######################################################################
# This script crawls and grabs the attendance numbers of each MLB
# team as well as their record, payroll, attendance, and stadium.
# The output is then written to a local MySQL instance.
######################################################################


library(rvest)
library(dplyr)
library(DBI)
library(odbc)
library(stringr)
library(RMySQL)


# List of all teams using the abbreviation used on the attendance page.
# Notable changes are LAA to ANA and TBR to TBD.
teams <- c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", "HOU", "KCR", "ANA", "LAD", "FLA",
           "MIL", "MIN", "NYM", "NYY", "OAK", "PHI", "PIT", "SDP", "SFG", "SEA", "STL", "TBD", "TEX", "TOR", "WSN")




# Empty data set for for-loop
teams_all <- data.frame(name = character(), 
                        team = character())  

# This loops through each team abbreviation and pulls the variables 
# listed in the description above. `Lg` is broken out into
# league and division and `Stadium` only pulls the first  stadium
# listed. This is where most games were played and excludes
# stadiums were international or promotional games were played.

for(i in 1:length(teams)){
  
  # Website to scrape
  url <- paste0("http://www.baseball-reference.com/teams/",teams[i],"/attend.shtml")
  
  
  # Read in the URL, rename the Player column and add the team name
  team_status <- rvest::read_html(url) %>% 
    rvest::html_nodes("#franchise_years") %>% 
    rvest::html_table() %>% 
    .[[1]] %>% 
    dplyr::select(-c(`Attend/G`, Rank, PPF, BPF)) %>% 
    dplyr::rename_with(tolower) %>%
    dplyr::rename(team = tm,
                  payroll = `est. payroll`,
                  win = w,
                  loss = l) %>% 
    dplyr::mutate(attendance = as.numeric(stringr::str_replace_all(attendance, ",", "")),
                  payroll = as.numeric(stringr::str_replace_all(payroll, c("\\$" = "", "," = ""))),
                  # Extracts the first entry up to a comma
                  stadium = stringr::str_extract(stadium, "^([^,]+)"),
                  # Changes any playoff result that's blank to "Missed Playoffs"
                  playoffs = stringr::str_replace_all(playoffs, "^$", "Missed Playoffs"),
                  league = stringr::str_sub(lg, 1, 2),
                  division = stringr::str_sub(lg, 4)) %>% 
    dplyr::select(-lg)
  
  Sys.sleep(4)
  
  teams_all <- rbind(teams_all, team_status)
  
  
  # Progress bar
  print(paste0("Finished Team: ", teams[i])) 
}





# Write player list to sql

con <- dbConnect(RMySQL::MySQL(),
                 dbname = 'mlb',
                 host = key_get('Local_Host'),
                 port = 3306,
                 user = 'Austin',
                 password = key_get('local', 'Austin'))

dbWriteTable(con, "team_records", teams_all, append = T, row.names = F)

dbDisconnect(con)
