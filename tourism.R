#load library and import documents

library(tidyverse)
country <- readr::read_rds("country_info.rds")
tourism <- readr::read_rds("tourism.rds")


#select columns

options(scipen = 999)['digits']
tourism1 <- tourism %>%
  select("A", "COUNTRY", "Series", "_2014") %>%
  mutate(Country_Name = case_when(A != "NA" ~ paste0(tourism$COUNTRY))) %>% 
  fill(Country_Name, .direction = "down") %>%
  mutate(Tourism_Type = ifelse (grepl("Inbound", COUNTRY), "Inbound Tourism",
                                ifelse(grepl("Outbound", COUNTRY), "Outbound Tourism", NA))) %>%
  fill(Tourism_Type, .direction="down") %>%
  filter(Country_Name != COUNTRY) %>%
  filter(COUNTRY !=  str_to_sentence(Tourism_Type)) 


# convert missing data

tourism2 <- tourism1 %>%
  mutate(Series = str_to_upper(Series, locale="en")) %>%
  mutate(Series = na_if(Series, "")) %>%
  mutate(Series = na_if(Series, "..")) %>%
  rename(Y2014 = `_2014`) %>%
  mutate(Y2014 = as.numeric(na_if(Y2014, ".."))) 

#convert text to numeric

tourism3 <- tourism2 %>%
  mutate(Y2014 = case_when(
    str_detect(COUNTRY, "Mn") ~ as.numeric(paste0(Y2014))*1000000,
    str_detect(COUNTRY, "Thousands") ~ as.numeric(paste0(Y2014))*1000,
    T ~ as.numeric(Y2014)))


tourism4 <- tourism3 %>%
  mutate(Category = case_when(COUNTRY == "Arrivals - Thousands" ~ "Arivals",
                              COUNTRY == "Departures - Thousands" ~ "Departures",
                              COUNTRY == "Travel - US$ Mn" ~ "Travel - US$",
                              COUNTRY == "Passenger transport - US$ Mn" ~ "Passenger transport - US$",
                              COUNTRY == "Tourism expenditure in other countries - US$ Mn" ~ 
                                "Tourism expenditure in other countries - US$",
                              COUNTRY == "Tourism expenditure in the country - US$ Mn" ~ 
                                "Tourism expenditure in the country - US$"))


cleaned_tourism <- tourism4 %>%
  select(Country_Name, Tourism_Type, Category, Series, Y2014)

#continent names

country_info <- country %>%
  mutate(Continent_Name = case_when(Continent == 1 ~ "North America",
                                    Continent == 2 ~ "South America",
                                    Continent == 3 ~ "Europe",
                                    Continent == 4 ~ "Africa",
                                    Continent == 5 ~ "Asia",
                                    Continent == 6 ~ "Oceania",
                                    Continent == 7 ~ "Antarctica"))


# final filter and no country found

final_tourism <-  left_join(cleaned_tourism, country_info, by = c("Country_Name" = "Country"))

NoCountryFound <- anti_join(cleaned_tourism, country_info, by =c("Country_Name" = "Country")) %>%
  select(Country_Name) %>%
  filter(!duplicated(Country_Name))

