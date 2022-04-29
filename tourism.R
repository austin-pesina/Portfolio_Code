library(tidyverse)

# Read in data. Data is in .rds file so we use the read_rds function from
country <- readr::read_rds("data/country_info.rds")
tourism <- readr::read_rds("data/tourism.rds")

# Create tourism_clean table
tourism_clean <- tourism %>%
  # Start by removing all columns from 1995-2013
  dplyr::select(-c(`_1995`:`_2013`))%>% 
  # Create country_name column by using the value when column "A" is not NA
  dplyr::mutate(country_name = case_when(A != "NA" ~ tourism$COUNTRY),
                # If COUNTRY column contains "inbound" then tourism_type will be "Inboud Tourism"
                # but if COUNTRY contains "Outbound" then it will be "Outbound Tourism"
                tourism_type = case_when(grepl("Inbound", COUNTRY) ~ "Inbound Tourism",
                                         grepl("Outbound", COUNTRY) ~ "Outbound Tourism")) %>% 
  # Fills the values in country_name and tourism_type until another value is present
  tidyr::fill(c(country_name, tourism_type), .direction = "down") %>% 
  # Filters out rows where country_name does not match COUNTRY and COUNTRY does not have
  # a match in tourism_type
  dplyr::filter(country_name != COUNTRY,
                COUNTRY != str_to_sentence(tourism_type)) %>% 
  # Series is replaced by all uppercase values and ".." and blank spaces are converted
  # to NA. `_2014` has the same values converted to NA.
  dplyr::mutate(Series = stringr::str_to_upper(Series),
                Series = dplyr::na_if(Series, ".."),
                Series = dplyr::na_if(Series, ""),
                `_2014` = dplyr::na_if(`_2014`, ".."),
                `_2014` = dplyr::na_if(`_2014`, ""),
                # Y2014 is the value in `_2014` multiplied by either 1,000 or 1,000,000 depending
                # on what value is in COUNTRY
                Y2014 = dplyr::case_when(grepl("MN", COUNTRY) ~ as.numeric(`_2014`)*1000000,
                                         grepl("Thousands", COUNTRY) ~ as.numeric(`_2014`)*1000),
                # The term in the grepl function is searched for in COUNTRY and the new value is
                # added to a new variable called `category`
                category = dplyr::case_when(grepl("Arrivals - Thousands", COUNTRY) ~ "Arrivals",
                                            grepl("Departure - Thousands", COUNTRY) ~ "Departures",
                                            grepl("Passenger transport", COUNTRY) ~  "Passenger Transport - US$",
                                            grepl("other countries", COUNTRY) ~ "Tourism expenditure in other countries - US$",
                                            grepl("the country", COUNTRY) ~ "Tourism expenditure in the country - US$",
                                            grepl("travel - US", COUNTRY) ~ "Travel - US$")) %>% 
  # We select only the following columns
  dplyr::select(country_name, tourism_type, category, Series, Y2014)


# A new df, `continent`, is created from the `country` table
continent <- country %>% 
  # The variable `continent` is given names based on the number associated with it
  mutate(contient = case_when(Continent == 1 ~ "North America",
                              Continent == 2 ~ "South America",
                              Continent == 3 ~ "Europe",
                              Continent == 4 ~ "Africa",
                              Continent == 5 ~ "Asia",
                              Continent == 6 ~ "Oceania",
                              Continent == 7 ~ "Antarctica"))

# final_cleaned_tourism is created by combining tourism_clean and continent into a single df
# and they are joined by "country_name" on tourism_clean and "Country" on continent
final_cleaned_tourism <- dplyr::left_join(tourism_clean, continent, by = c("country_name" = "Country"))


# no_country_found is created by taking the final_cleaned_tourism table
no_country_found <- final_cleaned_tourism %>% 
  # anti_join take rows that are not in final_cleaned_tourism no continent and uses the same
  # join logic as before
  dplyr::anti_join(continent, by = c("country_name" = "Country")) %>% 
  # we select only the country_name variable
  dplyr::select(country_name) %>% 
  # and then find only the distinct values
  dplyr::distinct(country_name)
                                    