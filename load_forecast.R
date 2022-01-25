library(odbc)
library(tidyverse)
library(lubridate)
library(data.table)
library(tidytable)
library(olsrr)
library(xgboost)
library(randomForest)


#=========================================================================================
##### SQL Connection and Importing Data #####
#=========================================================================================


# Set up SQL connection to database

con <- DBI::dbConnect(connectiondetails)

# Read in SQL statement

load <- "SELECT datetime,mw FROM table"
weather <- "SELECT * FROM table"

# Create data frame

data_load <- dbGetQuery(con, load)
data_weather <- dbGetQuery(con, weather)

# Disconnect from server
disconnect <- dbDisconnect(con)





##### Cleaning Data and Adding Variables #####

# Join data_load and data_weather

data <- inner_join(data_load, data_weather, by = c("datetime" = "dt"))


# Create load data frame
load <- data
load$pressure <- as.numeric(load$pressure)
load$humidity <- as.numeric(load$humidity)

# Remove duplicates and add date/day/time columns
load <- load[!duplicated(load[c("datetime")]),] 


load <- load %>% 
  dplyr::select(-timezone, -lat, -lon, - sea_level, - grnd_level, -wind_deg, -weather_id, -weather_main, -clouds_all, -weather_icon, -temp_min, -temp_max,
                -index, -dt_iso, -city_name, -weather_description) %>% 
  arrange(datetime) %>% 
  # Add day variables
  mutate(date = date(datetime),
         month = month(as.POSIXlt(datetime, format = "%y/%m/%d")),
         day_of_week = wday(as.POSIXlt(datetime, format = "%y/%m/%d")),
         hour = hour(as.POSIXlt(datetime, format = "%y/%m/%d %h/%m/$s"))) %>% 
  # Add min/max
  group_by(date) %>% 
  mutate(temp_min = min(temp),
         temp_max = max(temp)) %>% 
  ungroup(date) %>% 
  # Add Cooling Degree Day (CDD) & Heating Degree Day (HDD)
  mutate(cdd = ((temp_max + temp_min) * 0.5)-65,
         cdd = ifelse(cdd < 0, 0, cdd),
         hdd = 65-((temp_max + temp_min) * 0.5),
         hdd = ifelse(hdd < 0, 0, hdd)) %>% 
  # Add season
  mutate(season = ifelse(month >= 06 & month <= 09, "summer",
                         ifelse(month <= 11 & month > 08, "fall",
                                ifelse(month <= 02 | month == 12, "winter", "spring")))) %>% 
  # Add day type
  mutate(day_type = ifelse(day_of_week <=4, "weekday", "weekend"))


# Convert NAs to 0

load[is.na(load)]  <- 0



load_final <- load %>%
  dplyr::select(col_name, col_name, col_name, col_name, col_name, col_name, col_name, col_name, col_name, col_name, col_name, col_name)

#=========================================================================================
##### SQL Data For Output #####
#=========================================================================================


# Set up SQL connection to database

con <- DBI::dbConnect(connectiondetails)

# Read in SQL statement

sql_forecast <- "SELECT CONVERT(DATE,dt) AS date, temp_max, temp_min, temp_day, temp_eve, temp_morn, temp_night
                    FROM table WHERE script_run_time =(Select MAX(script_run_time) as Max from table)"
sql_actual <- "SELECT CONVERT(date,DateTime) as date, Max(Air) AS temp_max, Min(Air) AS temp_min, MAX(IIF(DATEPART(hour,DateTime)=6,Air,0)) As temp_morn, MAX(IIF(DATEPART(hour,DateTime)=12,Air,0)) As temp_day, MAX(IIF(DATEPART(hour,DateTime)=18,Air,0)) As temp_eve, MAX(IIF(DATEPART(hour,DateTime)=0,Air,0)) As temp_night
                    FROM table GROUP BY CONVERT(date,DateTime)"



# Create data frame

data_forecast <- dbGetQuery(con, sql_forecast)
data_actual <- dbGetQuery(con, sql_actual)


# Disconnect from server
dbDisconnect(con)



# Degree Day Function

degree_day <- function(x){
  x %>%
    mutate(dd = ifelse((((x$temp_max + x$temp_min)/2) > 65),
                       (((x$temp_max + x$temp_min) * 0.5)-65),
                       65-((x$temp_max + x$temp_min) * 0.5)),
           type = ifelse(((x$temp_max + x$temp_min)/2) > 65, "cooling", "heating"))
  
}


# Add degree day to forecast and actual data frames

forecast <- degree_day(data_forecast) %>%
  arrange(date)
actual <- degree_day(data_actual)


for (i in 1:nrow(forecast)){
  typ <-  forecast[i, 9]
  dd <- forecast[i,8]
  temp_night <- forecast[i, 7]
  temp_morn <- forecast[i, 6]
  temp_eve <- forecast[i, 5]
  temp_day <-  forecast[i, 4]
  temp_min <- forecast[i,3]
  temp_max <- forecast[i, 2]
  
  
  type = typ
  dd_fil = abs(actual$dd - dd)
  
  
  eval(parse(text=paste0("actual$dd_",i,"<-abs(actual$dd - forecast$dd[i])")))
  eval(parse(text=paste0("actual$temp_night_",i,"<-abs(actual$temp_night - forecast$temp_night[i])")))
  eval(parse(text=paste0("actual$morn_",i,"<-abs(actual$dd - forecast$dd[i])")))
  eval(parse(text=paste0("actual$eve_",i,"<-abs(actual$dd - forecast$dd[i])")))
  eval(parse(text=paste0("actual$day_",i,"<-abs(actual$dd - forecast$dd[i])")))
  eval(parse(text=paste0("actual$min_",i,"<-abs(actual$dd - forecast$dd[i])")))
  eval(parse(text=paste0("actual$max_",i,"<-abs(actual$dd - forecast$dd[i])")))
  
}


# Create tables with best fitting values

b3 <- actual %>%
  dplyr::select(1:9,24:30) %>% 
  mutate(best_dd = ifelse(actual$dd_3 == min(actual$dd_3), actual$dd_3,  NA)) %>%
  dplyr::filter(type == forecast$type[3],
                actual$dd_3 == min(actual$dd_3)) %>% 
  mutate(total = sum (temp_night_3 + morn_3 + eve_3 + day_3 + min_3 + max_3)) %>%
  arrange(total)

b4 <- actual %>%
  dplyr::select(1:9,31:37) %>% 
  mutate(best_dd = ifelse(actual$dd_4 == min(actual$dd_4), actual$dd_4,  NA)) %>%
  dplyr::filter(type == forecast$type[4],
                actual$dd_4 == min(actual$dd_4)) %>%
  mutate(total = sum (temp_night_4 + morn_4 + eve_4 + day_4 + min_4 + max_4)) %>%
  arrange(total)

b5 <- actual %>%
  dplyr::select(1:9,38:44) %>% 
  mutate(best_dd = ifelse(actual$dd_5 == min(actual$dd_5), actual$dd_5,  NA)) %>%
  dplyr::filter(type == forecast$type[5],
                actual$dd_5 == min(actual$dd_5)) %>%
  mutate(total = sum (temp_night_5 + morn_5 + eve_5 + day_5 + min_5 + max_5)) %>%
  arrange(total)

b6 <- actual %>%
  dplyr::select(1:9,45:51) %>% 
  mutate(best_dd = ifelse(actual$dd_6 == min(actual$dd_6), actual$dd_6,  NA)) %>%
  dplyr::filter(type == forecast$type[6],
                actual$dd_6 == min(actual$dd_6)) %>%
  mutate(total = sum (temp_night_6 + morn_6 + eve_6 + day_6 + min_6 + max_6)) %>%
  arrange(total)

b7 <- actual %>%
  dplyr::select(1:9,52:58) %>% 
  mutate(best_dd = ifelse(actual$dd_7 == min(actual$dd_7), actual$dd_7,  NA)) %>%
  dplyr::filter(type == forecast$type[7],
                actual$dd_7 == min(actual$dd_7)) %>%
  mutate(total = sum (temp_night_7 + morn_7 + eve_7 + day_7 + min_7 + max_7)) %>%
  arrange(total)

b8 <- actual %>%
  dplyr::select(1:9,59:65) %>% 
  mutate(best_dd = ifelse(actual$dd_8 == min(actual$dd_8), actual$dd_8,  NA)) %>%
  dplyr::filter(type == forecast$type[8],
                actual$dd_8 == min(actual$dd_8)) %>%
  mutate(total = sum (temp_night_8 + morn_8 + eve_8 + day_8 + min_8 + max_8)) %>%
  arrange(total)

b9 <- actual %>%
  dplyr::select(1:9,66:72) %>% 
  mutate(best_dd = ifelse(actual$dd_9 == min(actual$dd_9), actual$dd_9,  NA)) %>%
  dplyr::filter(type == forecast$type[9],
                actual$dd_9 == min(actual$dd_9)) %>%
  mutate(total = sum (temp_night_9 + morn_9 + eve_9 + day_9 + min_9 + max_9)) %>%
  arrange(total)

b10 <- actual %>%
  dplyr::select(1:9,73:79) %>% 
  mutate(best_dd = ifelse(actual$dd_10 == min(actual$dd_10), actual$dd_10,  NA)) %>%
  dplyr::filter(type == forecast$type[10],
                actual$dd_10 == min(actual$dd_10)) %>%
  mutate(total = sum (temp_night_10 + morn_10 + eve_10 + day_10 + min_10 + max_10)) %>%
  arrange(total)

b11 <- actual %>%
  dplyr::select(1:9,80:86) %>% 
  mutate(best_dd = ifelse(actual$dd_11 == min(actual$dd_11), actual$dd_11,  NA)) %>%
  filter(!is.na(best_dd)) %>%
  mutate(total = sum (temp_night_11 + morn_11 + eve_11 + day_11 + min_11 + max_11)) %>%
  arrange(total)

b12 <- actual %>%
  dplyr::select(1:9,87:93) %>% 
  mutate(best_dd = ifelse(actual$dd_12 == min(actual$dd_12), actual$dd_12,  NA)) %>%
  dplyr::filter(type == forecast$type[12],
                actual$dd_12 == min(actual$dd_12)) %>%
  mutate(total = sum (temp_night_12 + morn_12 + eve_12 + day_12 + min_12 + max_12)) %>%
  arrange(total)

b13 <- actual %>%
  dplyr::select(1:9,94:100) %>% 
  mutate(best_dd = ifelse(actual$dd_13 == min(actual$dd_13), actual$dd_13,  NA)) %>%
  dplyr::filter(type == forecast$type[13],
                actual$dd_13 == min(actual$dd_13)) %>%
  mutate(total = sum (temp_night_13 + morn_13 + eve_13 + day_13 + min_13 + max_13)) %>%
  arrange(total)

b14 <- actual %>%
  dplyr::select(1:9,101:107) %>% 
  mutate(best_dd = ifelse(actual$dd_14 == min(actual$dd_14), actual$dd_14,  NA)) %>%
  dplyr::filter(type == forecast$type[14],
                actual$dd_14 == min(actual$dd_14)) %>%
  mutate(total = sum (temp_night_14 + morn_14 + eve_14 + day_14 + min_14 + max_14)) %>%
  arrange(total)

b15 <- actual %>%
  dplyr::select(1:9,108:114) %>% 
  mutate(best_dd = ifelse(actual$dd_15 == min(actual$dd_15), actual$dd_15,  NA)) %>%
  dplyr::filter(type == forecast$type[15],
                actual$dd_15 == min(actual$dd_15)) %>%
  mutate(total = sum (temp_night_15 + morn_15 + eve_15 + day_15 + min_15 + max_15)) %>%
  arrange(total)

b16 <- actual %>%
  dplyr::select(1:9,115:121) %>% 
  dplyr::filter(type == forecast$type[16],
                actual$dd_16 == min(actual$dd_16)) %>%
  mutate(total = sum (temp_night_16 + morn_16 + eve_16 + day_16 + min_16 + max_16)) %>%
  arrange(total)

# Best comparison dates

day_list <- data.frame(dates = as_date(c(NA, NA, b3$date[1], b4$date[1],
                      b5$date[1], b6$date[1], b7$date[1],
                      b8$date[1], b9$date[1], b10$date[1], b11$date[1], b12$date[1], b13$date[1], b14$date[1],
                      b15$date[1], b16$date[1]), origin="1970-01-01"))

# If a day is duplicated, move to the next best day

for (i in 3:nrow(day_list)){
  for (j in 3:(nrow(day_list) - 1)){
  day_list$dates[i] <- if(duplicated(day_list)[i]){ eval(parse(text=paste0("b",i,"$date[2]")))
                               }else if(duplicated(day_list)[j]){
                                 eval(parse(text=paste0("b",i,"$date[3]"))) 
                               }else{}
      }
  }


forecast$comp <- day_list$dates








#=========================================================================================
##### SQL Connection and Importing Data #####
#=========================================================================================


# Set up SQL connection to database

con <- DBI::dbConnect(connectiondetails)

# Read in SQL statement

hourly <-paste0("SELECT DISTINCT CONVERT(date,dt) AS date, dt, temp FROM table WHERE CONVERT(date,dt) IN (convert(date,'", day_list$dates, "'))")

# Create table of data from comparison weather

hourly_actual <- rbind(dbGetQuery(con, hourly[3]), dbGetQuery(con, hourly[4]), dbGetQuery(con, hourly[5]),
  dbGetQuery(con, hourly[6]), dbGetQuery(con, hourly[7]), dbGetQuery(con, hourly[8]), dbGetQuery(con, hourly[9]), dbGetQuery(con, hourly[10]),
  dbGetQuery(con, hourly[11]), dbGetQuery(con, hourly[12]), dbGetQuery(con, hourly[13]), dbGetQuery(con, hourly[14]), dbGetQuery(con, hourly[15]),
  dbGetQuery(con, hourly[16])) 



# Disconnect from server
dbDisconnect(con)


# Creates hourly data for future days

hourly_forecast <- data.frame()


hourly_forecast <- hourly_actual %>%
  mutate(comp = date(dt)) %>% 
  mutate(date = case_when(date == forecast$comp[3] ~ forecast$date[3],
                          date == forecast$comp[4] ~ forecast$date[4],
                          date == forecast$comp[5] ~ forecast$date[5],
                          date == forecast$comp[6] ~ forecast$date[6],
                          date == forecast$comp[7] ~ forecast$date[7],
                          date == forecast$comp[8] ~ forecast$date[8],
                          date == forecast$comp[9] ~ forecast$date[9],
                          date == forecast$comp[10] ~ forecast$date[10],
                          date == forecast$comp[11] ~ forecast$date[11],
                          date == forecast$comp[12] ~ forecast$date[12],
                          date == forecast$comp[13] ~ forecast$date[13],
                          date == forecast$comp[14] ~ forecast$date[14],
                          date == forecast$comp[15] ~ forecast$date[15],
                          date == forecast$comp[16] ~ forecast$date[16]),
         dt = paste0(date, " ", format(as.POSIXct(hourly_actual$dt), format = "%H:%M:%S")),
         comp_dt = paste0(comp, " ",format(as.POSIXct(hourly_actual$dt), format = "%H:%M:%S")))

hourly_forecast$dt <- as.POSIXct(hourly_forecast$dt, format = "%Y-%m-%d %H:%M:%S")
hourly_forecast$comp_dt <- as.POSIXct(hourly_forecast$comp_dt, format = "%Y-%m-%d %H:%M:%S")

hourly_forecast <- hourly_forecast %>%
  mutate(missing_date = date(col_name)) %>%
  dplyr::filter(!is.na(missing_date)) %>%
  dplyr::select(col_name, col_name, col_name, col_name) %>%
  arrange(col_name)







#=========================================================================================
##### SQL Connection and Importing Data #####
#=========================================================================================


# Set up SQL connection to database

con <- DBI::dbConnect(connectiondetails)


sql_weather <- "SELECT * FROM table"
sql_prev <- "SELECT dt, temp, feels_like, temp_min, temp_max, pressure, humidity, wind_speed,rain_1h, rain_3h, snow_1h, snow_3h FROM table where dt >= 'yyyy-mm-dd'"
sql_current <- "SELECT hourbeg AS dt, var_name AS load FROM table"
sql_four_day <- "SELECT script_run_time AS run_time, dt, main_temp, main_pressure, main_humidity, main_feels_like, wind_speed, rain_1h FROM table WHERE script_run_time = (SELECT MAX(script_run_time) FROM table) GROUP BY script_run_time, dt,main_temp, main_pressure, main_humidity, main_feels_like, wind_speed, rain_1h order by  script_run_time desc, dt"

# Load data

past_weather <- dbGetQuery(con, sql_prev)
data_current <- dbGetQuery(con, sql_current)
four_day_forecast <- dbGetQuery(con, sql_four_day) 
  %>% dplyr::select(-run_time)


# Disconnect from server
dbDisconnect(con)





# Build data set to pull comparison weather

comp <- past_weather %>%
  mutate(date = date(dt))



# Combine comparison dates (str(comp)) with forecast (str(hourly_forecast), no hours in dt)

comp_weather <- left_join(hourly_forecast, comp, by = c("col_name" = "col_name"))
comp_weather$dt <- as.POSIXct(comp_weather$dt, format = "%Y-%m-%d %H:%M:%S")
comp_weather <- comp_weather %>% 
  dplyr::rename(temp = temp.y) %>% 
  dplyr::select(col_name, col_name, col_name, col_name, col_name, col_name, col_name) %>% 
  dplyr::filter(!duplicated(col_name))



new_hourly <- anti_join(four_day_forecast, comp_weather, by = "col_name") %>% 
  dplyr::rename(temp = main_temp,
         feels_like = main_feels_like,
         pressure = main_pressure,
         humidity = main_humidity)


weather_forecasted <- full_join(comp_weather, new_hourly, by = c("col_name", "col_name", "col_name", "col_name", "col_name", "col_name", "col_name")) %>% 
  arrange(col_name) %>% 
  dplyr::filter(!duplicated(col_name))

# weather_full contains full weather with comparable dates used to sub for weather conditions for future dates

weather_full <- full_join(comp, weather_forecasted, by = "dt") %>%
  mutate(temp = ifelse(is.na(temp.x), temp.y, temp.x),
         feels_like = ifelse(is.na(feels_like.x), feels_like.y, feels_like.x),
         pressure = ifelse(is.na(pressure.x), pressure.y, pressure.x),
         humidity = ifelse(is.na(humidity.x), humidity.y, humidity.x),
         wind_speed = ifelse(is.na(wind_speed.x), wind_speed.y, wind_speed.x),
         rain_1h = ifelse(is.na(rain_1h.x), rain_1h.y, rain_1h.x),
         date = date(dt)) %>%
  dplyr::select(col_name, col_name, col_name, col_name, col_name, col_name, col_name, col_name)


##### Recreate Data Set For Future Weather #####

load <- load %>% # uses load df from earlier in script
  dplyr::select(col_name, col_name, col_name, col_name, col_name, col_name, col_name, col_name, col_name)


weather1 <- weather_full %>%
  rename(datetime = dt) %>%
  # Add blank columns for next join
  mutate(mw = NA)



# Join weather data with mw data until date

weather1 <- full_join(load, weather1, by = "col_name") %>% 
  mutate(mw = ifelse(is.na(mw.x), mw.y, mw.x),
         date = ifelse(is.na(date.x), date.y, date.x),
         temp = ifelse(is.na(temp.x), temp.y, temp.x),
         feels_like = ifelse(is.na(feels_like.x), feels_like.y, feels_like.x),
         pressure = ifelse(is.na(pressure.x), pressure.y, pressure.x),
         humidity = ifelse(is.na(humidity.x), humidity.y, humidity.x),
         wind_speed = ifelse(is.na(wind_speed.x), wind_speed.y, wind_speed.x),
         rain_1h = ifelse(is.na(rain_1h.x), rain_1h.y, rain_1h.x)) %>% 
  dplyr::select(col_name, col_name, col_name, col_name, col_name, col_name,
                col_name, col_name, col_name)




# Add mw data after date from dbname

weather1 <- left_join(weather1, data_current, by = c("col_name" = "col_name"))
weather1$mw <- ifelse(is.na(weather1$mw), weather1$load, weather1$mw)


weather_final <- weather1 %>%
  dplyr::filter(!duplicated(datetime)) %>% 
  dplyr::arrange(datetime) %>% 
  dplyr::select(-col_name) %>%
  mutate(hour = hour(datetime)) %>% 
  # Add day variables
  mutate(date = date(datetime),
         month = month(as.POSIXlt(datetime, format = "%y/%m/%d")),
         day_of_week = wday(as.POSIXlt(datetime, format = "%y/%m/%d")),
         hour = hour(as.POSIXlt(datetime, format = "%y/%m/%d %h/%m/$s"))) %>% 
  # Add min/max
  group_by(date) %>% 
  mutate(temp_min = min(temp),
         temp_max = max(temp)) %>% 
  ungroup(date) %>% 
  # Add Cooling Degree Day (CDD) & Heating Degree Day (HDD)
  mutate(cdd = ((temp_max + temp_min) * 0.5)-65,
         cdd = ifelse(cdd < 0, 0, cdd),
         hdd = 65-((temp_max + temp_min) * 0.5),
         hdd = ifelse(hdd < 0, 0, hdd)) %>% 
  # Add season
  mutate(season = ifelse(month >= 06 & month <= 09, "summer",
                         ifelse(month <= 11 & month > 08, "fall",
                                ifelse(month <= 02 | month == 12, "winter", "spring")))) %>% 
  # Add day type
  mutate(day_type = ifelse(day_of_week <=4, "weekday", "weekend")) %>% 
  # Add Cooling Degree Day (CDD) & Heating Degree Day (HDD)
  mutate(cdd = ((temp_max + temp_min) * 0.5)-65,
         cdd = ifelse(cdd < 0, 0, cdd),
         hdd = 65-((temp_max + temp_min) * 0.5),
         hdd = ifelse(hdd < 0, 0, hdd)) %>%
  # Add peak labels
  mutate(peak = ifelse (hour >= 7 & hour <= 23, "peak", "off-peak")) %>% 
  # Add dummy variables
  get_dummies.(c(col_name, col_name)) %>% 
  # Add lag
  mutate(col_name = lag(col_name, 1),
         col_name = lag(col_name, 24),
         col_name = lag(col_name, 168))







# Convert NAs to 0

weather_final[is.na(weather_final)]  <- 0

weather_final <- weather_final %>% 
  dplyr::select(-c(col_name, col_name, col_name))


weather_final$day_type <- factor(weather_final$day_type, levels = c("weekday", "weekend"), labels = c(0,1))
weather_final$peak <- factor(weather_final$peak, levels = c("peak", "off-peak"), labels = c(0,1))

# Final data sets to be used in models 

final_data <- weather_final %>%
  dplyr::filter(datetime > "yyyy-mm-dd hh:mm:ss") %>%
  dplyr::select(-c(col_name, col_name, col_name, col_name, col_name, col_name, col_name, col_name,  col_name))

final_data_dt <- weather_final %>%
  dplyr::filter(datetime > "yyyy-mm-dd hh:mm:ss") %>%
  dplyr::select(-c(col_name, col_name, col_name, col_name, col_name, col_name, col_name, col_name,  col_name))


final_data$col_name <- ifelse(final_data$col_name == 0, NA, final_data$col_name)
final_data$col_name <- ifelse(final_data$col_name == 0, NA, final_data$col_name)
final_data_dt$col_name <- ifelse(final_data_dt$col_name == 0, NA, final_data_dt$col_name)
final_data_dt$col_name <- ifelse(final_data_dt$col_name == 0, NA, final_data_dt$col_name)




test_data <- final_data %>% filter(is.na(col_name)) %>% dplyr::select(-col_name)
test_data_dt <- final_data_dt %>% filter(is.na(col_name)) %>%  dplyr::select(-col_name)



training <- final_data %>% filter(!is.na(col_name))
testing <- final_data %>% filter(is.na(col_name))



#=========================================================================================
              ##### Model Building ######
#=========================================================================================





### XGB Model


mw_xgb_2 <- testing


train_x_set <- data.matrix(training[,-1])
train_y_set <- data.matrix(training[,1])

test_x_set_2 <- data.matrix(mw_xgb_2[1,-1])
test_y_set_2 <- data.matrix(0)



xgb_train_set_2 <- xgb.DMatrix(data = train_x_set, label = train_y_set)
xgb_test_set_2 <- xgb.DMatrix(data = test_x_set_2, label = test_y_set_2)


xgb_2 <- xgboost(data = xgb_train_set_2,
                 nrounds = x,
                 max_depth = x,
                 objective = "reg:squarederror",
                 early_stopping_rounds = x,
                 eta = x)





for (i in 1:nrow(mw_xgb_2)){
  if(i < 24){
    second_xgb <- predict(xgb_2, xgb_test_set_2)
    mw_xgb_2$col_name[i] <- second_xgb[i]
    mw_xgb_2$col_name[i+1] <- mw_xgb_2$col_name[i] 
    mw_xgb_2$col_name[i+24] <- mw_xgb_2$col_name[i]
    mw_xgb_2$col_name[i+168] <- mw_xgb_2$col_name[i]
    test_x_set_2 <- data.matrix(mw_xgb_2[,-1])
    test_y_set_2 <- data.matrix(mw_xgb_2[,1])
    test_y_set_2[is.na(mw_xgb_2)] <- 0
    test_y_set_2 <- test_y_set_2[1:nrow(test_x_set_2)]
    xgb_test_set_2 <- xgb.DMatrix(data = test_x_set_2, label = test_y_set_2)
  }  else if (i == 24)  {
    second_xgb <- predict(xgb_2, xgb_test_set_2)
    mw_xgb_2$col_name[i] <- second_xgb
    mw_xgb_2$col_name[i+1] <- mw_xgb_2$col_name[i]
    mw_xgb_2$col_name[i+1] <- mw_xgb_2$col_name[i-23]
    test_x_set_2 <- data.matrix(mw_xgb_2[i,-1])
    test_y_set_2 <- data.matrix(mw_xgb_2[,1])
    test_y_set_2[is.na(mw_xgb_2)] <- 0
    test_y_set_2 <- test_y_set_2[1:nrow(test_x_set_2)]
    xgb_test_set_2 <- xgb.DMatrix(data = test_x_set_2, label = test_y_set_2)
  }  else if (i >= 25 & i < 168)  {
    second_xgb <- predict(xgb_2, xgb_test_set_2)
    mw_xgb_2$mcol_namew[i] <- second_xgb
    mw_xgb_2$col_name[i+1] <- mw_xgb_2$col_name[i]
    mw_xgb_2$col_name[i+1] <- mw_xgb_2$col_name[i-23]
    test_x_set_2 <- data.matrix(mw_xgb_2[i,-1])
    test_y_set_2 <- data.matrix(mw_xgb_2[i,1])
    test_y_set_2[is.na(mw_xgb_2)] <- 0
    test_y_set_2 <- test_y_set_2[1:nrow(test_x_set_2)]
    xgb_test_set_2 <- xgb.DMatrix(data = test_x_set_2, label = test_y_set_2)
  }  else if (i >= 168 & i < nrow(mw_xgb_2))  {
    second_xgb <- predict(xgb_2, xgb_test_set_2)
    mw_xgb_2$col_name[i] <- second_xgb
    mw_xgb_2$col_name[i+1] <- mw_xgb_2$col_name[i]
    mw_xgb_2$col_name[i+1] <- mw_xgb_2$col_name[i-23]
    mw_xgb_2$col_name[i+1] <- mw_xgb_2$col_name[i-167]
    test_x_set_2 <- data.matrix(mw_xgb_2[i,-1])
    test_y_set_2 <- data.matrix(mw_xgb_2[i,1])
    test_y_set_2[is.na(mw_xgb_2)] <- 0
    test_y_set_2 <- test_y_set_2[1:nrow(test_x_set_2)]
    xgb_test_set_2 <- xgb.DMatrix(data = test_x_set_2, label = test_y_set_2)
  } else {
    second_xgb <- predict(xgb_2, xgb_test_set_2)
    mw_xgb_2$col_name[i] <- second_xgb
    mw_xgb_2$col_name[i] <- mw_xgb_2$col_name[i-2]
    mw_xgb_2$col_name[i] <- mw_xgb_2$col_name[i-23]
    mw_xgb_2$col_name[i] <- mw_xgb_2$mcol_namew[i-167]
  }
}







### Random Forest

rf <- randomForest(mw ~ . , data = training, mtry = x, ntree = x, importance = T)

mw_rf <- testing




for (i in 1:nrow(testing)){
  if(i < 24){
    first_rf <- predict(rf, mw_rf)
    mw_rf$col_name[i] <- first_rf[i]
    mw_rf$col_name[i+1] <- mw_rf$col_name[i] 
    mw_rf$col_name[i+24] <- mw_rf$col_name[i]
    mw_rf$col_name[i+168] <- mw_rf$col_name[i]
  }  else if (i == 24)  {
    first_rf <- predict(rf, mw_rf)
    mw_rf$col_name[i] <- first_rf[i]
    mw_rf$col_name[i+1] <- mw_rf$col_name[i]
    mw_rf$col_name[i+1] <- mw_rf$col_name[i-23]
  }  else if (i > 24 & i < 168)  {
    first_rf <- predict(rf, mw_rf)
    mw_rf$col_name[i] <- first_rf[i]
    mw_rf$col_name[i+1] <- mw_rf$col_name[i]
    mw_rf$col_name[i+1] <- mw_rf$col_name[i-23]
  }  else if (i >= 168 & i < nrow(mw_rf))  {
    first_rf <- predict(rf, mw_rf)
    mw_rf$col_name[i] <- first_rf[i]
    mw_rf$col_name[i+1] <- mw_rf$col_name[i]
    mw_rf$col_name[i+1] <- mw_rf$mcol_namew[i-23]
    mw_rf$col_name[i+1] <- mw_rf$col_name[i-167]
  } else {
    first_rf <- predict(rf, mw_rf)
    mw_rf$col_name[i] <- first_rf[i]
    mw_rf$col_name[i] <- mw_rf$col_name[i-2]
    mw_rf$col_name[i] <- mw_rf$col_name[i-23]
    mw_rf$col_name[i] <- mw_rf$col_name[i-167]
  }
}



### Random Forrest 2


rf_2 <- randomForest(mw ~ . , data = training, mtry = x, ntree = x, importance = T)

mw_rf_2 <- testing




for (i in 1:nrow(testing)){
  if(i < 24){
    first_rf_2 <- predict(rf_2, mw_rf_2)
    mw_rf_2$col_name[i] <- first_rf_2[i]
    mw_rf_2$col_name[i+1] <- mw_rf_2$col_name[i] 
    mw_rf_2$col_name[i+24] <- mw_rf_2$col_name[i]
    mw_rf_2$col_name[i+168] <- mw_rf_2$col_name[i]
  }  else if (i == 24)  {
    first_rf_2 <- predict(rf_2, mw_rf_2)
    mw_rf_2$col_name[i] <- first_rf_2[i]
    mw_rf_2$col_name[i+1] <- mw_rf_2$col_name[i]
    mw_rf_2$col_name[i+1] <- mw_rf_2$col_name[i-23]
  }  else if (i > 24 & i < 168)  {
    first_rf_2 <- predict(rf_2, mw_rf_2)
    mw_rf_2$col_name[i] <- first_rf_2[i]
    mw_rf_2$col_name[i+1] <- mw_rf_2$col_name[i]
    mw_rf_2$col_name[i+1] <- mw_rf_2$col_name[i-23]
  }  else if (i >= 168 & i < nrow(mw_rf_2))  {
    first_rf_2 <- predict(rf_2, mw_rf_2)
    mw_rf_2$col_name[i] <- first_rf_2[i]
    mw_rf_2$col_name[i+1] <- mw_rf_2$col_name[i]
    mw_rf_2$col_name[i+1] <- mw_rf_2$col_name[i-23]
    mw_rf_2$col_name[i+1] <- mw_rf_2$col_name[i-167]
  } else {
    first_rf_2 <- predict(rf_2, mw_rf_2)
    mw_rf_2$col_name[i] <- first_rf_2[i]
    mw_rf_2$col_name[i] <- mw_rf_2$col_name[i-2]
    mw_rf_2$col_name[i] <- mw_rf_2$col_name[i-23]
    mw_rf_2$col_name[i] <- mw_rf_2$col_name[i-167]
  }
}







#=========================================================================================
                  ##### Build Final Table & Write to SQL ######
#=========================================================================================

forecast_pred <- data.frame(datetime = test_data_dt$datetime,
                            #forecast_xgb = mw_xgb$mw,
                            forecast_rf = mw_rf$mw,
                            forecast_xgb_2 = mw_xgb_2$mw,
                            forecast_rf_2 = mw_rf_2$mw)
  forecast_pred <- forecast_pred %>% 
  mutate(forecast = x,
         timestamp = now(tz = "America/Chicago"))
  




con <- DBI::dbConnect(connectiondetails)


dbAppendTable(conn = con,
              name = "tablename",
              value = forecast_pred)


next_day_forecast <- "SELECT DateTime, convert(date,DateTime) as Date, Datepart(hour,DateTime)+1 as HE, forecast_rf AS RF, forecast_rf_2 AS RF_2, forecast_xgb_2 AS XGB_2, forecast as MW from table where timestamp = (Select MAX(timestamp) as max from table) and  DateTime >= convert(varchar, GETDATE() +1, 101)  ORDER BY timestamp, DateTime"

next_day <- dbGetQuery(con, next_day_forecast)


colors <- c("MW" = "red", "RF" = "green", "XGB_2" = "orange", "RF_2" = "blue")
ggplot(data = next_day, mapping = aes(x = DateTime)) +
  geom_line(mapping = aes(y = MW, color = "MW")) +
  geom_line(mapping = aes(y = XGB_2, color = "XGB_2")) +
  geom_line(mapping = aes(y = RF, color = "RF")) +
  geom_line(mapping = aes(y = RF_2, color = "RF_2")) +
  scale_x_datetime(breaks = "day") +
  labs(x = "datetime",
       y = "mw",
       color = "Legend")
  

dbDisconnect(con)