#set up the environment

library(tidyverse)  
library(dplyr)
library(data.table)
library(lubridate)

#Import files ranging from 202004 to 202012

filenames <-list.files("C:\\Users\\BinZhang\\Desktop\\Cap1", pattern = "*.csv", full.names = TRUE)
tripdata <- rbindlist(lapply(filenames, fread))


#Data cleaning: remove rows with NA values.
cleaned_tripdata<-tripdata[complete.cases(tripdata[,]),]

#Date cleaning: remove more rows with missing/blank values in certain columns.
cleaned_tripdata<-cleaned_tripdata[!(cleaned_tripdata$start_station_name =="" | cleaned_tripdata$start_station_id =="" | cleaned_tripdata$end_station_name == "" | cleaned_tripdata$end_station_id == "" ),] 


#data cleaning: filter out data with started_at > ended_at
cleaned_tripdata <- cleaned_tripdata %>% 
  filter(started_at < ended_at)

#Add a new column for the duration of each trip in min:
diff_time_min <- difftime(as.POSIXct(cleaned_tripdata$ended_at), as.POSIXct(cleaned_tripdata$started_at), units = "mins")
cleaned_tripdata <- cleaned_tripdata %>% 
  mutate(trip_duration_min = as.integer(diff_time_min))

#Calculate the average time of the service being used by members and casual riders.
cleaned_tripdata %>% 
  group_by(member_casual) %>%
  drop_na() %>% 
  summarize(ave_time = mean(trip_duration_min))

#member_casual ave_dist     
#casual        45.6 mins
#member        15.8 mins

#Let's find out what the top 5 busiest and least busy start stations are. 
cleaned_tripdata %>% 
  count(start_station_name, member_casual, sort = TRUE)

#       start_station_name      member_casual  n
#1:   Streeter Dr & Grand Ave        casual 24329
#2: Lake Shore Dr & Monroe St        casual 18302
#3:           Millennium Park        casual 17589
#4:         Clark St & Elm St        member 17472
#5:       Theater on the Lake        casual 13950
---                                              
#1354:   Stewart Ave & 63rd St (*)      member     1
#1355:   Torrence Ave & 106th St        member     1
#1356:   Torrence Ave & 126th Pl        member     1
#1357:    Western Ave & 104th St        member     1
#1358:    Western Ave & 111th St        member     1 

  
#Let's find out what the top 5 busiest and least busy end stations are.  
cleaned_tripdata %>% 
  count(end_station_name, member_casual, sort = TRUE)

#     end_station_name          member_casual n
#1:   Streeter Dr & Grand Ave        casual 26607
#2:           Millennium Park        casual 18305
#3: Lake Shore Dr & Monroe St        casual 17984
#4:         Clark St & Elm St        member 17928
#5:       Theater on the Lake        casual 15693
#---                                              
#1362:   Stewart Ave & 63rd St (*)    member     1
#1363:   Torrence Ave & 106th St      member     1
#1364:   Torrence Ave & 126th Pl      member     1
#1365:    Western Ave & 104th St      member     1
#1366:    Western Ave & 111th St      member     1


#Let's find out how busy each day is within a week. 
cleaned_tripdata %>% 
  count(weekdays(as.Date(cleaned_tripdata$started_at)), sort = TRUE)

#1:       Saturday    559587
#2:       Sunday      452936
#3:       Friday      442346
#4:       Thursday    402126
#5:       Wednesday   390278
#:        Tuesday     357311
#7:       Monday      347175

#Insert a new column showing on which day each ridership happens.
cleaned_tripdata <- cleaned_tripdata %>% 
  mutate(day_of_week = weekdays(as.Date(started_at)))

#We can see that docked_bike is the most popular choice among both members and casual riders.
cleaned_tripdata %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(num_of_rides = n())

#member_casual  rideable_type     num_of_rides
#1 casual        classic_bike       11260
#2 casual        docked_bike        1092325
#3 casual        electric_bike      145379
#4 member        classic_bike       59143
#5 member        docked_bike        1432356
#6 member        electric_bike      211296


library(ggplot2)

options(scipen = 999)
cleaned_tripdata$day_of_week <- factor(cleaned_tripdata$day_of_week, levels = c("Monday","Tuesday","Wednesday", "Thursday","Friday","Saturday","Sunday"))

#Visualize the popularity of each ride type.

ggplot(cleaned_tripdata) +
  geom_bar(aes(x = cleaned_tripdata$day_of_week, fill = rideable_type)) +
  labs(title = "Share of Ride Type",
       caption = paste0("Data from Google Capstone Project 1"),
        x = "Weekdays", y = "Number of rides")


#Visualize the weekly usage of each ride type for members and casual riders respectively.

ggplot(cleaned_tripdata)+
  geom_bar(aes(x = cleaned_tripdata$day_of_week, fill = rideable_type))+
  facet_wrap(~member_casual)+
  labs(title = "Share of Ride Type",
       caption = paste0("Data from Google Capstone Project 1"),
       x = "Weekdays", y = "Number of rides")

