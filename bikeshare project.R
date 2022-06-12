#loading libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
getwd()
#setting the working directory
setwd("/Users/user/Documents/BIKESHARE")
#loading data
d2022_03<-read_csv("202203-divvy-tripdata.csv")
d2022_02<-read_csv("202202-divvy-tripdata.csv")
d2022_01<-read_csv("202201-divvy-tripdata.csv")
d2021_12<-read_csv("202112-divvy-tripdata.csv")
d2021_11<-read_csv("202111-divvy-tripdata.csv")
d2021_10<-read_csv("202110-divvy-tripdata.csv")
d2021_09<-read_csv("202109-divvy-tripdata.csv")
d2021_08<-read_csv("202108-divvy-tripdata.csv")
d2021_07<-read_csv("202107-divvy-tripdata.csv")
d2021_06<-read_csv("202106-divvy-tripdata.csv")
d2021_05<-read_csv("202105-divvy-tripdata.csv")
#i have physically compared the colnames and verified that they are the same
#i also used view() to open the data in other tabs in the script to also take a look at them one by one
#joining all the rows
trip_data<- bind_rows(d2022_03,d2022_02,d2022_01,d2021_12,d2021_11,d2021_10,d2021_09,d2021_08,d2021_07,d2021_06,d2021_05)
trip_data
view(trip_data)
#the combined data has 5,386,302 entries, 13 total columns
#checking the unique values in the member type column
unique(trip_data$member_casual)
#creating separate columns for the date, year and day and also the ride length whch is a difference of the time in started_at and ended_at
trip_data$date<-as.Date(trip_data$started_at)
trip_data$day<-format(as.Date(trip_data$date), "%d")
trip_data$month<-format(as.Date(trip_data$date), "%m")
trip_data$year<-format(as.Date(trip_data$date), "%y")
trip_data$day_of_week <- format(as.Date(trip_data$date), "%A")
trip_data$ride_length <- difftime(trip_data$ended_at,trip_data$started_at)
#inspecting the structure of the columns again
str(trip_data)
#converting the ride length to numeric so as to allow for calculations
trip_data$ride_length<-as.numeric(trip_data$ride_length)
#creating a copy table to remove the trips that were just maintenance (they were taken from the hq)
trip_data_v2 <- trip_data[!(trip_data$start_station_name == "HQ QR" | trip_data$ride_length<=0),]
#trip_data_v2 has 5,385,681 entries, 19 total columns. 
#summary stat for the ride length.
summary(trip_data_v2$ride_length)
#min ride is 1s, max is 3356649s, mean is 1339s, median = 712s
#getting the number of trips for each start station so as to know the most and least used
#i had to set the print limit to be 999999 so the console can print more than 500 results (i didnt knw how to sort it by ascending order)
vec_count(trip_data_v2$start_station_name)
#most used station is Streeter Dr & Grand Ave  with 80637 trips
#least used stations are Lyft Driver Center Private Rack, Whipple St & Irving Park Rd,  Pawel Bialowas - Test- PBSC charging station and Troy Ave & 27th St with 1 trip each
#comparing the ride length stats in terms of member type
aggregate(trip_data_v2$ride_length ~trip_data_v2$member_casual, FUN = mean)
aggregate(trip_data_v2$ride_length ~trip_data_v2$member_casual, FUN = median)
aggregate(trip_data_v2$ride_length ~trip_data_v2$member_casual, FUN = min)
aggregate(trip_data_v2$ride_length ~trip_data_v2$member_casual, FUN = max)
#average ride time by each day in terms of member type
aggregate(trip_data_v2$ride_length ~ trip_data_v2$member_casual + trip_data_v2$day_of_week, FUN = mean)
# re-ordering the weekdays
trip_data_v2$day_of_week <- ordered(trip_data_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
#running the aggregate command again
aggregate(trip_data_v2$ride_length ~ trip_data_v2$member_casual + trip_data_v2$day_of_week, FUN = mean)
#further exploring the data, creating separate tables for the two member types
#for member
member_data <- trip_data_v2 %>% filter(member_casual=="member")
#for casual
casual_data <- trip_data_v2 %>% filter(member_casual=="casual")
#getting the number of trips by station so as to know the most and least used station for each member type
vec_count(member_data$start_station_name)
vec_count(casual_data$start_station_name)
#getting the number of trips by bike type
vec_count(member_data$rideable_type)
vec_count(casual_data$rideable_type)
#average ride time by each day in terms of member type
# analyze ridership data by type and weekday
trip_data_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)	
# Let's visualize the number of rides by rider type
trip_data_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
# Let's create a visualization for average duration
trip_data_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
#creating a csv of some important data so i can use Tableau to visualise
counts <- aggregate(trip_data_v2$ride_length ~ trip_data_v2$member_casual + trip_data_v2$day_of_week, FUN = mean)
write.csv(counts,'/Users/user/Documents/BIKESHARE/avg_ride_length.csv')
write.csv(casual_data,'/Users/user/Documents/BIKESHARE/casual.csv')
write.csv(member_data,'/Users/user/Documents/BIKESHARE/member.csv')
write.csv(trip_data_v2,'/Users/user/Documents/BIKESHARE/trip_data_v2.csv')
