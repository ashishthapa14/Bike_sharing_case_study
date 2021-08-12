# Loading libraries
library(tidyverse)
library(lubridate)

#load Dataset
q1_2020 <- read_csv("D:\\Dataset_ash\\case study\\bike sharing divvy\\Divvy_Trips_2020_Q1.csv")
q2_2019 <- read_csv("D:\\Dataset_ash\\case study\\bike sharing divvy\\Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("D:\\Dataset_ash\\case study\\bike sharing divvy\\Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("D:\\Dataset_ash\\case study\\bike sharing divvy\\Divvy_Trips_2019_Q4.csv")

#data Wrangling
#Rename columns to make them consistent with q1_2020.
q2_2019 <- q2_2019 %>% rename(ride_id = "01 - Rental Details Rental ID",
                              rideable_type = "01 - Rental Details Bike ID",
                              started_at = "01 - Rental Details Local Start Time",
                              ended_at = "01 - Rental Details Local End Time"
                              ,start_station_name = "03 - Rental Start Station Name" 
                              ,start_station_id = "03 - Rental Start Station ID"
                              ,end_station_name = "02 - Rental End Station Name" 
                              ,end_station_id = "02 - Rental End Station ID"
                              ,member_casual = "User Type")

q3_2019 <- q3_2019 %>% rename(ride_id = "trip_id",
                              rideable_type = "bikeid",
                              started_at = "start_time",
                              ended_at = "end_time"
                              ,start_station_name = "from_station_name" 
                              ,start_station_id = "from_station_id"
                              ,end_station_name = "to_station_name" 
                              ,end_station_id = "to_station_id"
                              ,member_casual = "usertype")

q4_2019 <- q4_2019 %>% rename(ride_id = "trip_id",
                              rideable_type = "bikeid",
                              started_at = "start_time",
                              ended_at = "end_time"
                              ,start_station_name = "from_station_name" 
                              ,start_station_id = "from_station_id"
                              ,end_station_name = "to_station_name" 
                              ,end_station_id = "to_station_id"
                              ,member_casual = "usertype")

#converting ride_id and rideable_type to character so they can stack easily:
q2_2019 <- mutate(q2_2019,ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
q3_2019 <- mutate(q3_2019,ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
q4_2019 <- mutate(q4_2019,ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

#Concatenating
#stacking individual data frames into one.
all_trips_df <- bind_rows(q1_2020,q2_2019,q3_2019,q4_2019)
head(all_trips_df)

#Remove extra columns.
all_trips_df <- all_trips_df %>% select(-c(start_lat,start_lng,end_lat,
                                           end_lng,birthyear,gender,"01 - Rental Details Duration In Seconds Uncapped",
                                           "05 - Member Details Member Birthday Year","Member Gender","tripduration"))
#Data cleaning
table(all_trips_df$member_casual) #seeing how many observation fall under each userType

#Reassign to the desired values.
all_trips_df <- all_trips_df %>% mutate(member_casual = recode(member_casual, "Subscriber" = "member","Customer" = "casual"))

table(all_trips_df$member_casual) #check to make sure the proper number of observation.

# Parsing datetime columns

# Add columns that list the date, month, day, and year of each ride
all_trips_df$date <- as.Date(all_trips_df$started_at)
all_trips_df$month  <- format(as.Date(all_trips_df$date), "%m")
all_trips_df$day <- format(as.Date(all_trips_df$date),"%d")
all_trips_df$year <- format(as.Date(all_trips_df$date),"%Y")
all_trips_df$day_of_week <- format(as.Date(all_trips_df$date),"%A")
all_trips_df <- all_trips_df %>%
  mutate(year_month = paste(strftime(all_trips_df$started_at, "%Y"),
                            "-",
                            strftime(all_trips_df$started_at, "%m"),
                            paste("(",strftime(all_trips_df$started_at, "%b"), ")", sep="")))
all_trips_df <- all_trips_df %>%
  mutate(start_hour = strftime(all_trips_df$ended_at, "%H"))

#ride length
all_trips_df$ride_length <- (difftime(all_trips_df$ended_at,all_trips_df$started_at)/60)

#convert ride_length from factor to numeric for ease in calculation
all_trips_df$ride_length <- as.numeric(as.character(all_trips_df$ride_length))
summary(all_trips_df$ride_length)

#remove bad data
all_trips_df_v2 <- all_trips_df[!(all_trips_df$start_station_name == "HQ QR"|all_trips_df$ride_length<0),]

summary(all_trips_df_v2)

#casual vs members
all_trips_df_v2 %>% group_by(member_casual) %>% summarise(count = length(ride_id),'%' = (length(ride_id)/ nrow(all_trips_df_v2))*100)

# Compare members and casual users
aggregate(all_trips_df_v2$ride_length ~ all_trips_df_v2$member_casual, FUN = mean)
aggregate(all_trips_df_v2$ride_length ~ all_trips_df_v2$member_casual, FUN = median)
aggregate(all_trips_df_v2$ride_length ~ all_trips_df_v2$member_casual, FUN = max)
aggregate(all_trips_df_v2$ride_length ~ all_trips_df_v2$member_casual, FUN = min)

ggplot(data = all_trips_df_v2) + geom_bar(mapping  = aes(member_casual, fill = member_casual))
+ labs(x = "Casual x Members", title = "Chart 01 - Casuals x Members distribution")

all_trips_df_v2 %>%  group_by(year_month) %>% 
  summarise(count = length(ride_id),'%' = (length(ride_id) / nrow(all_trips_df_v2)) * 100,
                                                      'members_percent' = (sum(member_casual == "member") / length(ride_id)) * 100,"Casual_percent" = (sum(member_casual == "casual") / length(ride_id)) * 100, 'Member x Casual perc Differ' = members_percent - Casual_percent)
all_trips_df_v2 %>% ggplot() + geom_bar(mapping = aes(x=year_month,fill= member_casual)) + labs(x="Month",title = "Chart 02 - Distribution by month") + coord_flip()

all_trips_df_v2 %>% group_by(day_of_week) %>% summarise(count = length(ride_id),'%' = (length(ride_id)/nrow(all_trips_df_v2))*100,'members_percent' = (sum(member_casual == "member") / length(ride_id))*100,
                                                        'casual_percent' = (sum(member_casual == "casual")/length(ride_id))*100,'Member X Casual Perc Differ' = members_percent-casual_percent)
ggplot(all_trips_df_v2)+geom_bar(mapping = aes(x=day_of_week,fill = member_casual)) + labs(x="Weekday",title = "Chart 03 - Distribution by weekdasy") + coord_flip()

ventiles = quantile(all_trips_df_v2$ride_length, seq(0,1,by = 0.05))
ventiles

all_trips_outliers <- all_trips_df_v2 %>% filter(ride_length > as.numeric(ventiles['5%'])) %>% filter(ride_length < as.numeric(ventiles["95%"]))

print(paste("Removed",nrow(all_trips_df_v2) - nrow(all_trips_outliers),"row as outliers" ))

all_trips_outliers %>% group_by(member_casual) %>% 
  summarise(mean = mean(ride_length), 'first_quarter'
            = as.numeric(quantile(ride_length,.25)),
            'median' = median(ride_length), 'third_quarter'
            = as.numeric(quantile(ride_length,0.75)),
            'IR' = third_quarter - first_quarter)

ggplot(all_trips_outliers, aes(x = member_casual, y = ride_length, fill = member_casual)) + labs(x= "member x Casual", y = "Riding time", title = "chart 04 - Distribution of riding time for Casual x member") + geom_boxplot()

ggplot(all_trips_outliers) + geom_boxplot(mapping = aes(x=day_of_week, y=ride_length,fill = member_casual)) +
  facet_wrap(~ member_casual) + labs(x = "weekday",y = "Riding time",title = "chart 05 - Distribution of Riding for day of the week") + coord_flip()

