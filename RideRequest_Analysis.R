#1  a. How many total ride requests were made from May through December 2021? Do you notice anything? Explain. 
#   b. Plot this time series by day. 
library(shiny)
library(scales)
library(tidyverse)
library(ggplot2)
library(ggmap)
ride_data = read.csv('ride_requests.csv')
#summary(ride_data)
#str(ride_data)

# All the data in the columns have appropriate data types assigned, changed timestamp to datetime
# There are few missing values for the lat long columns of some stops that have updated lat long values in further rows
sum(is.na(ride_data$drop_off_stop_long))
sum(is.na(ride_data$drop_off_stop_lat))
ride_data$timestamp =
  as.Date(ride_data$timestamp)

#Counting the number of ride requests per day
ride_count_2021 = ride_data %>% 
  filter(timestamp >= "2021-05-01" & timestamp <= "2021-12-31")

ride_count <-
  ride_count_2021 %>% count(timestamp)
head(ride_count)

#Total number of ride requests
sum(ride_count$n)

#Time series plot
ts <- ggplot(ride_count, aes(x=timestamp, y=n)) +
  geom_line()+ labs(title="Plot Of No Of Ride Requests Over Time",
                     x ="Timestamp", y = "No of ride requests (per day)")
ts

#Analyzing further for the month of August
Aug = ride_data %>% 
  filter(timestamp >= "2021-08-01" & timestamp <= "2021-08-31")
ride_count_aug <-
  Aug %>% count(timestamp)
ts <- ggplot(ride_count_aug, aes(x=timestamp, y=n)) +
  geom_line() + labs(title="Plot Of No Of Ride Requests Over Time (August)",
                     x ="Timestamp", y = "No of ride requests (per day)")
ts

#2. a. What are the top 5 most popular stops in total (meaning pick up AND drop off together)                                               together)? 
#   b. Display your findings geospatially. 

#Merging the pick up and drop locations (the column now has list of all pick + drop locations)
total_loc <- 
  rbind(data.frame(loc=ride_data$drop_off_stop_name), data.frame(loc=ride_data$pick_up_stop_name))

loc_count <-
  total_loc %>% count(loc)
#head(loc_count)

sum(loc_count$n)

#arranging in descending order gives us the top 5 stops
(top_five = head(arrange(loc_count, desc(n)), 5))

#Had to omit the na's as some of the initial rows had missing values for lat and lon
ride_data_nona <- ride_data %>%
  na.omit()
  
#filtered for the top 5 locations to fetch the lat long values
ride_filt <- ride_data_nona %>% 
  filter(drop_off_stop_name %in% top_five$loc) %>%
  distinct(drop_off_stop_name, .keep_all = TRUE)

#register_google(key = "XXX",write = TRUE)
#register_google(XXX)

#Geospatial map with 5 locations plotted on it
nyc_map <- get_map(location = c(lon = -73.79561, lat = 40.68865), maptype = "terrain", zoom = 12)
ggmap(nyc_map)
ggmap(nyc_map) +
  geom_point(aes(drop_off_stop_long, drop_off_stop_lat), data = ride_filt)

#3  a. Create a new variable for the average number of days it takes each individual rider to request their second ride after they requested their first (you should get one number for each unique rider). 
#   b. Make a density plot of this data. 

ride_data <- ride_data[order(ride_data$timestamp),]

#Calculating the consecutive days timestamp difference for each rider(using the groupby)
new <- ride_data %>%
  group_by(rider_id) %>%
  mutate(diff = c(difftime(tail(timestamp, -1), head(timestamp, -1)),0))%>%
  mutate(diff = as.numeric(diff/86400))

#Calculating the average of the above computed no of days per rider
avg = new %>% 
  group_by(rider_id) %>%
  summarise(Avg_days = mean(diff))
#head(new$diff)
#str(avg)

hist(avg$Avg_days,xlab="Average no of days")
hist(avg$Avg_days,breaks = 20,labels=TRUE,main="Average no of days it takes for a rider to request next ride",xlab="Average no of days")
