#Load packages
library(readr)
library(ggplot2)
library(corrplot)
library(PerformanceAnalytics)
library(lubridate)
library(plyr)
library(dplyr)


#Load in weather data
NOAA_SF_weather <- read_csv("NOAA_SF_weather.csv")

#Create separate variable for daily weather
NOAA_Daily_Weather <- NOAA_SF_weather[as.vector(is.na(NOAA_SF_weather[,2])),]
NOAA_Daily_Weather$DATE <- as.Date(NOAA_Daily_Weather$DATE)

#Load bike share data
BikeTripData <-read_csv("2017-fordgobike-tripdata.csv")
BikeTripData$bike_share_for_all_trip <- NA #2018 data has additional column not in 2017
BikeTripData <- rbind(BikeTripData
                      ,read_csv("201801-fordgobike-tripdata.csv")
                      ,read_csv("201802-fordgobike-tripdata.csv")
                      ,read_csv("201803-fordgobike-tripdata.csv")
                      ,read_csv("201804-fordgobike-tripdata.csv")
                      ,read_csv("201805-fordgobike-tripdata.csv")
                      )

#Add columns for Date/Hour/Day
BikeTripData$start_date <- as.Date(BikeTripData$start_time)
BikeTripData$start_hour <- hour(BikeTripData$start_time)
BikeTripData$start_day <- weekdays(BikeTripData$start_date)

#Combine the rideshare data to get the usage per day
BikeTripAggregate <- aggregate(BikeTripData["duration_sec"],by=BikeTripData["start_date"],sum)
BikeTripAggregate$average <- aggregate(BikeTripData$duration_sec,by=BikeTripData["start_date"],mean)[,2]
BikeTripAggregate$count <- BikeTripAggregate$duration_sec/BikeTripAggregate$average

#Combine weather data with rideshare data
CombinedWeatherBike <- NOAA_Daily_Weather[NOAA_Daily_Weather$DATE %in% BikeTripAggregate$start_date,]

#Add aggregate data
CombinedWeatherBike$TotalBikeTime <- BikeTripAggregate$duration_sec[BikeTripAggregate$start_date %in% CombinedWeatherBike$DATE]
CombinedWeatherBike$AverageBikeTime <- BikeTripAggregate$average[BikeTripAggregate$start_date %in% CombinedWeatherBike$DATE]
CombinedWeatherBike$TotalDailyBikers <- BikeTripAggregate$count[BikeTripAggregate$start_date %in% CombinedWeatherBike$DATE]

#Find correlations between temp/precipitation and bike time
cor(CombinedWeatherBike[,c(4,5,6,7,8,12,13,14,15)],method = "kendall")
corrplot(cor(CombinedWeatherBike[,c(4,5,6,7,8,12,13,14,15)],method = "kendall"))
chart.Correlation(CombinedWeatherBike[,c(4,5,6,7,8,12,13,14,15)],method = "kendall")

#From this graph, some tests appear extraneous
#Simplifying to only AVG temp, precipitation, and the 3 biking distributions.
corrplot(cor(CombinedWeatherBike[,c(6,12,13,14,15)],method = "kendall"))
chart.Correlation(CombinedWeatherBike[,c(6,12,13,14,15)],method = "kendall")


#Appears to have a correlation between weather and biking
#Higher temperatures = more biking

#Higher precipitation may result in less biking, but is less clear from this correlation test.
#Since precipitation appears to look more like a zero-inflated poisson, I used Kendall's as I figured that
#would result in the most accurate measure. 

#Improved ways of looking at precipitation would be a priority in refining this relationship.


#Let's see if we can refine this further. My hypothesis is there are two main biking populations.

#Commuters would primarily be renting weekdays before and after work. This population should not be as heavily
#influenced by weather, as they would need some sort of transportation regardless of weather.

#The second population would be casual bikers. These would be the people using the ride share intermittently.
#I would expect them to be more heavily influenced by weather, as they may either postpone their ride, or
#cancel it altogether.


#Using heatmaps to visualize ride density 
dayHour <- ddply(BikeTripData, c( "start_hour", "start_day"), summarise,
                 N    = length(start_date)
)


col1 = "#d8e1cf" 
col2 = "#438484"
ggplot(dayHour,aes(start_hour,start_day))+ geom_tile(aes(fill = N),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2)+  
  guides(fill=guide_legend(title="Rides taken")) +
  labs(title = "All Rides by Day and Hour",
       x = "Hour", y = "Day of the Week") +
  theme_bw() + theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dayHour <- ddply(BikeTripData, c( "start_hour", "start_day"), summarise,
                 N    = length(start_date)
)

#This data supports the idea of a large population of commuters.
#In attempting to look at how I could separate these populations, I decided to investigate the 
#user_type column in the bike share data.
#If commuters are primarily subscribers, this could be an easy way to separate these two populations

#Compare subscribers to customers

#Subscriber data
dayHourSub <- ddply(BikeTripData[BikeTripData$user_type == "Subscriber",], c( "start_hour", "start_day"), summarise,
                 N    = length(start_date)
)
#Customer Data
dayHourCus <- ddply(BikeTripData[BikeTripData$user_type == "Customer",], c( "start_hour", "start_day"), summarise,
                 N    = length(start_date)
)
                 
ggplot(dayHourSub,aes(start_hour,start_day))+ geom_tile(aes(fill = N),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2)+  
  guides(fill=guide_legend(title="Rides taken")) +
  labs(title = "Subscriber Rides by Day and Hour",
       x = "Hour", y = "Day of the Week") +
  theme_bw() + theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplot(dayHourCus,aes(start_hour,start_day))+ geom_tile(aes(fill = N),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2)+  
  guides(fill=guide_legend(title="Rides taken")) +
  labs(title = "Customer Rides by Day and Hour",
       x = "Hour", y = "Day of the Week") +
  theme_bw() + theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#My initial thought was that customer rides may look like a normal distribution regardless of day.
#However after looking at the data I think I need to reconsider my original assumption.
#Customers are primarily active on the weekends, which should not have been a surprise.
#This makes intuitive sense, since we would expect more recreational activity to occur on the weekend.
#I believe I can proceed with my initial goal of separating commuters from the rest using
#the user_type, since customers appear to be the recreational users I thought may be more affected by weather.


#Add columns to our groups for Subscriber/Customer
BikeTripAggregate$durationCust <- aggregate(BikeTripData[BikeTripData$user_type == "Customer",]$duration_sec,by=list(BikeTripData[BikeTripData$user_type == "Customer",]$start_date),sum)[,2]
BikeTripAggregate$averageCust <- aggregate(BikeTripData[BikeTripData$user_type == "Customer",]$duration_sec,by=list(BikeTripData[BikeTripData$user_type == "Customer",]$start_date),mean)[,2]
BikeTripAggregate$countCust <- BikeTripAggregate$durationCust/BikeTripAggregate$averageCust

BikeTripAggregate$durationSub <- aggregate(BikeTripData[BikeTripData$user_type == "Subscriber",]$duration_sec,by=list(BikeTripData[BikeTripData$user_type == "Subscriber",]$start_date),sum)[,2]
BikeTripAggregate$averageSub <- aggregate(BikeTripData[BikeTripData$user_type == "Subscriber",]$duration_sec,by=list(BikeTripData[BikeTripData$user_type == "Subscriber",]$start_date),mean)[,2]
BikeTripAggregate$countSub <- BikeTripAggregate$durationSub/BikeTripAggregate$averageSub



#Add aggregate data
CombinedWeatherBike$CustBikeTime <- BikeTripAggregate$durationCust[BikeTripAggregate$start_date %in% CombinedWeatherBike$DATE]
CombinedWeatherBike$AverageCustBikeTime <- BikeTripAggregate$averageCust[BikeTripAggregate$start_date %in% CombinedWeatherBike$DATE]
CombinedWeatherBike$CustDailyBikers <- BikeTripAggregate$countCust[BikeTripAggregate$start_date %in% CombinedWeatherBike$DATE]


CombinedWeatherBike$SubBikeTime <- BikeTripAggregate$durationSub[BikeTripAggregate$start_date %in% CombinedWeatherBike$DATE]
CombinedWeatherBike$AverageSubBikeTime <- BikeTripAggregate$averageSub[BikeTripAggregate$start_date %in% CombinedWeatherBike$DATE]
CombinedWeatherBike$SubDailyBikers <- BikeTripAggregate$countSub[BikeTripAggregate$start_date %in% CombinedWeatherBike$DATE]


cor(CombinedWeatherBike[,c(6,12,16,17,18)],method = "kendall")
corrplot(cor(CombinedWeatherBike[,c(6,12,16,17,18)],method = "kendall"))
chart.Correlation(CombinedWeatherBike[,c(6,12,16,17,18)],method = "kendall")

cor(CombinedWeatherBike[,c(6,12,19,20,21)],method = "kendall")
corrplot(cor(CombinedWeatherBike[,c(6,12,19,20,21)],method = "kendall"))
chart.Correlation(CombinedWeatherBike[,c(6,12,19,20,21)],method = "kendall")
