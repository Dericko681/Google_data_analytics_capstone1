#process phase
#install packages which i will use in this project
install.packages("tidyverse") #helps to transform and better present data: importation, tidying, manipulation...
install.packages("lubridate") #facilitates working with dates and time
install.packages("here") #enables easy file referencing
install.packages("janitor") #provides simple tools ofr examining and cleaning data
install.packages("skimr") #this package provides alternatives to the default summary function si R
install.packages("ggplot2") #dediacted to data visualization
install.packages("tidyr") #facilitates data cleaning
install.packages("readr") #facilitates data importation



## Loading installed packages

library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(here)
library(readr)
library(tidyr)
library(ggplot2)


## Uploading 12 csv files for cleaning and analysis
january <- read_csv("C:/Users/gee/Desktop/bikeshare/202101-divvy-tripdata.csv")
february <- read_csv("C:/Users/gee/Desktop/bikeshare/202102-divvy-tripdata.csv")
march <- read_csv("C:/Users/gee/Desktop/bikeshare/202103-divvy-tripdata.csv")
april <- read_csv("C:/Users/gee/Desktop/bikeshare/202004-divvy-tripdata.csv")
may <- read_csv("C:/Users/gee/Desktop/bikeshare/202005-divvy-tripdata.csv")
june<- read_csv("C:/Users/gee/Desktop/bikeshare/202006-divvy-tripdata.csv")
july <- read_csv("C:/Users/gee/Desktop/bikeshare/202007-divvy-tripdata.csv")
august <- read_csv("C:/Users/gee/Desktop/bikeshare/202008-divvy-tripdata.csv")
september <- read_csv("C:/Users/gee/Desktop/bikeshare/202009-divvy-tripdata.csv")
october <- read_csv("C:/Users/gee/Desktop/bikeshare/202010-divvy-tripdata.csv")
november <- read_csv("C:/Users/gee/Desktop/bikeshare/202011-divvy-tripdata.csv")
december<- read_csv("C:/Users/gee/Desktop/bikeshare/202012-divvy-tripdata.csv")


##converting the data types of station id and time
january <-  mutate(jan, start_station_id = as.character(start_station_id)
	                      ,end_station_id = as.character(end_station_id)
			                     ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
			                     ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

february <-  mutate(feb, start_station_id = as.character(start_station_id)
	                      ,end_station_id = as.character(end_station_id)
			                     ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
			                     ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

march <-  mutate(mar, start_station_id = as.character(start_station_id)
	                      ,end_station_id = as.character(end_station_id)
			                     ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
			                     ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))
april <-  mutate(apr, start_station_id = as.character(start_station_id)
	                      ,end_station_id = as.character(end_station_id)
			                     ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
			                     ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

may <-  mutate(may, start_station_id = as.character(start_station_id)
	                      ,end_station_id = as.character(end_station_id)
			                     ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
			                     ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

june <-  mutate(jun, start_station_id = as.character(start_station_id)
	                      ,end_station_id = as.character(end_station_id)
			                     ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
			                     ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

july <-  mutate(jul, start_station_id = as.character(start_station_id)
	                      ,end_station_id = as.character(end_station_id)
			                     ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
			                     ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

august <-  mutate(aug, start_station_id = as.character(start_station_id)
	                      ,end_station_id = as.character(end_station_id)
			                     ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
			                     ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

september <-  mutate(sep, start_station_id = as.character(start_station_id)
	                      ,end_station_id = as.character(end_station_id)
			                     ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
			                     ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

october <-  mutate(oct, start_station_id = as.character(start_station_id)
	                      ,end_station_id = as.character(end_station_id)
			                     ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
			                     ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

november <-  mutate(nov, start_station_id = as.character(start_station_id)
	                      ,end_station_id = as.character(end_station_id)
			                     ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
			                     ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

december <-  mutate(dec, start_station_id = as.character(start_station_id)
	                      ,end_station_id = as.character(end_station_id)
			                     ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
				      ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))



##data cleaning
# Merging the 12 different months into one data frame called ridedata

ridedata <- bind_rows(january,february,march,april,may,june,july,august,september,october,november,december)

## Inspect the combined data

colnames(ridedata)
str(ridedata)
head(ridedata)

## Clean dataset by removing unnessesary colons
ridedata <- ridedata %>% select(-c(start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng))

## Add date, month, day, and trip length colons
ridedata$date <- as.Date(ridedata$started_at)
ridedata$month <- format(as.Date(ridedata$date), "%B")
ridedata$day <- format(as.Date(ridedata$date), "%A")
ridedata$trip_length <- difftime(ridedata$ended_at, ridedata$started_at)

# More data cleaning of new colons

## remove empty rows, dataset trimmed down from 3489748 rows to 3294691
colSums(is.na(ridedata)) #returns a summary of empty colons on each variable
cleanridedata <- ridedata [complete.cases(ridedata), ]

## Remove all negative and zero trip length as well as NA/null values
cleanridedata <- subset(cleanridedata, trip_length>0)

## Convert ride length to integer
cleanridedata$trip_length <- as.integer(cleanridedata$trip_length)

