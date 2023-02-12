# Checking my working directory
getwd()# returns an absolute filepath representing the current working directory
# Setting my working directory

# Load libraries
library(tidyverse)  #library for dealing with tabular data
library(lubridate) # library for handling dates
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)

# Importing my .csv data to the Global Environment
fire_incidents_data = read_csv("Fire_Incidents.csv")
#code to import a dataset and name it fire_incidents_data

# The dataset contains the following columns:
#   X, Y - longitude and latitude of incident
# OBJECTID, incident_number - numeric identifiers for each incident
# incident_type - numeric code for the type of the incident (e.g. house fire, smell of gas, etc)
# incident_type_description - plain-English description of the incident type code
# arrive_date_time - when firefighters arrived at the scene of the incident
# dispatch_date_time - when the call came in and firefighters were dispatched to the incident
# cleared_date_time - when firefighters cleared the incident and left the scene
# exposure - if the fire spread from another fire (e.g. a house fire started by a car fire)
# platoon - which shift responded to the incident
# station - which fire station responded to the incident
# address, address2 - address and address line 2 for the incident
# apt_room - apartment or suite number, if applicable
# GlobalID - numeric identifier
# CreationDate, EditDate - when the incident was entered into the system, and when the record
# was last edited
# Creator, Editor - who entered the incident, and who edited it most recently

# New columns created to the orginal dataset (fire_incidents_data):
# arrival_time - includes arrival date and time 
# dispatch_time - includes dispatch date and time
# cleared_time- includes cleared date and time
# response_time_secs- response time in seconds

# New dataset created:
# clean_fire_incidents_data - excludes NA data and negative numbers for response time
# station_summary
# dispatch_hour_count
# other...

#**Question 1 and 2:**
#( included summarise table for part 2 in write up)
# How long it takes Wake County Fire to respond to incidents, on average
# Does this response time vary by station? 
#What stations have the highest and lowest average response times?

# changes dates from character to time format and creating three new columns
fire_incidents_data$arrival_time <- strptime(substr(fire_incidents_data$arrive_date_time, 1, 19), "%Y/%m/%d %H:%M:%S")
fire_incidents_data$dispatch_time <- strptime(substr(fire_incidents_data$dispatch_date_time, 1, 19), "%Y/%m/%d %H:%M:%S")
fire_incidents_data$cleared_time <- strptime(substr(fire_incidents_data$cleared_date_time, 1, 19), "%Y/%m/%d %H:%M:%S")

# response time (secs) = arrival time - dispatch time
# creating new column called response_time_secs
fire_incidents_data$response_time_secs <- difftime(fire_incidents_data$arrival_time, fire_incidents_data$dispatch_time, units="secs") #subtraction

#finding average response time and removing all NAs
avg_response_time <- mean(as.numeric(fire_incidents_data$response_time_secs), na.rm = TRUE)
avg_response_time

# creating a new data frame with positive response times, which also excludes all NAs
clean_fire_incidents_data <- fire_incidents_data[fire_incidents_data$response_time_secs >= 0 & !(is.na(fire_incidents_data$response_time_secs)), ]
clean_fire_incidents_data

# grouping by station and creating new columns for response time calculations
# excludes all NA data 
station_summary <- clean_fire_incidents_data%>%
  group_by(station)%>%
  summarise(Average=mean(response_time_secs, na.rm=TRUE),
            Maximum=max(response_time_secs, na.rm = TRUE),
            Minimum=min(response_time_secs, na.rm = TRUE),
            Median=median(response_time_secs, na.rm = TRUE),
            Standard_Deviation=sd(response_time_secs, na.rm = TRUE))

station_summary

#**Question 3**
# Has the response time been going up and down over time? What could be the reason?
#(included plots for part 3 in write up)

# changing the numeric values of station to factors. 
clean_fire_incidents_data$station <- as.factor(clean_fire_incidents_data$station)

# Plotting a graph of the response time with their dates
ggplot(data = clean_fire_incidents_data,
       aes(x = as.Date(dispatch_time),
           y = as.numeric(response_time_secs),
           group = station,
           colour = station)) +
  geom_line() +
  labs(title= "Response time vs Dispatch Date",
       x = "Dispatch Date",
       y = "Response time (in secs)") +
  theme(axis.text.x = element_text(colour="grey20", size=12, angle=90, hjust=.5, vjust=.5),
        axis.text.y = element_text(colour="grey20", size=12),
        text=element_text(size=12))

#**Question 4**
# At what times of day are fire calls most likely to occur?

# creating dispatch_hour from the dispatch_time column
clean_fire_incidents_data$dispatch_hour <- format(as.POSIXct(clean_fire_incidents_data$dispatch_time), format = "%H") 

# changing the time values of dispatch_hour to factors
clean_fire_incidents_data$dispatch_hour <- as.factor(clean_fire_incidents_data$dispatch_hour)

dispatch_hour_count <- clean_fire_incidents_data %>% 
  count(dispatch_hour, sort = TRUE)
dispatch_hour_count #view table

#**Question 5**
# How many of them are actual fires?

# creating an actual fire data frame
actual_fire_data <- fire_incidents_data[fire_incidents_data$incident_type < 200 & !(is.na(fire_incidents_data$incident_type)), ]
actual_fire_data

#**Question 6**
# Evaluate the average response time to actual fires. 
actual_fire_avg_response_time <- mean(as.numeric(actual_fire_data$response_time_secs), na.rm = TRUE)
actual_fire_avg_response_time

#**Question 7**
# Repeat the analysis for questions 2-4 for actual fires

# 2) Does this response time vary by station? 
# What stations have the highest and lowest average response times?
# (included summarise table for part 2 in write up)

actual_fire_station_summary <- actual_fire_data%>%
  group_by(station)%>%
  summarise(Average=mean(response_time_secs, na.rm=TRUE),
            Maximum=max(response_time_secs, na.rm = TRUE),
            Minimum=min(response_time_secs, na.rm = TRUE),
            Median=median(response_time_secs, na.rm = TRUE),
            Standard_Deviation=sd(response_time_secs, na.rm = TRUE))

actual_fire_station_summary

# 3) Has the response time been going up and down over time? What could be the reason?
# changing the numeric values of station to factors. 
actual_fire_data$station <- as.factor(actual_fire_data$station)
#(included plots for part 3 in write up)

# Plotting a graph of the response time with their dates
ggplot(data = actual_fire_data,
       aes(x = as.Date(dispatch_time),
           y = as.numeric(response_time_secs),
           group = station,
           colour = station)) +
  geom_line() +
  labs(title= " Actual Fires Response time vs Dispatch Date",
       x = "Dispatch Date",
       y = "Response time (in secs)") +
  theme(axis.text.x = element_text(colour="grey20", size=12, angle=90, hjust=.5, vjust=.5),
        axis.text.y = element_text(colour="grey20", size=12),
        text=element_text(size=12))

# 4) At what times of day are fire calls most likely to occur?

# creating dispatch_hour from the dispatch_time column
actual_fire_data$dispatch_hour <- format(as.POSIXct(actual_fire_data$dispatch_time), format = "%H") 

# changing the time values of dispatch_hour to factors
actual_fire_data$dispatch_hour <- as.factor(actual_fire_data$dispatch_hour)

actual_fire_dispatch_hour_count <- actual_fire_data %>% 
  count(dispatch_hour, sort = TRUE)
actual_fire_dispatch_hour_count #view table