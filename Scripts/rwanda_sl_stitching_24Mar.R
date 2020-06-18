#******************************************************************************************#
# This is the script for stitching data from all Rwanda SL together from July 2019 onwards #
# Author: K Bhargava                                                                       #
# Last updated on: 27th May 2020                                                           #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Define macros
AREA = 1.94
EFFICIENCY = 0.1649
#******************************************************************************************#

#******************************************************************************************#
# Set working directory to read and list all stitched files - data up to 30th Nov 2019
filepath <- "Data/Full"
file_list <- list.files(here(filepath))
output_directory <- "Data"

# Extract System overview Battery Power, PV-DC-coupled, AC consumption; Solar Charger PV current,PV voltage, 
# Load current, Battery watts; Battery Monitor State of charge; Inverter Output current, Output voltage
sl_rwanda <- map_dfr(file_list, ~ read.csv(here(filepath, .x), header=TRUE, stringsAsFactors = FALSE) %>%
                       mutate(streetlight = gsub("_.*","",.x)))
headers <- colnames(sl_rwanda)
sl_rwanda <- sl_rwanda[, c(headers[1], "streetlight",
                           headers[which(grepl("System.overview.PV...DC.coupled.W", headers, fixed=TRUE) | 
                                         grepl("System.overview.Battery.Power.W", headers, fixed=TRUE) | 
                                         grepl("System.overview.AC.Consumption.L1.W", headers, fixed=TRUE) |
                                         grepl("Solar.Charger.PV.current", headers, fixed=TRUE) |
                                         grepl("Solar.Charger.PV.voltage", headers, fixed=TRUE) |
                                         grepl("Solar.Charger.Load.current.A", headers, fixed=TRUE) |
                                         grepl("Solar.Charger.Battery.watts.W", headers, fixed=TRUE) |
                                         grepl("Battery.Monitor.State.of.charge..", headers, fixed=TRUE) |
                                         grepl("Battery.Monitor.Voltage.V", headers, fixed=TRUE) |
                                         grepl("Inverter.Output.current.A", headers, fixed=TRUE) | 
                                         grepl("Inverter.Output.voltage.V", headers, fixed=TRUE))])] 
write.csv(sl_rwanda, file=here(output_directory,"sl_all_raw_jun_nov.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Set working directory to read all files from 30th Nov 2019 to 30th April 2020
filepath <- "Data/Raw_data"
file_list <- list.files(here(filepath))
output_directory <- "Data"

sl_rwanda2 <- data.frame()
for(i in seq_along(file_list)) {
  # Victron files have log in the file name and are in csv format with a 3-row header
  headers <- read_csv(here(filepath,file_list[i]), col_names = FALSE, na="..", n_max = 3)
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  headers <- sub("\\[.*\\] ", "", headers)
  
  # Input data file
  df <- read_csv(here(filepath,file_list[i]), col_names = headers, na="..", skip = 3)
  df <- as.data.frame(df)
  df <- df[,seq_along(df)]
  
  # Subset data set
  df <- df[, c(headers[1], headers[which(grepl("System overview PV - DC-coupled W", headers, fixed=TRUE) | 
                                             grepl("System overview Battery Power W", headers, fixed=TRUE) | 
                                             grepl("System overview AC Consumption L1 W", headers, fixed=TRUE) |
                                             grepl("Solar Charger PV current ", headers, fixed=TRUE) |
                                             grepl("Solar Charger PV voltage ", headers, fixed=TRUE) |
                                             grepl("Solar Charger Load current A", headers, fixed=TRUE) |
                                             grepl("Solar Charger Battery watts W", headers, fixed=TRUE) |
                                             grepl("Battery Monitor State of charge %", headers, fixed=TRUE) |
                                             grepl("Battery Monitor Voltage V", headers, fixed=TRUE) |
                                             grepl("Inverter Output current A", headers, fixed=TRUE) | 
                                             grepl("Inverter Output voltage V", headers, fixed=TRUE))])] 
  df <- df %>% mutate(streetlight = substr(file_list[i],7,9))
  df <- df[,c(1,13,3,2,11,12,4,5,7,6,9,10,8)] # Re-arrange columns to match sl_rwanda data
  colnames(df)[1] <- c("timestamp")
  if(grepl("UTC", headers[1], fixed = TRUE)) {
    df$timestamp <- df$timestamp %m+% hours(2) }
  sl_rwanda2 <- rbind(sl_rwanda2, df)
}
write.csv(sl_rwanda2, file=here(output_directory,"sl_all_raw_nov_mar.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Set working directory to read all files from 30th Nov 2019 to 30th April 2020
filepath <- "Data"
sl_rwanda <- read.csv(here(filepath,"sl_all_raw_jun_nov.csv"), header = TRUE, stringsAsFactors = FALSE)
sl_rwanda2 <- read.csv(here(filepath,"sl_all_raw_nov_mar.csv"), header = TRUE, stringsAsFactors = FALSE)
colnames(sl_rwanda2) <- colnames(sl_rwanda)

sl_all_rwanda <- data.frame()
sl_all_rwanda <- rbind(sl_all_rwanda, sl_rwanda, sl_rwanda2)
sl_all_rwanda <- sl_all_rwanda[order(sl_all_rwanda$timestamp),]
sl_all_rwanda$date <- date(sl_all_rwanda$timestamp)
#Select dates greater than 1st July
sl_all_rwanda <- sl_all_rwanda[sl_all_rwanda$date>="2019-07-01",]
sl_all_rwanda <- sl_all_rwanda[,-14] #Remove date
write.csv(sl_all_rwanda, file=here(filepath,"sl_all_raw.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Set working directory to read weather data
filepath <- "Data/Weather data"
file_list <- list.files(here(filepath))
output_directory <- "Data"

weather <- data.frame()
for(i in seq_along(file_list)) {
  headers <- read_excel(here(filepath,file_list[i]), col_names = FALSE, na="..", n_max = 2)
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  
  weather_data <- read_excel(here(filepath,file_list[i]), col_names = headers, na="..", skip = 2,col_types=c("date","date","numeric","numeric"))
  weather_data <- weather_data[,-c(1,3)]
  weather_data$`In-plane PV Incident Solar kW/m2` <- weather_data$`In-plane PV Incident Solar kW/m2` * 
    AREA * EFFICIENCY * 1000.0
  s <- unlist(strsplit(gsub("_20.*","",file_list[i]), "_"))
  for(j in 1:(length(s)-1)) {
    weather_data <- cbind(weather_data, weather_data$`In-plane PV Incident Solar kW/m2`)
  }
  colnames(weather_data) <- c("timestamp",s)
  weather_data <- gather(weather_data, "streetlight", "value", 2:length(weather_data))
  weather <- rbind(weather, weather_data)
}
weather_data <- spread(weather, streetlight, value)
weather_data <- weather_data[complete.cases(weather_data), ]
write.csv(weather_data, file=here(output_directory,"weather_hourly_jul_mar.csv"), row.names=FALSE)
#******************************************************************************************#
#*************************************** EOF **********************************************#