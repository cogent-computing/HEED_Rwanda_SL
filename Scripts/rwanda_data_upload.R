#******************************************************************************************#
# This is the script to prepare data for upload on the data portal                         #
# Author: K Bhargava                                                                       #
# Last updated on: 14th July 2020                                                           #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data/Supplementary data"
#******************************************************************************************#

#******************************************************************************************#
# Read SL data between July 2019 and March 2020, subset data and re arrange columns
sl_all <- read.csv(here(filepath,"sl_all_raw.csv"), header = TRUE, stringsAsFactors = FALSE)
sl_all <- sl_all %>% 
  mutate(Solar.Charger.PV.power.W = Solar.Charger.PV.current * Solar.Charger.PV.voltage,
         timestamp = as.POSIXct(timestamp, origin="1970-01-01",tz="GMT"),
         date = date(timestamp)) 

# Remove extra columns
sl_all <- sl_all[,-c(4:6,8:10,13)]
# Subset data till 31st March 2020
sl_all <- sl_all[sl_all$date<="2020-03-31",]
sl_all <- distinct(sl_all)
# Remove date
sl_all <- sl_all[,-8]
# Rename columns
colnames(sl_all) <- c("Timestamp", "Streetlight", "Battery.Monitor.State.of.Charge",
                      "Solar.Charger.Battery.Power.W", "System.Overview.AC.Consumption.W",
                      "System.Overview.Battery.Power.W", "Solar.Charger.PV.Power.W")
# Re-arrange columns
sl_all <- sl_all[,c(1,2,3,4,7,5,6)]
#******************************************************************************************#

#******************************************************************************************#
# Save data for each SL
sl <- sl_all[sl_all$Streetlight=="SL1", -2] #SL1
sl <- sl[order(sl$Timestamp),]
write.csv(sl, file=here(filepath,"SL1.csv"), row.names=FALSE)

sl <- sl_all[sl_all$Streetlight=="SL2", -2] #SL2
sl <- sl[order(sl$Timestamp),]
write.csv(sl, file=here(filepath,"SL2.csv"), row.names=FALSE)

sl <- sl_all[sl_all$Streetlight=="SL3", -2] #SL3
sl <- sl[order(sl$Timestamp),]
write.csv(sl, file=here(filepath,"SL3.csv"), row.names=FALSE)

sl <- sl_all[sl_all$Streetlight=="SL4", -2] #SL4
sl <- sl[order(sl$Timestamp),]
write.csv(sl, file=here(filepath,"SL4.csv"), row.names=FALSE)
#******************************************************************************************#