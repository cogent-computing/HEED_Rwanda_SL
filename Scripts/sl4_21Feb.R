library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)
library(stringr)
library(rlang)
library(readr)

############# Read in data for SL4 - read in stitched data till Nov ##########
#Extract values System overview Battery Power, System overview PV-DC-coupled,
#Battery monitor state of charge, Solar Charger PV Power, 
#Battery Monitor Discharged energy and Battery Monitor Charged Energy 

setwd("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/Full/")
file_list <- list.files()
sl <- data.frame()
for(k in 4:4) {
  df <- read.csv(file_list[k], header=TRUE, stringsAsFactors = FALSE)
  
  headers = colnames(df)
  #Extract values for System overview Battery Power and System overview PV-DC-coupled
  #Find all column names with System overview
  columns <- headers[which(grepl("System.overview", headers, fixed=TRUE))]
  #Find all column names with PV - DC-coupled W
  colNames <- columns[which(grepl("PV...DC.coupled", columns, fixed=TRUE))]
  #Find all column names with Battery Power W
  colNames <- c(colNames, columns[which(grepl("Battery.Power", columns, fixed=TRUE))])
  colNames <- c(headers[1], colNames) 
  sysOverview <- df[,colNames]
  
  #Extract values for Solar Charger PV power
  #Find all column names with Solar Charger
  columns <- headers[which(grepl("Solar.Charger", headers, fixed=TRUE))]
  #Find all column names with PV power
  colNames <- columns[which(grepl("PV.power", columns, fixed=TRUE))]
  #Find all column names with PV.current
  colNames <- c(colNames, columns[which(grepl("PV.current", columns, fixed=TRUE))])
  #Find all column names with PV.voltage
  colNames <- c(colNames, columns[which(grepl("PV.voltage", columns, fixed=TRUE))])
  colNames <- c(headers[1], colNames) 
  solarCharger <- df[,colNames]
  
  #Extract values for Battery Monitor State of charge %, Discharged Energy kWh, Charged Energy kWh
  #Find all column names with Battery Monitor
  columns <- headers[which(grepl("Battery.Monitor", headers, fixed=TRUE))]
  #Find all column names with State of charge
  colNames <- columns[which(grepl("State.of.charge", columns, fixed=TRUE))]
  #Find all column names with Discharged Energy
  colNames <- c(colNames, columns[which(grepl("Discharged.Energy", columns, fixed=TRUE))])
  #Find all column names with Charged Energy
  colNames <- c(colNames, columns[which(grepl("Charged.Energy", columns, fixed=TRUE))])
  colNames <- c(headers[1], colNames) 
  batteryMonitor <- df[,colNames]
  
  systemData <- data.frame()
  systemData <- cbind(sysOverview, solarCharger[,-1], batteryMonitor[,-1])
  systemData$timestamp <- as.POSIXct(systemData$timestamp, tz="GMT", origin="1970-01-01")
  colnames(systemData) <- c("timestamp","PV_DC-coupled_W","Battery_power_W",
                            "PV_power_W", "PV_current_A","PV_voltage_V",
                            "State_of_charge","Discharged_energy_kWh",
                            "Charged_energy_kWh")
  systemData$Actual_power_W <- systemData$PV_current_A * systemData$PV_voltage_V
  ######Analyse data to get hourly values #######################################
  system_gather <- gather(systemData, "id", "value", 2:10)
  system_gather$timeUse <- format(system_gather$timestamp, format='%H')  
  system_gather$date <- date(system_gather$timestamp)
  
  #Hourly means can be calculated for AC consumption and PV power
  system_hourly <- system_gather[system_gather$id=="PV_DC-coupled_W" | 
                                   system_gather$id=="PV_power_W" |
                                   system_gather$id=="Battery_power_W" |
                                   system_gather$id=="Actual_power_W", ] %>%
    group_by(date,timeUse,id) %>%
    summarise(value=mean(value,na.rm = TRUE))
  system_hourly <- as.data.frame(system_hourly)
  
  #Calculate the last value in an hour for state of charge
  system_soc <- system_gather[system_gather$id=="State_of_charge",] %>%
    group_by(date,timeUse,id) %>%
    summarise(value=value[length(na.omit(value))])
  system_soc <- as.data.frame(system_soc)
  system_hourly <- rbind(system_hourly, system_soc)
  
  #Calculate hourly values for discharged and charged energy by taking hourly differences
  battery_charge <- system_gather[system_gather$id=="Charged_energy_kWh",]
  battery_charge <- battery_charge[complete.cases(battery_charge), ]
  #Extract hourly values by taking the last value for each hour 
  battery_charge_hours <- battery_charge %>%
    group_by(date, timeUse, id) %>%
    summarise(value = value[length(na.omit(value))])
  battery_charge_hours <- as.data.frame(battery_charge_hours)
  a <- diff(battery_charge_hours$value)
  battery_charge_hours <- battery_charge_hours[-1,]
  battery_charge_hours$value <- a
  battery_charge_hours$value <- battery_charge_hours$value * 1000.0 #W
  system_hourly <- rbind(system_hourly, battery_charge_hours)
  
  battery_discharge <- system_gather[system_gather$id=="Discharged_energy_kWh",]
  battery_discharge <- battery_discharge[complete.cases(battery_discharge), ]
  #Extract hourly values by taking the max value each hour 
  battery_discharge_hours <- battery_discharge %>%
    group_by(date, timeUse, id) %>%
    summarise(value = value[length(na.omit(value))])
  battery_discharge_hours <- as.data.frame(battery_discharge_hours)
  a <- diff(battery_discharge_hours$value)
  battery_discharge_hours <- battery_discharge_hours[-1,]
  battery_discharge_hours$value <- a
  battery_discharge_hours$value <- battery_discharge_hours$value * 1000.0 #W
  system_hourly <- rbind(system_hourly, battery_discharge_hours)
  
  #Add the Streetlight ID to the data
  system_hourly$streetlight <- rep(gsub("_.*","",file_list[k]), length(system_hourly$date))
  
  #Bind to sl data frame
  sl <- rbind(sl, system_hourly)
}
sl_write <- spread(sl, id, value)
sl_write$timestamp <- as.POSIXct(paste(paste(sl_write$date, sl_write$timeUse), ":00:01",sep=""),
                                 format="%Y-%m-%d %H:%M:%S", tz="GMT")
#Remove data before July 2019
sl_write <- sl_write[sl_write$date>=as.Date("2019-07-01"),]

#Replace NA values for charged and discharged energy with 0
sl_write$Charged_energy_kWh[is.na(sl_write$Charged_energy_kWh)] <- 0
sl_write$Discharged_energy_kWh[is.na(sl_write$Discharged_energy_kWh)] <- 0

sl_all <- sl_write
sl_write <- sl_write[,-c(1,2)]
sl_write <- sl_write[, c(9,1:8)]
colnames(sl_write) <- c("Time","ID","Actual_PV_power_W","Battery_power_W",
                        "Charged_energy_W","Discharged_energy_W",
                        "PV_DC_coupled_W", "PV_power_W", "State_of_charge_%")

##############################################################################################
#Get missing data for SL4
sl_all_gather <- gather(sl_all, "variable", "value", 4:10)
#sl_all_gather <- na.omit(sl_all_gather)
sl_all_gather$month <- as.character(month(sl_all_gather$date, label=TRUE, abbr=TRUE))
missing_sl <- sl_all_gather %>%
  group_by(month,date,streetlight,variable) %>%
  summarise(missingHours = 24-length(value[!is.na(value)]))
missing_sl <- as.data.frame(missing_sl)

#Plotting for SL1
missing_sl[missing_sl$streetlight=="SL1",] %>%
  ggplot(aes(x=date, y=missingHours, color=variable)) +
  geom_point() + 
  labs(title="Missing data for SL1" , 
       y="Number of hours missing",
       x = "Date of month")
lastDate <- max(sl_all_gather$date)

############################ Read in data after the one stitched #############################
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
sl2 <- data.frame()
for(k in 1:1) {
  #Read files for each CPE
  setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
  if(k==1) {
    setwd("./SL1/")
  } 
  
  #For SL1 - read files for Nov and Dec
  monthsList <- c("Nov","Dec","Jan")
  #Files available from 19th July 2019
  for(j in 1:length(monthsList)) {
    if(j==1) {
      setwd("./11 2019/")
    } else if(j==2) {
      setwd("../12 2019/")
    } else if(j==3) {
      setwd("../01 2020/")
    }
    
    #Input the file for each month
    file_list <- list.files()
    for(i in 1:length(file_list)) {
      #Concatenate the headers spread across first 3 rows
      headers <- read_csv(file_list[i], col_names = FALSE, na="..", n_max = 3)
      #Replace NA in header with "" for missing row 3 values
      headers[is.na(headers)] <- ""
      column_labels <- headers %>% summarize_all(str_c, collapse = " ")
      headers = unname(unlist(column_labels[1,]))
      
      #Read data without the first three rows
      df <- read_csv(file_list[i], col_names = headers, na="..", skip = 3)
      #Replace NA in data frame with "" for missing values as in raw file
      df[is.na(df)] <- ""
      
      #Extract values for System overview Battery Power and System overview PV-DC-coupled
      #Find all column names with System overview
      columns <- headers[which(grepl("System overview", headers, fixed=TRUE))]
      #Find all column names with PV - DC-coupled W
      colNames <- columns[which(grepl("PV - DC-coupled W", columns, fixed=TRUE))]
      #Find all column names with Battery Power W
      colNames <- c(colNames, columns[which(grepl("Battery Power W", columns, fixed=TRUE))])
      colNames <- c(headers[1], colNames) 
      sysOverview <- df[,colNames]
      
      #Extract values for Solar Charger PV power
      #Find all column names with Solar Charger
      columns <- headers[which(grepl("Solar Charger", headers, fixed=TRUE))]
      #Find all column names with PV power
      colNames <- columns[which(grepl("PV power", columns, fixed=TRUE))]
      #Find all column names with PV current
      colNames <- c(colNames, columns[which(grepl("PV current", columns, fixed=TRUE))])
      #Find all column names with PV voltage
      colNames <- c(colNames, columns[which(grepl("PV voltage", columns, fixed=TRUE))])
      colNames <- c(headers[1], colNames) 
      solarCharger <- df[,colNames]
      
      #Extract values for Battery Monitor State of charge %, Discharged Energy kWh, Charged Energy kWh
      #Find all column names with Battery Monitor
      columns <- headers[which(grepl("Battery Monitor", headers, fixed=TRUE))]
      #Find all column names with State of charge
      colNames <- columns[which(grepl("State of charge", columns, fixed=TRUE))]
      #Find all column names with Discharged Energy
      colNames <- c(colNames, columns[which(grepl("Discharged Energy", columns, fixed=TRUE))])
      #Find all column names with Charged Energy
      colNames <- c(colNames, columns[which(grepl("Charged Energy", columns, fixed=TRUE))])
      colNames <- c(headers[1], colNames) 
      batteryMonitor <- df[,colNames]
      
      systemData <- data.frame()
      systemData <- cbind(sysOverview, solarCharger[,-1], batteryMonitor[,-1])
      colnames(systemData) <- c("timestamp","PV_DC-coupled_W","Battery_power_W",
                                "PV_power_W", "PV_current_A","PV_voltage_V",
                                "State_of_charge","Discharged_energy_kWh",
                                "Charged_energy_kWh")
      systemData$`PV_DC-coupled_W` <- as.numeric(systemData$`PV_DC-coupled_W`)
      systemData$Battery_power_W <- as.numeric(systemData$Battery_power_W)
      systemData$Discharged_energy_kWh <- as.numeric(systemData$Discharged_energy_kWh)
      systemData$Charged_energy_kWh <- as.numeric(systemData$Charged_energy_kWh)
      
      systemData$month <- as.character(month(systemData$timestamp, label=TRUE, abbr=TRUE))
      systemData$Actual_power_W <- systemData$PV_current_A * systemData$PV_voltage_V
      ######Analyse data to get hourly values #######################################
      system_gather <- gather(systemData, "id", "value", c(2:9,11))
      system_gather$timeUse <- format(system_gather$timestamp, format='%H')  
      system_gather$date <- date(system_gather$timestamp)
      
      #Hourly means can be calculated for AC consumption and PV power
      system_hourly <- system_gather[system_gather$id=="PV_DC-coupled_W" | 
                                       system_gather$id=="PV_power_W" |
                                       system_gather$id=="Battery_power_W" |
                                       system_gather$id=="Actual_power_W", ] %>%
        group_by(date,timeUse,id) %>%
        summarise(value=mean(value,na.rm = TRUE))
      system_hourly <- as.data.frame(system_hourly)
      
      #Calculate the last value in an hour for state of charge
      system_soc <- system_gather[system_gather$id=="State_of_charge",] %>%
        group_by(date,timeUse,id) %>%
        summarise(value=value[length(na.omit(value))])
      system_soc <- as.data.frame(system_soc)
      system_hourly <- rbind(system_hourly, system_soc)
      
      #Calculate hourly values for discharged and charged energy by taking hourly differences
      battery_charge <- system_gather[system_gather$id=="Charged_energy_kWh",]
      battery_charge <- battery_charge[complete.cases(battery_charge), ]
      #Extract hourly values by taking the last value for each hour 
      battery_charge_hours <- battery_charge %>%
        group_by(date, timeUse, id) %>%
        summarise(value = value[length(na.omit(value))])
      battery_charge_hours <- as.data.frame(battery_charge_hours)
      a <- diff(battery_charge_hours$value)
      battery_charge_hours <- battery_charge_hours[-1,]
      battery_charge_hours$value <- a
      battery_charge_hours$value <- battery_charge_hours$value * 1000.0 #W
      system_hourly <- rbind(system_hourly, battery_charge_hours)
      
      battery_discharge <- system_gather[system_gather$id=="Discharged_energy_kWh",]
      battery_discharge <- battery_discharge[complete.cases(battery_discharge), ]
      #Extract hourly values by taking the max value each hour 
      battery_discharge_hours <- battery_discharge %>%
        group_by(date, timeUse, id) %>%
        summarise(value = value[length(na.omit(value))])
      battery_discharge_hours <- as.data.frame(battery_discharge_hours)
      a <- diff(battery_discharge_hours$value)
      battery_discharge_hours <- battery_discharge_hours[-1,]
      battery_discharge_hours$value <- a
      battery_discharge_hours$value <- battery_discharge_hours$value * 1000.0 #W
      system_hourly <- rbind(system_hourly, battery_discharge_hours)
      
      #Add the Streetlight ID to the data
      system_hourly$streetlight <- rep(paste("SL",k,sep=""), length(system_hourly$date))
      
      #Bind to sl data frame
      sl2 <- rbind(sl2, system_hourly)
    }
  }
}
sl_write2 <- spread(sl2, id, value)
sl_write2$timestamp <- as.POSIXct(paste(paste(sl_write2$date, sl_write2$timeUse), ":00:01",sep=""),
                                  format="%Y-%m-%d %H:%M:%S", tz="GMT")

#Replace NA values for charged and discharged energy with 0
sl_write2$Charged_energy_kWh[is.na(sl_write2$Charged_energy_kWh)] <- 0
sl_write2$Discharged_energy_kWh[is.na(sl_write2$Discharged_energy_kWh)] <- 0

sl_all <- rbind(sl_all, sl_write2)
sl_write2 <- sl_write2[,-c(1,2)]
sl_write2 <- sl_write2[, c(9, 1:8)]
colnames(sl_write2) <- c("Time","ID","Actual_PV_power_W","Battery_power_W",
                         "Charged_energy_W","Discharged_energy_W",
                         "PV_DC_coupled_W", "PV_power_W", "State_of_charge_%")
sl4_write <- data.frame()
sl4_write <- rbind(sl4_write, sl_write, sl_write2)
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
write.csv(sl4_write, file="SL4_jul_jan.csv", row.names=FALSE)

###############Remove overlapping entries ############################################
sl_all$date_time <- paste(sl_all$date, sl_all$timeUse, sep = " ")
sl_all <- sl_all[-which(duplicated(sl_all$date_time) == TRUE), ]
sl_all <- sl_all[,-12]
#Get missing data - july to jan for SL1
sl_all_gather <- gather(sl_all, "variable", "value", 4:10)
sl_all_gather$month <- as.character(month(sl_all_gather$date, label=TRUE, abbr=TRUE))
missing_sl <- sl_all_gather %>%
  group_by(month,date,streetlight,variable) %>%
  summarise(missingHours = 24-length(value[!is.na(value)]))
missing_sl <- as.data.frame(missing_sl)

#Plotting for SL1
missing_sl[missing_sl$streetlight=="SL1",] %>%
  ggplot(aes(x=date, y=missingHours, color=variable)) +
  geom_point() + 
  labs(title="Missing data for SL1" , 
       y="Number of hours missing",
       x = "Date of month") +
  scale_y_continuous(limits=c(0,24), 
                     breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24))
lastDate <- max(sl_all_gather$date)

###################################################################################################
#Hourly socket and system consumption for streetlight (1-7) in (month). 
#system and socket loads = Battery power - pv dc-coupled
sl_all$day <- weekdays(sl_all$date, abbr=TRUE)
sl_all$month <- as.character(month(sl_all$date, label=TRUE, abbr=TRUE))
sl_all$sysSocketLoad <- -1*(sl_all$Battery_power_W - sl_all$`PV_DC-coupled_W`)

sl_all_weday <- sl_all[sl_all$day=="Sat" | sl_all$day=="Sun", c(1,2,3,12,13,14)]
sl_all_wday <- sl_all[sl_all$day!="Sat" & sl_all$day!="Sun", c(1,2,3,12,13,14)]

plotMeans <- function(df) {
  df %>%
    ggplot(aes(x=timeUse, y=sysSocketLoad)) +
    geom_boxplot(aes(fill=timeUse))  +
    labs(y="Power consumption (W)",
         x = "Hour of day") + 
    theme(plot.title = element_text(size=10), legend.position = "none") 
}

p <- plotMeans(sl_all_wday[sl_all_wday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Jul'19")

p <- plotMeans(sl_all_weday[sl_all_weday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Jul'19")

p <- plotMeans(sl_all_wday[sl_all_wday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Aug'19")

p <- plotMeans(sl_all_weday[sl_all_weday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Aug'19")

p <- plotMeans(sl_all_wday[sl_all_wday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Sep'19")

p <- plotMeans(sl_all_weday[sl_all_weday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Sep'19")

p <- plotMeans(sl_all_wday[sl_all_wday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Oct'19")

p <- plotMeans(sl_all_weday[sl_all_weday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Oct'19")

p <- plotMeans(sl_all_wday[sl_all_wday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Nov'19")

p <- plotMeans(sl_all_weday[sl_all_weday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Nov'19")

p <- plotMeans(sl_all_wday[sl_all_wday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Dec'19")

p <- plotMeans(sl_all_weday[sl_all_weday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Dec'19")

p <- plotMeans(sl_all_wday[sl_all_wday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Jan'20")

p <- plotMeans(sl_all_weday[sl_all_weday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Jan'20")

##################################################################################
#Typical week (a) and weekend (b) day hourly socket and system consumption for a streetlight in (month).
typical_sl_wday <- sl_all_wday %>%
  group_by(streetlight, month, timeUse) %>%
  summarise(load = mean(sysSocketLoad))
typical_sl_wday <- as.data.frame(typical_sl_wday)

typical_sl_weday <- sl_all_weday %>%
  group_by(streetlight, month, timeUse) %>%
  summarise(load=mean(sysSocketLoad))
typical_sl_weday <- as.data.frame(typical_sl_weday)

plotTypical <- function(df) {
  df %>%
    ggplot(aes(x=as.numeric(timeUse), load, color=streetlight)) + 
    geom_line(aes(linetype=streetlight)) +
    geom_point(aes(shape=streetlight)) + 
    scale_shape_manual(values=c(0,1,4)) +
    scale_linetype_manual(values=c("solid","dashed", "dotted")) +
    labs(y="Power consumption (W)",
         x = "Time of day (hours)" ,
         color="Streetlight", 
         shape="Streetlight",
         linetype="Streetlight")
}

p <- plotTypical(typical_sl_wday[typical_sl_wday$month=="Jul",])
p + labs(title="Typical weekday system and socket load at Nepal SL in Jul'19")

p <- plotTypical(typical_sl_weday[typical_sl_weday$month=="Jul",])
p + labs(title="Typical weekend day system and socket load at Nepal SL in Jul'19")

p <- plotTypical(typical_sl_wday[typical_sl_wday$month=="Aug",])
p + labs(title="Typical weekday system and socket load at Nepal SL in Aug'19")

p <- plotTypical(typical_sl_weday[typical_sl_weday$month=="Aug",])
p + labs(title="Typical weekend day system and socket load at Nepal SL in Aug'19")

p <- plotTypical(typical_sl_wday[typical_sl_wday$month=="Sep",])
p + labs(title="Typical weekday system and socket load at Nepal SL in Sep'19")

p <- plotTypical(typical_sl_weday[typical_sl_weday$month=="Sep",])
p + labs(title="Typical weekend day system and socket load at Nepal SL in Sep'19")

p <- plotTypical(typical_sl_wday[typical_sl_wday$month=="Oct",])
p + labs(title="Typical weekday system and socket load at Nepal SL in Oct'19")

p <- plotTypical(typical_sl_weday[typical_sl_weday$month=="Oct",])
p + labs(title="Typical weekend day system and socket load at Nepal SL in Oct'19")

p <- plotTypical(typical_sl_wday[typical_sl_wday$month=="Nov",])
p + labs(title="Typical weekday system and socket load at Nepal SL in Nov'19")

p <- plotTypical(typical_sl_weday[typical_sl_weday$month=="Nov",])
p + labs(title="Typical weekend day system and socket load at Nepal SL in Nov'19")

p <- plotTypical(typical_sl_wday[typical_sl_wday$month=="Dec",])
p + labs(title="Typical weekday system and socket load at Nepal SL in Dec'19")

p <- plotTypical(typical_sl_weday[typical_sl_weday$month=="Dec",])
p + labs(title="Typical weekend day system and socket load at Nepal SL in Dec'19")

p <- plotTypical(typical_sl_wday[typical_sl_wday$month=="Jan",])
p + labs(title="Typical weekday system and socket load at Nepal SL in Jan'20")

p <- plotTypical(typical_sl_weday[typical_sl_weday$month=="Jan",])
p + labs(title="Typical weekend day system and socket load at Nepal SL in Jan'20")

#################################################################################
######### daily system and socket load ##########################################
###Daily socket and system consumption since commissioning
startDate <- as.Date("2019-07-01")
sl_all_daily <- sl_all %>%
  group_by(month, streetlight, date) %>%
  summarise(load = sum(sysSocketLoad))
sl_all_daily <- as.data.frame(sl_all_daily)
sl_all_daily$days <- as.numeric(sl_all_daily$date - startDate)

sl_all_daily[sl_all_daily$streetlight=="SL4",] %>%
  ggplot(aes(x=days, y=load, color="Actual")) +
  geom_point(shape=8) + 
  labs(title="Daily system and socket load at SL4 since commissioning (2019-07-01) " , 
       y="Total energy consumption (Wh)",
       x = "Days since commissioning",
       colour="") + 
  theme(plot.title = element_text(size=11),legend.position = "none") +
  scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180,200,220),
                     labels=c("0","20","40","60","80","100","120","140","160","180","200","220"))

#Getting daily average per month
sl_all_daily_avg <- sl_all_daily %>%
  group_by(month, streetlight) %>%
  summarise(load=mean(load))
sl_all_daily_avg <- as.data.frame(sl_all_daily_avg)
sl_all_daily_avg$id <- rep("Actual", length(sl_all_daily_avg$month))
sl_all_daily_avg$month2 <- factor(sl_all_daily_avg$month, 
                                  levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Jan"),
                                  labels = c("Jul","Aug","Sep","Oct","Nov","Dec","Jan"))
sl_all_daily_avg$id <- as.factor(sl_all_daily_avg$id)
sl_all_daily_avg[sl_all_daily_avg$streetlight=="SL1",] %>%
  ggplot(aes(x=month2, y=abs(load), fill=id)) +
  geom_bar(stat="identity", width=.5, position="dodge") + 
  labs(title="Average daily systems and sockets load at Nepal SL1 for each month" , 
       y="Average daily energy consumption (Wh)",
       x = "Month", 
       fill="") + 
  theme(plot.title = element_text(size=11),legend.position = "none")

#######################################################################
############################################################################
###Calculating socket load by subtracting system load - system load would vary for each streetlight
monthsList <- c("Jul","Aug","Sep","Oct","Nov","Dec","Jan")
sysLoadList <- c(8.125, 7.8, 7.625, 7.5, 7.625, 9.25, 15.75)
sl_all_daily$socketLoad <- sl_all_daily$load #for each month
sl_daily <- data.frame()
for(i in 1:length(monthsList)) {
  df <- sl_all_daily[sl_all_daily$month == monthsList[i], ]
  df$socketLoad <- df$socketLoad - (sysLoadList[i] * 24)
  sl_daily <- rbind(sl_daily, df)
}
sl_all_daily <- sl_daily

plotDaily <- function(df) {
  df %>%
    ggplot(aes(x = date, y= socketLoad, fill = streetlight)) +
    geom_bar(stat="identity", width=.5, position = "stack") + 
    labs( y="Daily socket consumption (Wh)",
          x = "Day of month",
          fill="") +
    theme(axis.text.x = element_text(size=9)) +
    scale_x_date(date_minor_breaks = "1 day")
}

p <- plotDaily(sl_all_daily[sl_all_daily$month=="Jul", ])
p + labs(title="Estimated streetlight socket consumption values in Jul'19")

p <- plotDaily(sl_all_daily[sl_all_daily$month=="Aug", ])
p + labs(title="Estimated streetlight socket consumption values in Aug'19")

p <- plotDaily(sl_all_daily[sl_all_daily$month=="Sep", ])
p + labs(title="Estimated streetlight socket consumption values in Sep'19")

p <- plotDaily(sl_all_daily[sl_all_daily$month=="Oct", ])
p + labs(title="Estimated streetlight socket consumption values in Oct'19")

p <- plotDaily(sl_all_daily[sl_all_daily$month=="Nov", ])
p + labs(title="Estimated streetlight socket consumption values in Nov'19")

p <- plotDaily(sl_all_daily[sl_all_daily$month=="Dec", ])
p + labs(title="Estimated streetlight socket consumption values in Dec'19")

p <- plotDaily(sl_all_daily[sl_all_daily$month=="Jan", ])
p + labs(title="Estimated streetlight socket consumption values in Jan'20")

##############################################################################
##Reading in weather data for all SL - all in 1 file
#Reading in weather data
setwd("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/Weather data/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  headers <- read_excel(file_list[i], col_names = FALSE, na="..", n_max = 2)
  #Replace NA in header with "" for missing row 3 values
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  weather_data <- read_excel(file_list[i], col_names = headers, na="..", skip = 2,
                             col_types=c("date","numeric","numeric"))
  weather_data <- weather_data[,-2]
  weather_data$`In-plane PV Incident Solar kW/m2` <- weather_data$`In-plane PV Incident Solar kW/m2` * 
    1.94 * 0.1649 * 1000.0
  s <- unlist(strsplit(gsub("_20.*","",file_list[i]), "_"))
  for(j in 1:(length(s)-1)) {
    weather_data <- cbind(weather_data, weather_data$`In-plane PV Incident Solar kW/m2`)
  }
  colnames(weather_data) <- c("timestamp",s)
  
  if(i==1) {
    weather <- weather_data
  } else {
    weather <- cbind(weather, weather_data[,-1])
  }
}
weather_data <- weather
##Data is 12 years behind - change year to 2019
weather_data$timestamp <- weather_data$timestamp %m+% years(12)
weather_data <- weather_data[weather_data$timestamp>="2019-07-01 00:00:00 GMT", ]
weather_data <- weather_data[complete.cases(weather_data), ]
weather_data <- weather_data[-1,]
setwd("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/")
write.csv(weather_data, file="weather_hourly_jul_dec.csv", row.names=FALSE)

################ Typical day profile for SL1 #################################
setwd("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/")
weather_data <- read.csv("weather_hourly_jul_dec.csv", header=TRUE,
                         stringsAsFactors = FALSE)
weather_data$timestamp <- as.POSIXct(weather_data$timestamp, tz="GMT", origin="1970-01-01")

####### Typical day profile ##############################
#Act. PV Power Output: [PV Power] - got the hourly values for Actual_power 
sl_all$month <- as.character(month(sl_all$date, label=TRUE, abbr=TRUE))
act_power_typical <- sl_all %>%
  group_by(month, streetlight,timeUse) %>%
  summarise(act_power = mean(PV_power_W))
act_power_typical <- as.data.frame(act_power_typical)
act_power_typical <- act_power_typical[act_power_typical$month!="Jan",]

#Total load: [IF (Battery Power <0, = Battery Power), IF(Battery Power >0, = Battery Power – PV-DC-Coupled)]. 
#calculate total load values
sl_all$totalLoad <- sl_all$Battery_power_W
for(i in 1:length(sl_all$date)) {
  if(sl_all$Battery_power_W[i]>=0) {
    sl_all$totalLoad[i] = sl_all$Battery_power_W[i] - sl_all$`PV_DC-coupled_W`[i]
  }
}
sl_all$totalLoad <- -1*sl_all$totalLoad
load_typical <- sl_all %>%
  group_by(month, streetlight,timeUse) %>%
  summarise(totalLoad = mean(totalLoad))
load_typical <- as.data.frame(load_typical)
load_typical <- load_typical[load_typical$month!="Jan",]

#Battery Charge Power - get hourly values from sl_all
battery_charge_typical <- sl_all %>%
  group_by(month, streetlight,timeUse) %>%
  summarise(battery_charge = mean(Charged_energy_kWh))
battery_charge_typical <- as.data.frame(battery_charge_typical)
battery_charge_typical <- battery_charge_typical[battery_charge_typical$month!="Jan",]

#Battery Discharge Power  - get hourly values from sl_all
battery_discharge_typical <- sl_all %>%
  group_by(month, streetlight,timeUse) %>%
  summarise(battery_discharge = mean(Discharged_energy_kWh))
battery_discharge_typical <- as.data.frame(battery_discharge_typical)
battery_discharge_typical <- battery_discharge_typical[battery_discharge_typical$month!="Jan",]

#Battery state of charge is stated directly - get hourly values
battery_state_typical <- sl_all %>%
  group_by(month, streetlight,timeUse) %>%
  summarise(battery_state = mean(State_of_charge))
battery_state_typical <- as.data.frame(battery_state_typical)
battery_state_typical <- battery_state_typical[battery_state_typical$month!="Jan",]

#Hourly Pot. PV power output from weather data
weather <- gather(weather_data, "streetlight", "Pot_PV_power", 2:5)
weather$timeUse <- format(weather$timestamp, format='%H')  
weather$month <- as.character(month(weather$timestamp, label=TRUE, abbr=TRUE)) 
pot_pv_typical <- weather %>%
  group_by(month, streetlight, timeUse) %>%
  summarise(pot_pv = mean(Pot_PV_power))
pot_pv_typical <- as.data.frame(pot_pv_typical)
#Select SL4 - Remove dec for now
pot_pv_typical <- pot_pv_typical[pot_pv_typical$streetlight=="SL4" 
                                 & pot_pv_typical$month!="Dec", ]

#Capture losses = Pot. PV power output – Act. PV power output 
pot_pv_typical$captureLosses <- pot_pv_typical$pot_pv - act_power_typical$act_power

#Getting all the typical day values together for the system
typical_day <- pot_pv_typical
typical_day <- cbind(typical_day, act_power_typical$act_power, 
                     load_typical$totalLoad,
                     battery_charge_typical$battery_charge, 
                     battery_discharge_typical$battery_discharge,
                     battery_state_typical$battery_state)
colnames(typical_day) <- c("month","streetlight","timeUse","Pot PV power",
                           "Capture loss","Actual PV power",
                           "Total load", "Battery charge energy", 
                           "Battery discharge energy",
                           "Battery state of charge")
typical_day$timeUse <- as.numeric(typical_day$timeUse)

#Change negative loss values to zero
for(i in 1:length(typical_day$month)) {
  if(typical_day$`Capture loss`[i]<0) {
    typical_day$`Capture loss`=0
  }
}

plotTypicalDay <- function(df) {
  df %>%
    ggplot(aes(x=timeUse)) +
    geom_line(aes(y = `Pot PV power`, color = "Potential PV power", group="Potential PV power"), linetype=1) + 
    geom_point(aes(y = `Pot PV power`, color = "Potential PV power"), shape=1) + 
    geom_line(aes(y = `Actual PV power`, color = "Actual PV power", group="Actual PV power"), linetype=2) + 
    geom_point(aes(y = `Actual PV power`, color = "Actual PV power"), shape=2) + 
    geom_line(aes(y = `Capture loss`, color = "Capture losses", group="Capture losses"), linetype=3) + 
    geom_point(aes(y = `Capture loss`, color = "Capture losses"), shape=3) + 
    geom_line(aes(y = `Battery charge energy`, color = "Charged Energy", group="Charged Energy"), linetype=4) + 
    geom_point(aes(y = `Battery charge energy`, color = "Charged Energy"), shape=4) + 
    geom_line(aes(y = `Battery discharge energy`, color = "Discharged Energy", group="Discharged Energy"), linetype=5) + 
    geom_point(aes(y = `Battery discharge energy`, color = "Discharged Energy"), shape=5) + 
    geom_line(aes(y = abs(`Total load`), color = "Total load", group="Total load"), linetype=6) + 
    geom_point(aes(y = abs(`Total load`), color = "Total load"), shape=6) + 
    geom_line(aes(y = `Battery state of charge`*2, color = "State of charge", group="State of charge"), linetype=7) + 
    geom_point(aes(y = `Battery state of charge`*2, color = "State of charge"), shape=7) +
    scale_y_continuous(sec.axis = sec_axis(~./2, name = "State of charge (%)")) +
    labs(y="Energy (Wh)",
         x = "Time of day (00-23 hours)", 
         colour="Parameter") +
    scale_x_continuous(labels=c("00","02","04","06","08","10","12","14","16","18",
                                "20","22"),
                       breaks=c(0,2,4,6,8,10,12,14,16,18,20,22))
}

###Plotting data for a typical day for SL1 per month
p <- plotTypicalDay(typical_day[typical_day$month=="Jul", ])
p + labs(title="Typical day profile for Nepal SL1 in Jul 2019")

p <- plotTypicalDay(typical_day[typical_day$month=="Aug", ])
p + labs(title="Typical day profile for Nepal SL1 in Aug 2019")

p <- plotTypicalDay(typical_day[typical_day$month=="Sep", ])
p + labs(title="Typical day profile for Nepal SL1 in Sep 2019")

p <- plotTypicalDay(typical_day[typical_day$month=="Oct", ])
p + labs(title="Typical day profile for Nepal SL1 in Oct 2019")

p <- plotTypicalDay(typical_day[typical_day$month=="Nov", ])
p + labs(title="Typical day profile for Nepal SL1 in Nov 2019")

p <- plotTypicalDay(typical_day[typical_day$month=="Dec", ])
p + labs(title="Typical day profile for Nepal SL1 in Dec 2019")

p <- plotTypicalDay(typical_day[typical_day$month=="Jan", ])
p + labs(title="Typical day profile for Nepal SL1 in Jan 2020")

###########################################################################
##Capture losses and surplus - daily values since commissioning
#Capture loss per day is given by pot pv power - act pv power - get hourly value
#Get Pot. PV daily per SL and per day
weather$date <- date(weather$timestamp)
weather_daily <- weather %>%
  group_by(month, date, streetlight) %>%
  summarise(pot_pv = sum(Pot_PV_power))
weather_daily <- as.data.frame(weather_daily)
#Extract Oct for now for SL1
weather_daily <- weather_daily[weather_daily$streetlight=="SL1",]

#Get Act PV daily - per SL per day
act_power_daily <- sl_all %>%
  group_by(month, date, streetlight) %>%
  summarise(act_pv = sum(Actual_power_W))
act_power_daily <- as.data.frame(act_power_daily)
act_power_daily <- act_power_daily[act_power_daily$month!="Jan",]
#Getting daily capture losses in total - all SL
weather_daily$loss <- weather_daily$pot_pv - act_power_daily$act_pv #Wh
weather_daily$days <- as.numeric(weather_daily$date - startDate)
for(i in 1:length(weather_daily$month)) {
  if(weather_daily$loss[i]<0) {
    weather_daily$loss[i]=0
  }
}
#Getting % of surplus used by sockets and system
#Surplus = [1-captureLosses(Wh)/(daily system and socket loads (Wh) + capture losses(Wh))]
#The system and socket load is given by [System Battery Power (W) - System PV-DC-Coupled (W)]
#system and socket load combined per SL per day - sysSocketLoad
sysSocketLoad_daily <- sl_all %>%
  group_by(month, date, streetlight) %>%
  summarise(load = sum(sysSocketLoad))
sysSocketLoad_daily <- as.data.frame(sysSocketLoad_daily)
sysSocketLoad_daily <- sysSocketLoad_daily[sysSocketLoad_daily$month!="Jan",]

sysSocketLoad_daily$surplus <- 1 - weather_daily$loss/
  (sysSocketLoad_daily$load + weather_daily$loss)
weather_daily$surplus <- sysSocketLoad_daily$surplus * 100.0
weather_daily$load <- sysSocketLoad_daily$load

weather_daily %>%
  ggplot(aes(x=days)) +
  geom_point(aes(y = loss, color = "Capture losses"), shape=1) +
  geom_point(aes(y = abs(load), color = "System and socket load"), shape=2) +
  geom_point(aes(y = abs(surplus)*18, color = "Surplus (%)"), shape=3) +
  scale_y_continuous(sec.axis = sec_axis(~./18, name = "Surplus (%)")) +
  labs(title="Estimated capture losses, load and % surplus used by external devices at Nepal SL1" , 
       y="Capture losses and load (Wh)",
       x = "Number of days since commissioning", 
       colour="Parameter") +
  theme(plot.title = element_text(size=9))

###############################################################################
#########Monthly capture losses, potential PV power and actual PV yield.
##Monthly potential PV power
weather_monthly <- weather %>%
  group_by(month, streetlight) %>%
  summarise(pot_pv = sum(Pot_PV_power))
weather_monthly <- as.data.frame(weather_monthly)
#Extract now for SL4
weather_monthly <- weather_monthly[weather_monthly$streetlight=="SL4",]
weather_monthly <- weather_monthly[weather_monthly$month!="Dec",]
##Monthly actual PV power
act_power_monthly <- sl_all %>%
  group_by(month, streetlight) %>%
  summarise(act_pv = sum(PV_power_W))
act_power_monthly <- as.data.frame(act_power_monthly)
act_power_monthly <- act_power_monthly[act_power_monthly$month!="Jan",]
weather_monthly$act_pv <- act_power_monthly$act_pv
weather_monthly$loss <- weather_monthly$pot_pv - weather_monthly$act_pv

weather_monthly$month2 <- factor(weather_monthly$month, 
                                 levels = c("Jul","Aug","Sep","Oct","Nov"),
                                 labels = c("Jul","Aug","Sep","Oct","Nov"))
weather_monthly %>%
  ggplot(aes(month2, loss/1000.0)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Capture losses at Rwanda SL4 in 2019" ,
       y="Capture losses (kWh)",
       x = "Month",
       fill="Parameter") 
