library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)
library(stringr)
library(rlang)
library(readr)
library(openxlsx)

##Read in the stiched data for all 4 lights - as older data disappears from the Victron portal
#Extract values System overview Battery Power, System overview PV-DC-coupled,
#Solar Charger PV Power, Battery monitor state of charge 
#Battery Monitor Discharged energy and Battery Monitor Charged Energy 
setwd("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/Full/")
file_list <- list.files()
sl_rwanda <- data.frame()
for(k in 1:length(file_list)) {
  df <- read.csv(file_list[k], header=TRUE, stringsAsFactors = FALSE)
  
  headers = colnames(df)
  columns <- headers[which(grepl("System.overview", headers, fixed=TRUE))]
  colNames <- columns[which(grepl("PV...DC.coupled", columns, fixed=TRUE))]
  colNames <- c(colNames, columns[which(grepl("Battery.Power", columns, fixed=TRUE))])
  colNames <- c(headers[1], colNames) 
  sysOverview <- df[,colNames]
  sysOverview <- as.data.frame(sysOverview)
  sysOverview <- sysOverview[,c(1:length(sysOverview))]
  
  columns <- headers[which(grepl("Solar.Charger", headers, fixed=TRUE))]
  colNames <- columns[which(grepl("PV.current", columns, fixed=TRUE))]
  colNames <- c(colNames, columns[which(grepl("PV.voltage", columns, fixed=TRUE))])
  colNames <- c(colNames, columns[which(grepl("Load.current", columns, fixed=TRUE))])
  colNames <- c(colNames, columns[which(grepl("Battery.watts", columns, fixed=TRUE))])
  colNames <- c(headers[1], colNames) 
  solarCharger <- df[,colNames]
  solarCharger <- as.data.frame(solarCharger)
  solarCharger <- solarCharger[,c(1:length(solarCharger))]
  colnames(solarCharger) <- c("timestamp","PV_current_A", "PV_voltage_V", "Load_current_A",
                              "Battery_watts_W")
  solarCharger$PV_power_W <- 
    solarCharger$PV_current_A * solarCharger$PV_voltage_V
  solarCharger <- solarCharger[,-c(2,3)] #Remove PV current and PV voltage
  solarCharger <- solarCharger[,c(1,4,3,2)] #Rearrange columns
  
  columns <- headers[which(grepl("Battery.Monitor", headers, fixed=TRUE))]
  colNames <- columns[which(grepl("State.of.charge", columns, fixed=TRUE))]
  colNames <- c(colNames, columns[which(grepl("Discharged.Energy", columns, fixed=TRUE))])
  colNames <- c(colNames, columns[which(grepl("Charged.Energy", columns, fixed=TRUE))])
  colNames <- c(headers[1], colNames) 
  batteryMonitor <- df[,colNames]
  batteryMonitor <- as.data.frame(batteryMonitor)
  batteryMonitor <- batteryMonitor[,c(1:length(batteryMonitor))]
  
  systemData <- data.frame()
  systemData <- cbind(sysOverview, solarCharger[,-1], batteryMonitor[,-1])
  #Time is in Nepal (GMT+5:45) - just parsing time to time in GMT
  systemData$timestamp <- as.POSIXct(systemData$timestamp, tz="GMT", origin="1970-01-01")
  
  colnames(systemData) <- c("timestamp","PV_DC-coupled_W","Battery_power_W",
                            "PV_power_W", "Battery_watts_W", "Load_current_A",
                            "State_of_charge","Discharged_energy_kWh",
                            "Charged_energy_kWh")
  
  #Add the Streetlight ID to the data
  systemData$streetlight <- rep(gsub("_.*","",file_list[k]), length(systemData$timestamp))
  
  #Bind to sl data frame
  sl_rwanda <- rbind(sl_rwanda, systemData)
}
write.csv(sl_rwanda, "~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/sl_all_raw_jun_nov.csv",
          row.names = FALSE)
###Last date read for each SL 1-4
#sl_1_rwanda <- range(sl_rwanda$timestamp[sl_rwanda$streetlight=="SL1"]) #2019-06-15 to 2019-11-30
#sl_2_rwanda <- range(sl_rwanda$timestamp[sl_rwanda$streetlight=="SL2"]) #2019-06-15 to 2019-11-30
#sl_3_rwanda <- range(sl_rwanda$timestamp[sl_rwanda$streetlight=="SL3"]) #2019-06-15 to 2019-12-01
#sl_4_rwanda <- range(sl_rwanda$timestamp[sl_rwanda$streetlight=="SL4"]) #2019-06-16 to 2019-11-30

########################################################################################################
### Read in data for all SL - read data 30th Nov 2019
##Data read till 23rd March 2020
setwd("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/")
sl_rwanda2 <- data.frame()
for(k in 1:4) {
  #Read files for each SL
  setwd("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/")
  if(k==1) {
    setwd("./SL1/")
  } else if(k==2) {
    setwd("./SL2/")
  } else if(k==3) {
    setwd("./SL3/")
  } else if(k==4) {
    setwd("./SL4/")
  }
  
  #Input the file for each SL
  file_list <- list.files()
  #SD card files have SD in it and are xlsx - 2 line header
  #Victron files have log in it and are csv - 3 line header
  for(i in 1:length(file_list)) {
    
    headers <- read_csv(file_list[i], col_names = FALSE, na="..", n_max = 3)
    headers[is.na(headers)] <- ""
    column_labels <- headers %>% summarize_all(str_c, collapse = " ")
    headers = unname(unlist(column_labels[1,]))
    
    df <- read_csv(file_list[i], col_names = headers, na="..", skip = 3)
    df[is.na(df)] <- ""
    
    columns <- headers[which(grepl("System overview", headers, fixed=TRUE))]
    colNames <- columns[which(grepl("PV - DC-coupled", columns, fixed=TRUE))]
    colNames <- c(colNames, columns[which(grepl("Battery Power", columns, fixed=TRUE))])
    colNames <- c(headers[1], colNames) 
    sysOverview <- df[,colNames]
    sysOverview <- as.data.frame(sysOverview)
    sysOverview <- sysOverview[,c(1:length(sysOverview))]
    colnames(sysOverview) <- c("timestamp","PV_DC-coupled_W","Battery_power_W")
    sysOverview$`PV_DC-coupled_W` <- 
      as.numeric(sysOverview$`PV_DC-coupled_W`)
    sysOverview$Battery_power_W <- 
      as.numeric(sysOverview$Battery_power_W)
    
    columns <- headers[which(grepl("Solar Charger", headers, fixed=TRUE))]
    colNames <- columns[which(grepl("PV current", columns, fixed=TRUE))]
    colNames <- c(colNames, columns[which(grepl("PV voltage", columns, fixed=TRUE))])
    colNames <- c(colNames, columns[which(grepl("Load current", columns, fixed=TRUE))])
    colNames <- c(colNames, columns[which(grepl("Battery watts", columns, fixed=TRUE))])
    colNames <- c(headers[1], colNames) 
    solarCharger <- df[,colNames]
    solarCharger <- as.data.frame(solarCharger)
    solarCharger <- solarCharger[,c(1:length(solarCharger))]
    colnames(solarCharger) <- c("timestamp","PV_current_A", "PV_voltage_V", "Load_current_A",
                                "Battery_watts_W")
    solarCharger$PV_current_A <- as.numeric(solarCharger$PV_current_A)
    solarCharger$PV_voltage_V <- as.numeric(solarCharger$PV_voltage_V)
    solarCharger$Load_current_A <- as.numeric(solarCharger$Load_current_A)
    solarCharger$Battery_watts_W <- as.numeric(solarCharger$Battery_watts_W)
    solarCharger$PV_power_W <- 
      solarCharger$PV_current_A * solarCharger$PV_voltage_V
    solarCharger <- solarCharger[,-c(2,3)] #Remove PV current and PV voltage
    solarCharger <- solarCharger[,c(1,4,3,2)] #Rearrange columns
    
    columns <- headers[which(grepl("Battery Monitor", headers, fixed=TRUE))]
    colNames <- columns[which(grepl("State of charge", columns, fixed=TRUE))]
    colNames <- c(colNames, columns[which(grepl("Discharged Energy", columns, fixed=TRUE))])
    colNames <- c(colNames, columns[which(grepl("Charged Energy", columns, fixed=TRUE))])
    colNames <- c(headers[1], colNames) 
    batteryMonitor <- df[,colNames]
    batteryMonitor <- as.data.frame(batteryMonitor)
    batteryMonitor <- batteryMonitor[,c(1:length(batteryMonitor))]
    colnames(batteryMonitor) <- c("timestamp","State_of_charge","Discharged_energy_kWh",
                                  "Charged_energy_kWh")
    batteryMonitor$State_of_charge <- as.numeric(batteryMonitor$State_of_charge)
    batteryMonitor$Discharged_energy_kWh <- as.numeric(batteryMonitor$Discharged_energy_kWh)
    batteryMonitor$Charged_energy_kWh <- as.numeric(batteryMonitor$Charged_energy_kWh)
    #test <- gather(batteryMonitor, "variable","value",2:length(batteryMonitor))
    #test$variable <- substr(test$variable, 23, 39)
    #test$value[test$value==""] <- NA
    #test <- test[complete.cases(test),]
    #test <- spread(test, variable, value)
    
    systemData <- data.frame()
    systemData <- cbind(sysOverview, solarCharger[,-1], batteryMonitor[,-1])
    
    #Add the Streetlight ID to the data
    systemData$streetlight <- rep(substr(file_list[i],7,9), length(systemData$timestamp))
    
    #Bind to sl data frame
    sl_rwanda2 <- rbind(sl_rwanda2, systemData)
  }
}
write.csv(sl_rwanda2, "~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/sl_all_raw_nov_mar.csv",
          row.names = FALSE)

#######################################################################################################
###Read in the raw data files
sl_rwanda <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/sl_all_raw_jun_nov.csv",
               header = TRUE, stringsAsFactors = FALSE)
sl_rwanda$timestamp <- as.POSIXct(sl_rwanda$timestamp, tz="GMT", origin="1970-01-01")

sl_rwanda2 <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/sl_all_raw_nov_mar.csv",
                header = TRUE, stringsAsFactors = FALSE)
sl_rwanda2$timestamp <- as.POSIXct(sl_rwanda2$timestamp, tz="GMT", origin="1970-01-01")

sl_all_rwanda <- data.frame()
sl_all_rwanda <- rbind(sl_all_rwanda, sl_rwanda, sl_rwanda2)
sl_all_rwanda <- sl_all_rwanda[order(sl_all_rwanda$timestamp),]
sl_all_rwanda$date <- date(sl_all_rwanda$timestamp)
sl_all_rwanda$month <- as.character(month(sl_all_rwanda$timestamp, label=TRUE, abbr=TRUE))
sl_all_rwanda$timeUse <- format(sl_all_rwanda$timestamp, format="%H")

#Select dates greater than 1st July
sl_all_rwanda <- sl_all_rwanda[sl_all_rwanda$date>="2019-06-17",]

##Getting typical load current values for each light per month
library(dplyr)
typical_load_current <- sl_all_rwanda %>%
  dplyr::group_by(month, timeUse, streetlight) %>%
  dplyr::summarise(current = mean(Load_current_A, na.rm=TRUE))
typical_load_current <- as.data.frame(typical_load_current)

plotTypical <- function(df) {
  df %>%
    ggplot(aes(x=as.numeric(timeUse), current, color=streetlight)) + 
    geom_line(aes(linetype=streetlight)) +
    geom_point(aes(shape=streetlight)) +
    scale_shape_manual(values=c(1,4,1,4,1,4,1)) +
    labs(y="Load current (A)",
         x = "Time of day (hours)" ,
         color="Streetlight", 
         shape="Streetlight",
         linetype="Streetlight") + 
    scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24),
                       labels=c("0","2","4","6","8","10","12","14","16","18","20","22","24"))
}

plotTypical(typical_load_current[typical_load_current$month=="Jun",]) +
  labs(title="Load current for Rwanda SL in Jun 2019")

plotTypical(typical_load_current[typical_load_current$month=="Jul",]) +
  labs(title="Load current for Rwanda SL in Jul 2019")

plotTypical(typical_load_current[typical_load_current$month=="Aug",]) +
  labs(title="Load current for Rwanda SL in Aug 2019")

plotTypical(typical_load_current[typical_load_current$month=="Sep",]) +
  labs(title="Load current for Rwanda SL in Sep 2019")

plotTypical(typical_load_current[typical_load_current$month=="Oct",]) +
  labs(title="Load current for Rwanda SL in Oct 2019")

plotTypical(typical_load_current[typical_load_current$month=="Nov",]) +
  labs(title="Load current for Rwanda SL in Nov 2019")

plotTypical(typical_load_current[typical_load_current$month=="Dec",]) +
  labs(title="Load current for Rwanda SL in Dec 2019")

plotTypical(typical_load_current[typical_load_current$month=="Jan",]) +
  labs(title="Load current for Rwanda SL in Jan 2020")

plotTypical(typical_load_current[typical_load_current$month=="Feb",]) +
  labs(title="Load current for Rwanda SL in Feb 2020")

plotTypical(typical_load_current[typical_load_current$month=="Mar",]) +
  labs(title="Load current for Rwanda SL in Mar 2020")

##Load cut offs - ignore data when load current==0 and time is less than 6am or greater than 6pm
#Remove rows where the light load is 0 between 18:00 and 6:00
sl_all_rwanda$time <- strftime(sl_all_rwanda$timestamp, "%H:%M:%S", tz="GMT")
#systemData <- sl_all[is.na(sl_all$Load_current_A) | sl_all$Load_current_A!=0 | 
#                       (sl_all$Load_current_A==0 & (sl_all$time>"06:00:00" & 
#                                                      sl_all$time<"18:00:00")), ]
systemData <- sl_all_rwanda
#Remove Load current and time value
systemData <- systemData[,-c(6,14)]
systemData <- gather(systemData, "id", "value", 2:8)

hourlyMeans <- function(system_gather) {
  #Hourly means can be calculated for AC consumption and PV power
  system_hourly <- system_gather[system_gather$id=="PV_DC.coupled_W" | 
                                   system_gather$id=="PV_power_W" |
                                   system_gather$id=="Battery_power_W" |
                                   system_gather$id=="Battery_watts_W", ] %>%
    dplyr::group_by(streetlight, date, timeUse, id) %>%
    dplyr::summarise(value=mean(value,na.rm = TRUE))
  system_hourly <- as.data.frame(system_hourly)
  
  #Calculate the last value in an hour for state of charge
  system_soc <- system_gather[system_gather$id=="State_of_charge",] %>%
    dplyr::group_by(streetlight, date, timeUse, id) %>%
    dplyr::summarise(value=mean(value, na.rm=TRUE))
  system_soc <- as.data.frame(system_soc)
  system_hourly <- rbind(system_hourly, system_soc)
  
  #Calculate hourly values for discharged and charged energy by taking hourly differences
  battery_charge <- system_gather[system_gather$id=="Charged_energy_kWh",]
  battery_charge <- battery_charge[complete.cases(battery_charge), ]
  #Extract hourly values by taking the last value for each hour 
  battery_charge_hours <- battery_charge %>%
    dplyr::group_by(streetlight, date, timeUse, id) %>%
    dplyr::summarise(value = value[length(na.omit(value))])
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
    dplyr::group_by(streetlight, date, timeUse, id) %>%
    dplyr::summarise(value = value[length(na.omit(value))])
  battery_discharge_hours <- as.data.frame(battery_discharge_hours)
  a <- diff(battery_discharge_hours$value)
  battery_discharge_hours <- battery_discharge_hours[-1,]
  battery_discharge_hours$value <- a
  battery_discharge_hours$value <- battery_discharge_hours$value * 1000.0 #W
  system_hourly <- rbind(system_hourly, battery_discharge_hours)
}

system_hourly <- hourlyMeans(systemData)
sl_write <- spread(system_hourly, id, value)
sl_write$timestamp <- as.POSIXct(paste(paste(sl_write$date, sl_write$timeUse), ":00:00",sep=""),
                                 format="%Y-%m-%d %H:%M:%S", tz="GMT")

#Replace NA values for charged and discharged energy with 0
sl_write$Charged_energy_kWh[is.na(sl_write$Charged_energy_kWh)] <- 0
sl_write$Discharged_energy_kWh[is.na(sl_write$Discharged_energy_kWh)] <- 0

sl_write <- sl_write[,-c(2,3)] #Remove date and timeUse
sl_write <- sl_write[, c(9,1:8)]
colnames(sl_write) <- c("timestamp","streetlight","Battery_power_W",
                        "Battery_watts_W", "Charged_energy_W",
                        "Discharged_energy_W", "PV_DC_coupled_W",
                        "PV_power_W", "State_of_charge_%")
setwd("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/")
write.csv(sl_write, file="SL_all_jun_mar.csv", row.names=FALSE)

###############################################################################
###Read in the system file
setwd("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/")
sl_all_rwanda <- read.csv("SL_all_jun_mar.csv", header=TRUE, stringsAsFactors = FALSE)
sl_all_rwanda$timestamp <- as.POSIXct(sl_all_rwanda$timestamp, tz="GMT", origin="1970-01-01")
sl_all_rwanda$date <- date(sl_all_rwanda$timestamp)
sl_all_rwanda$timeUse <- format(sl_all_rwanda$timestamp, format='%H')

######### For each streetlight see what check for missing data ####################
#Once the values are all read - analyse the data to get missing days and hours
#Data from 17 June 2019 to 23 March 2020 = 281 days
june_2019 <- seq(as.Date("2019-06-17"), as.Date("2019-06-30"), by="days")
july_2019 <- seq(as.Date("2019-07-01"), as.Date("2019-07-31"), by="days")
aug_2019 <- seq(as.Date("2019-08-01"), as.Date("2019-08-31"), by="days")
sep_2019 <- seq(as.Date("2019-09-01"), as.Date("2019-09-30"), by="days")
oct_2019 <- seq(as.Date("2019-10-01"), as.Date("2019-10-31"), by="days")
nov_2019 <- seq(as.Date("2019-11-01"), as.Date("2019-11-30"), by="days")
dec_2019 <- seq(as.Date("2019-12-01"), as.Date("2019-12-31"), by="days")
jan_2020 <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by="days")
feb_2020 <- seq(as.Date("2020-02-01"), as.Date("2020-02-29"), by="days")
mar_2020 <- seq(as.Date("2020-03-01"), as.Date("2020-03-23"), by="days")
all_days <- c(june_2019, july_2019, aug_2019, sep_2019, oct_2019, nov_2019, dec_2019,
              jan_2020, feb_2020, mar_2020)

#Create a vector for all 24 hours in a day
all_hours <- format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), 
                                by = "1 hour"), "%H", tz="GMT")
all_hours <- all_hours[-25]

#Number of on hours per day for each streetlight id and each variable
sl_rwanda_gather <- gather(sl_all_rwanda, "variable", "value", 3:9)
sl_on_hours <- sl_rwanda_gather %>%
  dplyr::group_by(streetlight, date, variable) %>%
  dplyr::summarise(onHours = length(na.omit(value)))
sl_on_hours <- as.data.frame(sl_on_hours)
#Once we have hours on for each available day, look for missing dates
sl_on_hours2 <- data.frame()
for(i in 1:length(unique(sl_on_hours$streetlight))) {
  df <- sl_on_hours[sl_on_hours$streetlight == unique(sl_on_hours$streetlight)[i], ]
  for(j in 1:length(unique(df$variable))) {
    df_sub <- df[df$variable == unique(df$variable)[j], ]
    for(k in 1:length(all_days)) {
      if(!(all_days[k] %in% df_sub$date)) {
        df_sub <- rbind(df_sub, data.frame(streetlight=df_sub$streetlight[1], date=all_days[k],
                                           variable=df_sub$variable[1], onHours=0))
      }
    }
    sl_on_hours2 <- rbind(sl_on_hours2, df_sub)
  }
}
sl_on_hours2 <- sl_on_hours2[order(sl_on_hours2$date),]
sl_on_hours2$month <- as.character(month(sl_on_hours2$date,label=TRUE, abbr=TRUE))

sl_on_hours2$date <- as.Date(sl_on_hours2$date)
sl_on_hours2$streetlight <- as.factor(sl_on_hours2$streetlight)
sl_on_hours2$variable <- as.factor(sl_on_hours2$variable)

#Plot yield map for yield - yield calculated as % hours of total per day i.e. 24
sl_on_hours2$yield <- sl_on_hours2$onHours * 100.0 / 24.0
sl_on_hours2$id2 <- paste(as.character(sl_on_hours2$streetlight), 
                          as.character(sl_on_hours2$variable))
library(wesanderson)
#Plotting a heat map for CPE
pal <- wes_palette("Zissou1", 100, type = "continuous")
sl_on_hours2[sl_on_hours2$streetlight=="SL1" | sl_on_hours2$streetlight=="SL2",] %>%
  ggplot(aes(date, id2)) + geom_tile(aes(fill = yield)) +
  scale_fill_gradientn(colours = pal) + 
  xlab("X axis") +
  ylab("Y axis") +
  labs(title="Yield for SL1 and SL2 data in Rwanda: 17 Jun'19 - 23 Mar'20" , 
       y="Parameter",
       x = "Day of study",
       fill="Yield") + 
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size=6),
        plot.title = element_text(size=10))

sl_on_hours2[sl_on_hours2$streetlight=="SL3" | sl_on_hours2$streetlight=="SL4",] %>%
  ggplot(aes(date, id2)) + geom_tile(aes(fill = yield)) +
  scale_fill_gradientn(colours = pal) + 
  xlab("X axis") +
  ylab("Y axis") +
  labs(title="Yield for SL3 and SL4 data in Rwanda: 17 Jun'19 - 23 Mar'20" , 
       y="Parameter",
       x = "Day of study",
       fill="Yield") + 
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size=6),
        plot.title = element_text(size=10))

###################################################################################################
#Hourly socket and system consumption for streetlight (1-4) in (month). 
#system and socket loads = Battery power - pv dc-coupled
sl_all_rwanda$day <- weekdays(sl_all_rwanda$date, abbr=TRUE)
sl_all_rwanda$month <- as.character(month(sl_all_rwanda$date, label=TRUE, abbr=TRUE))
sl_all_rwanda$sysSocketLoad <- -1*(sl_all_rwanda$Battery_power_W - sl_all_rwanda$Battery_watts_W)

sl_all_rwanda_weday <- sl_all_rwanda[sl_all_rwanda$day=="Sat" | sl_all_rwanda$day=="Sun", c(2,10,11,12,13,14)]
sl_all_rwanda_wday <- sl_all_rwanda[sl_all_rwanda$day!="Sat" & sl_all_rwanda$day!="Sun", c(2,10,11,12,13,14)]

plotMeans <- function(df) {
  df %>%
    ggplot(aes(x=timeUse, y=sysSocketLoad)) +
    geom_boxplot(aes(fill=timeUse))  +
    labs(y="Power consumption (W)",
         x = "Hour of day") + 
    theme(plot.title = element_text(size=10), legend.position = "none") 
}

####Plots for SL1
p <- plotMeans(sl_all_rwanda_wday[sl_all_rwanda_wday$streetlight=="SL1" & sl_all_rwanda_wday$month=="Jun",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekday in Jun'19") 

p <- plotMeans(sl_all_rwanda_weday[sl_all_rwanda_weday$streetlight=="SL1" & sl_all_rwanda_weday$month=="Jun",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekend day in Jun'19") 

p <- plotMeans(sl_all_rwanda_wday[sl_all_rwanda_wday$streetlight=="SL1" & sl_all_rwanda_wday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekday in Jul'19") 

p <- plotMeans(sl_all_rwanda_weday[sl_all_rwanda_weday$streetlight=="SL1" & sl_all_rwanda_weday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekend day in Jul'19") 

p <- plotMeans(sl_all_rwanda_wday[sl_all_rwanda_wday$streetlight=="SL1" & sl_all_rwanda_wday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekday in Aug'19") 

p <- plotMeans(sl_all_rwanda_weday[sl_all_rwanda_weday$streetlight=="SL1" & sl_all_rwanda_weday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekend day in Aug'19") 

p <- plotMeans(sl_all_rwanda_wday[sl_all_rwanda_wday$streetlight=="SL1" & sl_all_rwanda_wday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekday in Sep'19") 

p <- plotMeans(sl_all_rwanda_weday[sl_all_rwanda_weday$streetlight=="SL1" & sl_all_rwanda_weday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekend day in Sep'19") 

p <- plotMeans(sl_all_rwanda_wday[sl_all_rwanda_wday$streetlight=="SL1" & sl_all_rwanda_wday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekday in Oct'19") 

p <- plotMeans(sl_all_rwanda_weday[sl_all_rwanda_weday$streetlight=="SL1" & sl_all_rwanda_weday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekend day in Oct'19") 

p <- plotMeans(sl_all_rwanda_wday[sl_all_rwanda_wday$streetlight=="SL1" & sl_all_rwanda_wday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekday in Nov'19") 

p <- plotMeans(sl_all_rwanda_weday[sl_all_rwanda_weday$streetlight=="SL1" & sl_all_rwanda_weday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekend day in Nov'19") 

p <- plotMeans(sl_all_rwanda_wday[sl_all_rwanda_wday$streetlight=="SL1" & sl_all_rwanda_wday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekday in Dec'19") 

p <- plotMeans(sl_all_rwanda_weday[sl_all_rwanda_weday$streetlight=="SL1" & sl_all_rwanda_weday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekend day in Dec'19") 

p <- plotMeans(sl_all_rwanda_wday[sl_all_rwanda_wday$streetlight=="SL1" & sl_all_rwanda_wday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekday in Jan'20") 

p <- plotMeans(sl_all_rwanda_weday[sl_all_rwanda_weday$streetlight=="SL1" & sl_all_rwanda_weday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekend day in Jan'20") 

p <- plotMeans(sl_all_rwanda_wday[sl_all_rwanda_wday$streetlight=="SL1" & sl_all_rwanda_wday$month=="Feb",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekday in Feb'20") 

p <- plotMeans(sl_all_rwanda_weday[sl_all_rwanda_weday$streetlight=="SL1" & sl_all_rwanda_weday$month=="Feb",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekend day in Feb'20") 

p <- plotMeans(sl_all_rwanda_wday[sl_all_rwanda_wday$streetlight=="SL1" & sl_all_rwanda_wday$month=="Mar",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekday in Mar'20") 

p <- plotMeans(sl_all_rwanda_weday[sl_all_rwanda_weday$streetlight=="SL1" & sl_all_rwanda_weday$month=="Mar",])
p + labs(title="Mean hourly system and socket load at Rwanda SL1 on a weekend day in Mar'20") 