#******************************************************************************************#
# This is the script for pre-processing data from all Rwanda SL to convert to hourly data  #
# analyse yield of hourly data and explore imputation techniques                           #
# Author: K Bhargava                                                                       #
# Last updated on: 6th July 2020                                                          #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(mice) # for mice imputation
library(wesanderson)
library(timeSeries) # for converting data frame into time series
library("ggpubr")
library("imputeTS") # for na_seadec imputation
library(mgcv) # for gam model-based imputation
library(simputation) # For impute_knn, impute_rf
library(randomForest) #For random Forest functions used in the back end by impute_rf
library(missForest)
library(xts)
library(MLmetrics)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Define macros - theme for all plots
THEME <- theme(plot.title = element_text(size=12), legend.position = "bottom",
               legend.key.size = unit(0.5, "cm"), 
               legend.margin = margin(t=0,r=0,b=0,l=0), panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank(), panel.background = element_blank(), 
               axis.line = element_line(colour = "black"), axis.text = element_text(size=12), 
               axis.title = element_text(size=12)) 
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data"
plot_dir <- "Plots/Paper 7"
#******************************************************************************************#

#******************************************************************************************#
# Read SL and weather data between July to April
sl_all <- read.csv(here(filepath,"sl_all_raw.csv"), header = TRUE, stringsAsFactors = FALSE)
sl_all <- sl_all %>% 
  mutate(PV.power.W = Solar.Charger.PV.current * Solar.Charger.PV.voltage,
          Charged.energy.W = ifelse(System.overview.Battery.Power.W<0, 0, System.overview.Battery.Power.W),
          Discharged.energy.W = ifelse(System.overview.Battery.Power.W>0,0,System.overview.Battery.Power.W),
          timestamp = as.POSIXct(timestamp, origin="1970-01-01",tz="GMT"),
          date = date(timestamp), month=as.character(month(timestamp, label=TRUE, abbr=TRUE)), 
          timeUse = hour(timestamp)) 
sl_all <- sl_all[,-c(9:10)] # Remove PV current and PV voltage
sl_all <- distinct(sl_all)
# Subset data till 31st March 2020
sl_all <- sl_all[sl_all$date<="2020-03-31",]

weather_data <- read_csv(here(filepath,"weather_hourly_jul_mar.csv"), col_names = TRUE)
weather_data <- weather_data[,1:5]
weather_data <- gather(weather_data, "streetlight", "Potential_PV_power_W", 2:5)
weather_data$date <- date(weather_data$timestamp)
weather_data$timeUse <- hour(weather_data$timestamp)
weather_data <- weather_data[,-1] # Remove timestamp
weather_data <- weather_data[,c(1,3,4,2)] # Rearrange columns
weather_data <- weather_data[weather_data$date>="2019-07-01",] # Select data from 1st July
#******************************************************************************************#

#******************************************************************************************#
# Plot number of values per hour for each day
sl_qual <- sl_all[,c(1,2,15,17,7,9,10,12)]
colnames(sl_qual) <- c(colnames(sl_qual)[1:4],"Solar battery power", "AC load", "Battery power",
                       "PV power")
sl_qual <- gather(sl_qual, "id", "value", 5:8)
sl_qual <- sl_qual %>% group_by(streetlight, date, timeUse, id) %>% 
  summarise(count = length(unique(timestamp)))
sl_qual <- as.data.frame(sl_qual)
sl_qual <- sl_qual %>% mutate(count2 = ifelse(count<100, count, 100),
                              yield=ifelse(date>="2019-08-05" & timeUse>=16, count*100/60, count*100/4),
                              yield2=ifelse(yield>100, 100, yield))
pal <- wes_palette("Zissou1", 100, type = "continuous")
ggplot(sl_qual[sl_qual$id=="Battery power",], aes(date, timeUse)) + facet_wrap(~streetlight, nrow=2) + geom_tile(aes(fill = yield2)) + 
  scale_fill_gradientn(colours = pal, breaks=c(0,25,50,75,100)) + 
  scale_y_continuous(breaks=seq(0,24,by=4)) + xlab("X axis") + ylab("Y axis") + 
  labs(y="Time of day", x = "Day of study", fill="Yield (%)",
       title="Yield per hour for Rwanda streetlights: 1 Jul 2019 - 31 Mar 2020") + THEME + 
  guides(fill = guide_colorbar(barwidth = 15, barheight = 0.5))
ggsave(here(plot_dir,"yield_hourly_all.png"))
#******************************************************************************************#

#******************************************************************************************#
# Convert data into hourly mean, fill in NA for all days missing, add Potential PV data and save
sl_all <- gather(sl_all, "id", "value", 3:14)
system_hourly <- sl_all %>% group_by(streetlight, date, timeUse, id) %>%
  summarise(value = mean(value, na.rm = TRUE))
system_hourly <- as.data.frame(system_hourly)
system_hourly <- spread(system_hourly, id, value)
system_hourly[is.na(system_hourly)] <- NA

# For each streetlight see what check for missing data - 1 Jul'19 to 31 Mar'20 = 275 days
all_days <- seq(as.Date("2019-07-01"), as.Date("2020-03-31"), by="days")
all_hours <- format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1),by = "1 hour"), "%H", tz="GMT")
all_hours <- as.numeric(all_hours[-25])

# Create a complete data set for all days - add NA for all missing values
data <- data.frame()
for(i in seq_along(all_days)) {
  data <- rbind(data, data.frame(date = rep(all_days[i], length(all_hours)), timeUse=all_hours))
}
data <- data %>% mutate(sl1 = "SL1", sl2 = "SL2", sl3 = "SL3", sl4 = "SL4")
data <- gather(data, "id", "streetlight", 3:6)
data <- data[,-3] #remove column id
data <- data[,c(3,1,2)] # rearrange columns
data <- data %>% mutate(`Battery.Monitor.State.of.charge..` = NA, `Battery.Monitor.Voltage.V` = NA, 
                        Charged.energy.W = NA, Discharged.energy.W = NA, Inverter.Output.current.A=NA, 
                        Inverter.Output.voltage.V=NA, PV.power.W=NA,Solar.Charger.Battery.watts.W=NA, 
                        Solar.Charger.Load.current.A=NA,System.overview.AC.Consumption.L1.W=NA, 
                        System.overview.Battery.Power.W=NA,System.overview.PV...DC.coupled.W=NA)

# Include data from data to get all days not there in system_hourly
system_hourly <- system_hourly %>% mutate(id = paste(streetlight,date,timeUse, sep=" ")) 
data <- data %>% mutate(id=paste(streetlight,date,timeUse, sep=" "))
system_hourly <- rbind(system_hourly, data[!(data$id %in% system_hourly$id),])
system_hourly <- system_hourly[order(system_hourly$streetlight,system_hourly$date,system_hourly$timeUse),]

# Add in weather data - weather data unavailable for April
weather_data <- weather_data %>% mutate(id=paste(streetlight,date,timeUse, sep=" "))
weather_data <- weather_data[,-c(1:3)] # Remove streetlight, date and time
system_hourly <- merge(system_hourly, weather_data, by="id")
system_hourly <- system_hourly[,-1] # Remove id
system_hourly <- system_hourly[order(system_hourly$streetlight,system_hourly$date,system_hourly$timeUse),]
write.csv(system_hourly, file=here(filepath,"raw_hourly_sl_data.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
library(tidyverse)
library(lubridate)
library(wesanderson)
library("imputeTS") # for na_seadec imputation
library(xts)
library(here)

filepath <- "Data"
plot_dir <- "Plots/Paper 7"

# Read hourly data and calculate yield
system_hourly <- read.csv(here(filepath,"raw_hourly_sl_data.csv"), header = TRUE, stringsAsFactors=FALSE)
system_hourly <- system_hourly %>% mutate(date = as.Date(date))

# Get summary of the data to get number of NA values for each var
summary(system_hourly) # Roughly 3K values missing for all variables except AC load for which 14K missing
# Get percentage missing data for each variable for each SL
system <- gather(system_hourly,"id","value",4:16)
missingData <- system %>% group_by(streetlight, id) %>%
  summarise(missingPercent = sum(is.na(value))*100/length(value))  
missingData <- spread(missingData, id, missingPercent)
write.csv(missingData, file=here(filepath,"missing_sl_data.csv"), row.names=FALSE)

# Calculate yield as % hours on during a day
sl_on_hours <- system %>% group_by(streetlight, date, id) %>%
  summarise(yield = length(na.omit(value)) * 100.0 / 24.0 )
sl_on_hours <- as.data.frame(sl_on_hours)

pal <- wes_palette("Zissou1", 100, type = "continuous")
ggplot(sl_on_hours[sl_on_hours$id=="PV.power.W",], aes(date, streetlight)) + geom_tile(aes(fill = yield)) +
  scale_fill_gradientn(colours = pal) + xlab("X axis") + ylab("Y axis") +
  labs(title="Yield for Rwanda SL: 1 Jul'19 - 30 Apr'20",y="Streetlight",x = "Day of study",fill="Yield")
ggsave(here(plot_dir,"yield_raw.png"))

ggplot(sl_on_hours, aes(date, id)) + facet_wrap(~streetlight) +  geom_tile(aes(fill = yield)) +
  scale_fill_gradientn(colours = pal) + xlab("X axis") + ylab("Y axis") +
  labs(title="Yield for Rwanda SL: 1 Jul'19 - 30 Apr'20", y="Variable",x ="Day of study",fill="Yield") 
ggsave(here(plot_dir,"yield_sl_all.png"))

# Calculate hourly yield for each day and SL after summarising to hourly means
sl_on_hours2 <- system %>% group_by(streetlight, date, timeUse, id) %>%
  summarise(yield = length(na.omit(value)) * 100.0)
sl_on_hours2 <- as.data.frame(sl_on_hours2)

ggplot(sl_on_hours2[sl_on_hours2$id=="PV.power.W",], aes(date, timeUse)) + facet_wrap(~streetlight) +
  geom_tile(aes(fill = yield)) + scale_fill_gradientn(colours = pal) + xlab("X axis") + ylab("Y axis") +
  labs(title="Yield of hourly data for PV power for Rwanda SL: 1 Jul'19 - 30 Apr'20",
       y="Time of day",x = "Day of study",fill="Yield") + scale_y_continuous(limits=c(0,23),breaks=0:23)
ggsave(here(plot_dir,"hourly_yield_sl_all.png"))
ggplot(sl_on_hours2[sl_on_hours2$id=="System.overview.AC.Consumption.L1.W",], aes(date, timeUse)) + 
  facet_wrap(~streetlight) + geom_tile(aes(fill = yield)) + scale_fill_gradientn(colours = pal) + xlab("X axis") + ylab("Y axis") +
  labs(title="Yield of hourly data for AC consumption for Rwanda SL: 1 Jul'19 - 30 Apr'20",
       y="Time of day",x = "Day of study",fill="Yield") + scale_y_continuous(limits=c(0,23),breaks=0:23)
ggsave(here(plot_dir,"hourly_yield_ACLoad_sl_all.png"))
#******************************************************************************************#

#******************************************************************************************#
# Get rows where NA values are a certain proportion of variables
system_hourly$na_count <- apply(system_hourly, 1, function(x) sum(is.na(x)))
# How many have more than 3 variables missing (3 account for inverter and AC consumption data)
sum(system_hourly$na_count>3) #3486*100/26400 = 13% rows
#******************************************************************************************#

#******************************************************************************************#
# Get days for which more than 50% data is missing i.e. yield is less than 50%
# Cut off of 50% may imply lights could not be turned on coz of low battery power and hence power outage
# More than 50% data loss may imply other failures
sl_all_50 <- sl_on_hours[sl_on_hours$id=="PV.power.W" & sl_on_hours$yield<50,] 
sl_all_50 <- sl_all_50 %>% mutate(month = as.character(month(date, label=TRUE, abbr=TRUE)))
# Count the number of days per SL - Sl1 43 days, SL2 23 days and SL4 19 days - see if these need to be removed
sl_all_50_count <- sl_all_50 %>% group_by(streetlight) %>% summarise(count=length(yield))
# Count days with more than 50% data missing per month
sl_all_50_monthly <- sl_all_50 %>% group_by(streetlight, month) %>% summarise(count=length(yield))

# Get full days per month
sl_all_PV_100 <- sl_on_hours[sl_on_hours$id=="PV.power.W" & sl_on_hours$yield==100,] 
sl_all_PV_100 <- sl_all_PV_100 %>% mutate(month = as.character(month(date, label=TRUE, abbr=TRUE)))
sl_all_ACLoad_100 <- sl_on_hours[sl_on_hours$id=="System.overview.AC.Consumption.L1.W" & sl_on_hours$yield==100,] 
sl_all_ACLoad_100 <- sl_all_ACLoad_100 %>% mutate(month = as.character(month(date, label=TRUE, abbr=TRUE)))
# Count full days per SL for each month
sl_all_PV_100_count <- sl_all_PV_100 %>% group_by(streetlight,month) %>% summarise(count=length(yield))
sl_all_ACLoad_100_count <- sl_all_ACLoad_100 %>% group_by(streetlight,month) %>% summarise(count=length(yield))
#******************************************************************************************#

#******************************************************************************************#
# Get typical hourly data and see how it varies over time
system_typical <- system %>% group_by(streetlight, timeUse, id) %>% summarise(value = mean(value, na.rm=TRUE))
system_typical <- as.data.frame(system_typical)
system_typical <- system_typical[system_typical$id=="Potential_PV_power_W" | system_typical$id=="PV.power.W" | 
  system_typical$id=="Charged.energy.W" | system_typical$id=="Solar.Charger.Battery.watts.W" | 
  system_typical$id=="System.overview.Battery.Power.W" | system_typical$id=="Discharged.energy.W" |
  system_typical$id=="System.overview.AC.Consumption.L1.W" ,]

plotTypical <- function(df) {
  ggplot(df, aes(timeUse, abs(value), color=factor(id))) + geom_line(aes(linetype=factor(id))) + 
    labs(x="Time of day",y="Energy (Wh)",color="Variable",linetype="Variable") + 
    scale_linetype_discrete(labels=c("Charged energy","Discharged energy","Potential PV power","PV power",
                                     "Solar Charger Battery Power","AC consumption","System Battery Power"))+
    scale_color_discrete(labels=c("Charged energy","Discharged energy","Potential PV power","PV power",
                                  "Solar Charger Battery Power","AC consumption","System Battery Power")) +
    theme(legend.position = "bottom") +
    scale_x_continuous(breaks = 0:23) +  scale_y_continuous(breaks=seq(0,225,25))
}

plotTypical(system_typical) + facet_wrap(~streetlight) +
  labs(title="Typical day values for Rwanda SL: 01 Jul'19 to 31 Mar'20") 
ggsave(here(plot_dir,"typical_day_sl_all.png"))
#******************************************************************************************#

#******************************************************************************************#
# Plot hourly data to see trend and seasonality - seasonality in data is 1 day as expected
# Add timestamp and month to the data
system <- system %>% mutate(month = as.character(month(date, label=TRUE, abbr=TRUE)),
  timestamp = as.POSIXct(paste(date, ifelse(timeUse<10, paste("0",timeUse,":00:00",sep=""), 
                                  paste(timeUse,":00:00",sep="")), sep=" "), origin="1970-01-01",tz="GMT"),
  month2 = factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                  labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))

# Plot hourly values against time
subSystem <- system[system$streetlight=="SL1",]
ggplot(subSystem[(subSystem$id=="PV.power.W" | subSystem$id=="Potential_PV_power_W" |
                    subSystem$id=="System.overview.Battery.Power.W" |
                    subSystem$id=="Solar.Charger.Battery.watts.W") & subSystem$month=="Oct",], aes(timestamp, value)) + 
  facet_wrap(~id) + geom_line() + labs(x="Date", y="Energy (Wh)", 
       title = "Energy profile of SL1 in Rwanda in Oct 2019")

ggplot(subSystem[(subSystem$id=="PV.power.W" | subSystem$id=="Potential_PV_power_W" | 
                  subSystem$id=="System.overview.Battery.Power.W"),], 
       aes(timestamp, value, color=as.factor(id))) + facet_wrap(~month2, scales = "free") + 
  geom_line(aes(linetype=factor(id))) + theme(legend.position = "bottom") + 
  labs(x="Day of study", y="Energy (Wh)", color="Variable", linetype="Variable",
       title = "Energy profile of SL1 in Rwanda: 01 Jul'19 to 30 Apr'20")
ggsave(here(plot_dir,"energy_profile_sl1.png"))

subSystem <- system[system$streetlight=="SL2",]
ggplot(subSystem[(subSystem$id=="PV.power.W" | subSystem$id=="Potential_PV_power_W" | 
                    subSystem$id=="System.overview.Battery.Power.W"),], 
       aes(timestamp, value, color=as.factor(id))) + facet_wrap(~month2, scales = "free") + 
  geom_line(aes(linetype=factor(id))) + theme(legend.position = "bottom") + 
  labs(x="Day of study", y="Energy (Wh)", color="Variable", linetype="Variable",
       title = "Energy profile of SL2 in Rwanda: 01 Jul'19 to 30 Apr'20")
ggsave(here(plot_dir,"energy_profile_sl2.png"))

subSystem <- system[system$streetlight=="SL3",]
ggplot(subSystem[(subSystem$id=="PV.power.W" | subSystem$id=="Potential_PV_power_W" | 
                    subSystem$id=="System.overview.Battery.Power.W"),], 
       aes(timestamp, value, color=as.factor(id))) + facet_wrap(~month2, scales = "free") + 
  geom_line(aes(linetype=factor(id))) + theme(legend.position = "bottom") + 
  labs(x="Day of study", y="Energy (Wh)", color="Variable", linetype="Variable",
       title = "Energy profile of SL3 in Rwanda: 01 Jul'19 to 30 Apr'20")
ggsave(here(plot_dir,"energy_profile_sl3.png"))

subSystem <- system[system$streetlight=="SL4",]
ggplot(subSystem[(subSystem$id=="PV.power.W" | subSystem$id=="Potential_PV_power_W" | 
                    subSystem$id=="System.overview.Battery.Power.W"),], 
       aes(timestamp, value, color=as.factor(id))) + facet_wrap(~month2, scales = "free") + 
  geom_line(aes(linetype=factor(id))) + theme(legend.position = "bottom") + 
  labs(x="Day of study", y="Energy (Wh)", color="Variable", linetype="Variable",
       title = "Energy profile of SL4 in Rwanda: 01 Jul'19 to 30 Apr'20")
ggsave(here(plot_dir,"energy_profile_sl4.png"))
#******************************************************************************************#

#******************************************************************************************#
# Box plots 
ggplot(system[system$id=="PV.power.W",], aes(as.factor(timeUse), abs(value))) + 
  facet_wrap(~streetlight) + geom_boxplot() + 
  labs(x="Time of day", y="Energy (Wh)", title = "PV power at Rwanda SL: 01 Jul'19 to 30 Apr'20")
ggsave(here(plot_dir,"pv_power.png"))

ggplot(system[system$id=="Charged.energy.W",], aes(as.factor(timeUse), abs(value))) + 
  facet_wrap(~streetlight) + geom_boxplot() + 
  labs(x="Time of day", y="Energy (Wh)", title = "Charged energy at Rwanda SL: 01 Jul'19 to 30 Apr'20")
ggsave(here(plot_dir,"charged_energy.png"))

ggplot(system[system$id=="Discharged.energy.W",], aes(as.factor(timeUse), abs(value))) + 
  facet_wrap(~streetlight) + geom_boxplot() + 
  labs(x="Time of day", y="Energy (Wh)", title = "Discharged energy at Rwanda SL: 01 Jul'19 to 30 Apr'20")
ggsave(here(plot_dir,"discharged_energy.png"))

ggplot(system[system$id=="System.overview.AC.Consumption.L1.W",], aes(as.factor(timeUse), abs(value))) + 
  facet_wrap(~streetlight) + geom_boxplot() + 
  labs(x="Time of day", y="Energy (Wh)", title = "AC consumption at Rwanda SL: 01 Jul'19 to 30 Apr'20")
ggsave(here(plot_dir,"ac_consumption.png"))  
#******************************************************************************************#

#******************************************************************************************#
# Get full data days for all SL
df <- system_hourly[,c(1,2,3,5,10,11,12,13,14,16,17)]
df <- df[df$na_count==0,-11]
sl_on_hours <- df %>% group_by(streetlight, date) %>% summarise(onHours = length(PV.power.W))
sl_on_hours <- as.data.frame(sl_on_hours[sl_on_hours$onHours==24,])
# Consider all full days
full_data <- data.frame()
for(i in seq_along(unique(sl_on_hours$streetlight))) {
  x <- df[df$streetlight == unique(sl_on_hours$streetlight)[i],]
  days <- sl_on_hours$date[sl_on_hours$streetlight == unique(sl_on_hours$streetlight)[i]]
  x <- x[x$date %in% days,]
  full_data <- rbind(full_data, x)
}

# Create a copy of full data and replace 20% data with NA - in system hourly we have 13% data missing for all
# variables except 24% for AC consumption
incomplete_data <- data.frame()
for(i in seq_along(unique(full_data$streetlight))) {
  df_sub <- full_data[full_data$streetlight == unique(full_data$streetlight)[i],]
  n <- length(df_sub$streetlight)
  ind <- sample( c(1:n), floor(n/5))
  df_sub[ind,c("Battery.Monitor.Voltage.V","PV.power.W","Solar.Charger.Battery.watts.W","Solar.Charger.Load.current.A",
        "System.overview.AC.Consumption.L1.W","System.overview.Battery.Power.W")] <- NA
  incomplete_data <- rbind(incomplete_data, df_sub)
}
#******************************************************************************************#

# Try different imputation techniques and compare performance using RMSE and MAPE metrics

#******************************************************************************************#
# Imputation using na_seadec owing to seasonality - works on univariate time series
methodImpute <- c("random", "mean", "locf", "interpolation", "ma", "kalman")
# Impute missing values for  Voltage, PV power, Load current, AC consumption, Battery power
variables <- c("Battery.Monitor.Voltage.V","PV.power.W","Solar.Charger.Battery.watts.W","Solar.Charger.Load.current.A",
               "System.overview.AC.Consumption.L1.W","System.overview.Battery.Power.W")
na_seadec_imputedData <- data.frame()
for(k in seq_along(variables)) {
  x <- incomplete_data[c("streetlight","date","timeUse",variables[k])]
  for(i in seq_along(unique(x$streetlight))) {
    df <- x[x$streetlight == unique(x$streetlight)[i], ]
    
    # Convert data frame into a time series using xts to serve as input to na_seadec function
    # For this data, seasonality is 1 day with a reading every hour
    df.ts <- df[,-1]
    df.ts <- spread(df.ts, timeUse, variables[k])
    df.ts <- xts(df.ts[,-1], order.by=as.Date(df.ts[,1], "%Y-%m-%d"))
    
    # Impute data using different functions of na_seadec and bind to df
    for(j in seq_along(methodImpute)) {
      df1 <- as.data.frame(na_seadec(df.ts, algorithm=methodImpute[j],find_frequency=TRUE))
      df1 <- df1 %>% mutate(date=row.names(df1))
      df1 <- gather(df1, "timeUse", "value", 1:24)
      df1[is.na(df1)] <- 0
      df1 <- df1[order(df1$date),]
      df <- cbind(df, df1$value)
    }
    colnames(df) <- c(colnames(df)[1:3],paste(variables[k],"original",sep="_"),
                      paste(variables[k],methodImpute,sep="_"))
    df <- gather(df, "variable","value",4:10)
    
    # Bind data for all SL
    na_seadec_imputedData <- rbind(na_seadec_imputedData, df)
  }
}
na_seadec_imputedData <- spread(na_seadec_imputedData, variable, value)
na_seadec_imputedData <- na_seadec_imputedData %>% 
  mutate(Potential.PV.power.W = incomplete_data$Potential_PV_power_W,
         month = as.character(lubridate::month(date, label=TRUE, abbr=TRUE)),
         timestamp = as.POSIXct(paste(date, ifelse(timeUse<10, paste("0",timeUse,":00:00",sep=""), 
                                                   paste(timeUse,":00:00",sep="")), sep=" "), origin="1970-01-01",tz="GMT"),
         month2 = factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                         labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))
write.csv(na_seadec_imputedData, file=here(filepath,"na_seadec_test_imputed_data.csv"), row.names=FALSE)

# Calculating RMSE to get performance of na_seadec approach for each SL
na_seadec_imputedData <- na_seadec_imputedData %>% 
  mutate(Battery.Monitor.Voltage.V = full_data$Battery.Monitor.Voltage.V,
         PV.power.W = full_data$PV.power.W,
         Solar.Charger.Battery.watts.W = full_data$Solar.Charger.Battery.watts.W,
         Solar.Charger.Load.current.A = full_data$Solar.Charger.Load.current.A,
         System.overview.AC.Consumption.L1.W = full_data$System.overview.AC.Consumption.L1.W,
         System.overview.Battery.Power.W = full_data$System.overview.Battery.Power.W)
# Subset data where original values are na
na_seadec_sub <- na_seadec_imputedData[is.na(na_seadec_imputedData$Battery.Monitor.Voltage.V_original),]
# Performance considering the sub set
perf_na_seadec_sub <- na_seadec_sub %>% group_by(streetlight) %>%
  summarise_at(vars(matches("Battery.Monitor.Voltage.V_")), ~RMSE(.x, Battery.Monitor.Voltage.V)) %>% 
  bind_cols(na_seadec_sub %>% group_by(streetlight) %>% 
              summarise_at(vars(matches("PV.power.W_")), ~RMSE(.x, PV.power.W))) %>%
  bind_cols(na_seadec_sub %>% group_by(streetlight) %>% 
              summarise_at(vars(matches("Solar.Charger.Battery.watts.W_")), ~RMSE(.x, Solar.Charger.Battery.watts.W))) %>%
  bind_cols(na_seadec_sub %>% group_by(streetlight) %>% 
              summarise_at(vars(matches("Solar.Charger.Load.current.A_")), ~RMSE(.x, Solar.Charger.Load.current.A))) %>% 
  bind_cols(na_seadec_sub %>% group_by(streetlight) %>% 
              summarise_at(vars(matches("System.overview.AC.Consumption.L1.W_")), ~RMSE(.x, System.overview.AC.Consumption.L1.W))) %>%
  bind_cols(na_seadec_sub %>% group_by(streetlight) %>% 
              summarise_at(vars(matches("System.overview.Battery.Power.W_")), ~RMSE(.x, System.overview.Battery.Power.W)))
# Remove streetlight columns that are redundant and original data
perf_na_seadec_sub <- perf_na_seadec_sub[,-c(7,9,15,17,23,25,31,33,39,41,47)]
perf_na_seadec_sub <- gather(perf_na_seadec_sub, id, value, 2:37)
ggplot(perf_na_seadec_sub[perf_na_seadec_sub$id %in% unique(perf_na_seadec_sub$id)[c(1:6)],], aes(id, value)) + facet_wrap(~streetlight) + 
  geom_bar(stat="identity", width=.3, position = "dodge") + theme(axis.text.x = element_text(angle=45)) +
  scale_x_discrete(labels=c("Interpolation","Kalman","LOCF","MA","Mean",
                            "Random")) + labs(y="RMSE (V)",x="na_seadec approach", title="RMSE for Voltage using na_seadec")
ggsave(here(plot_dir,"rmse_seadec1.png")) 
ggplot(perf_na_seadec_sub[perf_na_seadec_sub$id %in% unique(perf_na_seadec_sub$id)[c(19:24)],], aes(id, value)) + facet_wrap(~streetlight) + 
  geom_bar(stat="identity", width=.3, position = "dodge") + theme(axis.text.x = element_text(angle=45)) +
  scale_x_discrete(labels=c("Interpolation","Kalman","LOCF","MA","Mean",
                            "Random")) + labs(y="RMSE (A)",x="na_seadec approach", title="RMSE for Load current using na_seadec")
ggsave(here(plot_dir,"rmse_seadec2.png")) 
ggplot(perf_na_seadec_sub[perf_na_seadec_sub$id %in% unique(perf_na_seadec_sub$id)[c(7:18,25:36)],], 
       aes(id, value, fill=id)) + theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  facet_wrap(~streetlight) + labs(x="Variable", y="RMSE (W)", title="RMSE using na_seadec approach") +
  geom_bar(stat="identity", width=.3, position = "dodge") + 
  scale_x_discrete(labels=c("PV_power_interpolation","PV_power_kalman","PV_power_LOCF","PV_power_ma",
                            "PV_power_mean","PV_power_random","Solar_battery_power_interpolation",
                            "Solar_battery_power_kalman","Solar_battery_power_LOCF","Solar_battery_power_ma",
                            "Solar_battery_power_mean","Solar_battery_power_random","AC_load_interpolation",
                            "AC_load_kalman","AC_load_LOCF","AC_load_ma","AC_load_mean","AC_load_random",
                            "System_battery_power_interpolation","System_battery_power_kalman",
                            "System_battery_power_LOCF","System_battery_power_ma",
                            "System_battery_power_mean","System_battery_power_random"))
ggsave(here(plot_dir,"rmse_seadec3.png")) 

# To conclude na_seadec approach is suitable for Battery Monitor Voltage, Solar Chager Load Current and 
# System AC consumption - makes sense as these values are seasonal each day due to constant behaviour
# Use kalman approach
perf_na_seadec2 <- perf_na_seadec_sub[!(perf_na_seadec_sub$id=="PV.power.W_interpolation" | 
                                          perf_na_seadec_sub$id=="PV.power.W_ma" | 
                                          perf_na_seadec_sub$id=="PV.power.W_kalman" | perf_na_seadec_sub$id=="PV.power.W_locf" | 
                                          perf_na_seadec_sub$id=="PV.power.W_mean" | perf_na_seadec_sub$id=="PV.power.W_random" | 
                                          perf_na_seadec_sub$id=="System.overview.Battery.Power.W_interpolation" | 
                                          perf_na_seadec_sub$id=="System.overview.Battery.Power.W_kalman" | 
                                          perf_na_seadec_sub$id=="System.overview.Battery.Power.W_locf" | 
                                          perf_na_seadec_sub$id=="System.overview.Battery.Power.W_ma" | 
                                          perf_na_seadec_sub$id=="System.overview.Battery.Power.W_mean" | 
                                          perf_na_seadec_sub$id=="System.overview.Battery.Power.W_random"),]
ggplot(perf_na_seadec2, aes(id, value)) + facet_wrap(~streetlight) +
  geom_bar(stat="identity", width=.3, position = "dodge") + theme(axis.text.x = element_text(angle=90))

# Compute statistics for original and imputed data
na_seadec_sub <- gather(na_seadec_sub, id, value, c(4:46,50:55))
stats_na_seadec_sub <- na_seadec_sub %>% group_by(streetlight, id) %>%
  summarise(mean = mean(value, na.rm=TRUE), median = median(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE),
            skew = skewness(value, na.rm=TRUE), kurt = kurtosis(value, na.rm=TRUE))
stats_na_seadec_sub <- as.data.frame(stats_na_seadec_sub)  
stats_na_seadec_sub <- stats_na_seadec_sub[complete.cases(stats_na_seadec_sub),]
stats_na_seadec_sub <- stats_na_seadec_sub %>% mutate(mean=round(mean,2), median=round(median,2),
                                                      sd=round(sd,2), skew=round(skew,2), kurt=round(kurt,2))
write.table(stats_na_seadec_sub, file = here(filepath,"stats_na_seadec.txt"), sep = ",", quote = FALSE, row.names = F)

# Plot data to check mapping
ggplot(na_seadec_sub[na_seadec_sub$streetlight=="SL1" & na_seadec_sub$id==unique(na_seadec_sub$id)[c(2,37)],], 
       aes(timestamp, value, color=id)) + geom_line() + theme(legend.position = "bottom")
#******************************************************************************************#

#******************************************************************************************#

#******************************************************************************************#
# Impute PV values using models based on correlation - understanding correlation between variables
cor_sl1 <- symnum(cor(system_hourly[system_hourly$streetlight=="SL1",c(5,10,12,13,14,16)], use="complete.obs"))
cor_sl2 <- symnum(cor(system_hourly[system_hourly$streetlight=="SL2",c(5,10,12,13,14,16)], use="complete.obs"))
cor_sl3 <- symnum(cor(system_hourly[system_hourly$streetlight=="SL3",c(5,10,12,13,14,16)], use="complete.obs"))
cor_sl4 <- symnum(cor(system_hourly[system_hourly$streetlight=="SL4",c(5,10,12,13,14,16)], use="complete.obs"))

# Potential PV power can be used to model PV power and Battery power
# It is only weakly correlated to voltage and not correlated to voltage, current and ac load
ggplot(system_hourly, aes(Potential_PV_power_W,PV.power.W)) + facet_wrap(~streetlight) +
  geom_point() + geom_smooth(method = "auto") + 
  labs(x = "Potential PV power (W)" , y= "PV power (W)", title="PV power ~ Potential PV power")
ggsave(here(plot_dir,"gam1.png")) 
ggplot(system_hourly, aes(Potential_PV_power_W,System.overview.Battery.Power.W)) + facet_wrap(~streetlight) +
  geom_point() + geom_smooth(method = "auto") + 
  labs(x = "Potential PV power (W)" , y= "Battery power (W)", title="Battery power ~ Potential PV power")
ggsave(here(plot_dir,"gam2.png")) 

# Impute values using linear and non linear models with the function gam
variables <- c("PV.power.W", "System.overview.Battery.Power.W")
gam_imputedData <- data.frame()
for(k in seq_along(variables)) {
  x <- incomplete_data[c("streetlight","date","timeUse","Potential_PV_power_W",variables[k])]
  for(i in seq_along(unique(x$streetlight))) {
    df <- x[x$streetlight == unique(x$streetlight)[i], ]
    df <- df %>% mutate(mod_lm=df[,5], mod_gam=df[,5])
    
    if(k==1) {
      mod_lm <- gam(PV.power.W ~ Potential_PV_power_W, data=df) # Linear fit
      mod_gam <- gam(PV.power.W ~ s(Potential_PV_power_W, bs="cs"), data=df) # Non-linear fit
    } else if(k==2) {
      mod_lm <- gam(System.overview.Battery.Power.W ~ Potential_PV_power_W, data=df) # Linear fit
      mod_gam <- gam(System.overview.Battery.Power.W ~ s(Potential_PV_power_W, bs="cs"), data=df) # Non-linear fit
    }
    
    # Impute values using linear and non linear fit
    a <- predict(mod_lm, newdata=df[!complete.cases(df),1:5], type='response', se=F)
    df$mod_lm[as.numeric(row.names(a))] <- a
    
    a <- predict(mod_gam, newdata=df[!complete.cases(df),1:5], type='response', se=F)
    df$mod_gam[as.numeric(row.names(a))] <- a
    
    colnames(df) <- c(colnames(df)[1:4],paste(variables[k],"original",sep="_"),
                      paste(variables[k],"mod_lm",sep="_"), paste(variables[k],"mod_gam",sep="_"))
    df <- gather(df, "variable","value",5:7)
    
    # Bind data for all SL
    gam_imputedData <- rbind(gam_imputedData, df)
  }
}
gam_imputedData <- spread(gam_imputedData, variable, value)
gam_imputedData <- gam_imputedData %>% 
  mutate(month = as.character(lubridate::month(date, label=TRUE, abbr=TRUE)),
         timestamp = as.POSIXct(paste(date, ifelse(timeUse<10, paste("0",timeUse,":00:00",sep=""), 
                                                   paste(timeUse,":00:00",sep="")), sep=" "), origin="1970-01-01",tz="GMT"),
         month2 = factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                         labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))
write.csv(gam_imputedData, file=here(filepath,"gam_imputed_test_data.csv"), row.names=FALSE)

# Calculating RMSE to get performance of na_seadec approach for each SL
gam_imputedData <- gam_imputedData %>% mutate(PV.power.W = full_data$PV.power.W,
         System.overview.Battery.Power.W = full_data$System.overview.Battery.Power.W)
# Subset data where original values are na
gam_sub <- gam_imputedData[is.na(gam_imputedData$PV.power.W_original),]
# Performance considering the sub set
perf_gam_sub <- gam_sub %>% group_by(streetlight) %>%
  summarise_at(vars(matches("System.overview.Battery.Power.W_")), ~RMSE(System.overview.Battery.Power.W, .x)) %>% 
  bind_cols(gam_sub %>% group_by(streetlight) %>% 
              summarise_at(vars(matches("PV.power.W_")), ~RMSE(PV.power.W, .x))) 
# Remove streetlight columns that are redundant and original data
perf_gam_sub <- perf_gam_sub[,-c(4,5,8)]
perf_gam_sub <- gather(perf_gam_sub, id, value, 2:5)
ggplot(perf_gam_sub, aes(id, value)) + facet_wrap(~streetlight) +
  geom_bar(stat="identity", width=.3, position = "dodge") + theme(axis.text.x = element_text(angle=30)) + 
  scale_x_discrete(labels=c("PV.power_GAM", "PV.power_LM", "Battery.power_GAM", "Battery.power_LM")) + 
  labs(x="Variables", y="RMSE (W)", title="RMSE using LM and GAM models")
ggsave(here(plot_dir,"rmse_gam.png")) 

# Compute statistics for original and imputed data
gam_sub <- gather(gam_sub, id, value, c(5:10,14:15))
stats_gam_sub <- gam_sub %>% group_by(streetlight, id) %>%
  summarise(mean = mean(value, na.rm=TRUE), median = median(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE),
            skew = skewness(value, na.rm=TRUE), kurt = kurtosis(value, na.rm=TRUE))
stats_gam_sub <- as.data.frame(stats_gam_sub)  
stats_gam_sub <- stats_gam_sub[complete.cases(stats_gam_sub),]
stats_gam_sub <- stats_gam_sub %>% mutate(mean=round(mean,2), median=round(median,2),
                                                      sd=round(sd,2), skew=round(skew,2), kurt=round(kurt,2))
write.table(stats_gam_sub, file = here(filepath,"stats_gam.txt"), sep = ",", quote = FALSE, row.names = F)

# Plot data to check mapping
ggplot(gam_sub[gam_sub$streetlight=="SL1" & gam_sub$id==unique(gam_sub$id)[c(1,7)],], 
       aes(timestamp, value, color=id)) + geom_line() + theme(legend.position = "bottom")

# To conclude stats much better when using na_seadec for both PV power and battery power
#******************************************************************************************#

#******************************************************************************************#
# Alternative imputation techniques
# Impute using hot-deck approach using kNN and random forest
df <- imputedPVPower2[,c(1,2,3,4,12,13)]
df <- df %>% mutate(knn1 = impute_knn(df, PV.power.W ~ Potential.PV.power.W|streetlight)$PV.power.W,
                    knn2 = impute_knn(df, PV.power.W ~ Potential.PV.power.W+timeUse|streetlight)$PV.power.W,
                    knn3 = impute_knn(df, PV.power.W ~ Potential.PV.power.W+timeUse+month|streetlight)$PV.power.W,
                    rf1 = impute_rf(df, PV.power.W ~ Potential.PV.power.W|streetlight)$PV.power.W,
                    rf2 = impute_rf(df, PV.power.W ~ Potential.PV.power.W+timeUse|streetlight)$PV.power.W)
imputedPVPower2 <- imputedPVPower2 %>% mutate(knn1 = df$knn1, knn2 = df$knn2, knn3=df$knn3, 
                                              rf1 = df$rf1, rf2=df$rf2)  

# Impute using MICE -  SKIP for the moment
df <- imputedPVPower2[,c(1,3,4,12,13)]
for(i in seq_along(unique(df$streetlight))) {
  df_sub <- df[df$streetlight == unique(df$streetlight)[i],]
  mod <- mice(df_sub[,-c(1,2,3)], method="pmm")$imp$PV.power.W
  df_sub <- df_sub %>% mutate(mice1=PV.power.W, mice2=PV.power.W)
  
}
#******************************************************************************************#