#******************************************************************************************#
# This is the script for imputing missing data for all Rwanda SL                           #
# Author: K Bhargava                                                                       #
# Last updated on: 10th Jun 2020                                                           #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(wesanderson)
library("imputeTS") # for na_seadec imputation
library(xts) # for converting data into time series
library(timeDate)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data"
plot_dir <- "Plots/Paper 7"
#******************************************************************************************#

#******************************************************************************************#
# Read hourly data and subset data to choose only columns we need to impute
system_hourly <- read.csv(here(filepath,"raw_hourly_sl_data.csv"), header = TRUE, stringsAsFactors=FALSE)
system_hourly <- system_hourly %>% mutate(date = as.Date(date))
system_sub <- system_hourly[,c(1:3,5,10:14,16)]
#******************************************************************************************#

#******************************************************************************************#
# Imputation using na_seadec (kalman) owing to seasonality in data - works on univariate time series
# Impute missing values for Voltage, PV power, Solar charger battery power, Load current, AC consumption, Battery power
variables <- c("Battery.Monitor.Voltage.V","PV.power.W","Solar.Charger.Battery.watts.W","Solar.Charger.Load.current.A",
               "System.overview.AC.Consumption.L1.W","System.overview.Battery.Power.W")
na_seadec_imputedData <- data.frame()
for(k in seq_along(variables)) {
  x <- system_sub[c("streetlight","date","timeUse",variables[k])]
  for(i in seq_along(unique(x$streetlight))) {
    df <- x[x$streetlight == unique(x$streetlight)[i], ]
    
    # Convert data frame into a time series using xts to serve as input to na_seadec function
    # For this data, seasonality is 1 day with a reading every hour
    df.ts <- df[,-1]
    df.ts <- spread(df.ts, timeUse, variables[k])
    df.ts <- xts(df.ts[,-1], order.by=as.Date(df.ts[,1], "%Y-%m-%d"))
    
    # Impute data using different functions of na_seadec and bind to df
    df1 <- as.data.frame(na_seadec(df.ts, algorithm="kalman",find_frequency=TRUE))
    df1 <- df1 %>% mutate(date=row.names(df1))
    df1 <- gather(df1, "timeUse", "value", 1:24)
    df1[is.na(df1)] <- 0
    df1 <- df1[order(df1$date),]
    df <- cbind(df, df1$value)
    
    colnames(df) <- c(colnames(df)[1:3],paste(variables[k],"original",sep="_"),
                      paste(variables[k],"Kalman",sep="_"))
    df <- gather(df, "variable","value",4:5)
    
    # Bind data for all SL
    na_seadec_imputedData <- rbind(na_seadec_imputedData, df)
  }
}
na_seadec_imputedData <- spread(na_seadec_imputedData, variable, value)
na_seadec_imputedData <- na_seadec_imputedData %>% 
  mutate(Potential.PV.power.W = system_sub$Potential_PV_power_W,
         month = as.character(lubridate::month(date, label=TRUE, abbr=TRUE)),
         timestamp = as.POSIXct(paste(date, ifelse(timeUse<10, paste("0",timeUse,":00:00",sep=""), 
                                                   paste(timeUse,":00:00",sep="")), sep=" "), origin="1970-01-01",tz="GMT"),
         month2 = factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                         labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))
write.csv(na_seadec_imputedData, file=here(filepath,"na_seadec_imputed_data.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Compute statistics for original and imputed data
na_seadec_sub <- gather(na_seadec_imputedData, id, value, c(4:16))
stats_na_seadec_sub <- na_seadec_sub %>% group_by(streetlight, id) %>%
  summarise(mean = mean(value, na.rm=TRUE), median = median(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE),
            skew = skewness(value, na.rm=TRUE), kurt = kurtosis(value, na.rm=TRUE))
stats_na_seadec_sub <- as.data.frame(stats_na_seadec_sub)  
stats_na_seadec_sub <- stats_na_seadec_sub[complete.cases(stats_na_seadec_sub),]
write.csv(stats_na_seadec_sub, file=here(filepath,"stats_na_seadec.csv"), row.names=FALSE)

# Plot data for all variables
ggplot(na_seadec_sub[na_seadec_sub$id==unique(na_seadec_sub$id)[c(1:2)],], 
       aes(timestamp, value, color=id)) + facet_wrap(~streetlight) + geom_line() + theme(legend.position = "bottom") + 
  labs(x="Timestamp", y="Battery monitor voltage (V)")
ggsave(here(plot_dir,"imputed_voltage.png"))

ggplot(na_seadec_sub[na_seadec_sub$id==unique(na_seadec_sub$id)[c(3:4,13)],], 
       aes(timestamp, value, color=id)) + facet_wrap(~streetlight) + geom_line() + theme(legend.position = "bottom") + 
  labs(x="Timestamp", y="PV power (Wh)")
ggsave(here(plot_dir,"imputed_pv_power.png"))

ggplot(na_seadec_sub[na_seadec_sub$id==unique(na_seadec_sub$id)[c(5:6,13)],], 
       aes(timestamp, value, color=id)) + facet_wrap(~streetlight) + geom_line() + theme(legend.position = "bottom") + 
  labs(x="Timestamp", y="Solar Charger Battery power (Wh)")
ggsave(here(plot_dir,"imputed_sc_battery.png"))

ggplot(na_seadec_sub[na_seadec_sub$id==unique(na_seadec_sub$id)[c(7:8)],], 
       aes(timestamp, value, color=id)) + facet_wrap(~streetlight) + geom_line() + theme(legend.position = "bottom") + 
  labs(x="Timestamp", y="Solar Charger Load current (A)")
ggsave(here(plot_dir,"imputed_current.png"))

ggplot(na_seadec_sub[na_seadec_sub$id==unique(na_seadec_sub$id)[c(9:10)],], 
       aes(timestamp, value, color=id)) + facet_wrap(~streetlight) + geom_line() + theme(legend.position = "bottom") + 
  labs(x="Timestamp", y="AC Consumption (Wh)")
ggsave(here(plot_dir,"imputed_acLoad.png"))

ggplot(na_seadec_sub[na_seadec_sub$id==unique(na_seadec_sub$id)[c(11:12)],], 
       aes(timestamp, value, color=id)) + facet_wrap(~streetlight) + geom_line() + theme(legend.position = "bottom") + 
  labs(x="Timestamp", y="System Battery Power (Wh)")
ggsave(here(plot_dir,"imputed_battery_power.png"))
#******************************************************************************************#

#******************************************************************************************#
# Add to the data SoC, actual battery power, actual PV power and actual AC consumption
na_seadec_imputedData <- read.csv(here(filepath,"na_seadec_imputed_data.csv"), header=TRUE, stringsAsFactors = FALSE)
na_seadec_imputedData <- na_seadec_imputedData %>% mutate(date = as.Date(date), month2=as.factor(month2))

# AC consumption, load current and PV power must always be +ve; change all -ve imputed values to zero
na_seadec_imputedData <- na_seadec_imputedData %>% 
  mutate(PV.power.W_Kalman=ifelse(PV.power.W_Kalman<0,0, PV.power.W_Kalman),
         Solar.Charger.Load.current.A_Kalman=ifelse(Solar.Charger.Load.current.A_Kalman<0,0,
                                                    Solar.Charger.Load.current.A_Kalman),
         System.overview.AC.Consumption.L1.W_Kalman=ifelse(System.overview.AC.Consumption.L1.W_Kalman<0, 0,
                                                           System.overview.AC.Consumption.L1.W_Kalman))

na_seadec_correctedData <- data.frame()
for(i in seq_along(unique(na_seadec_imputedData$streetlight))) {
  x <- na_seadec_imputedData[na_seadec_imputedData$streetlight==unique(na_seadec_imputedData$streetlight)[i],]
  
  # Calculate state of charge of battery and apply upper and lower thresholds - discharge of up to 20% acceptable
  state <- 3072 + x$System.overview.Battery.Power.W_Kalman[1]
  if(state>3072) {
    state <- 3072
  } else if(state<614) {
    state <- 614
  }
  for(j in 2:length(x$date)) {
    soc <- state[j-1] + x$System.overview.Battery.Power.W_Kalman[j]
    if(soc>3072) {
      soc <- 3072
    } else if(soc<614) {
      soc <- 614
    }
    state <- append(state, soc)
  }
  x$State.of.Charge.W <- state
  
  # Use SoC to correct Actual Battery power, Actual PV power and Actual AC consumption
  x$Actual.PV.power.W <- ifelse((x$State.of.Charge.W==614 & is.na(x$PV.power.W_original)), 0, x$PV.power.W_Kalman)
  x$Actual.Solar.Charger.Battery.Power.W <- ifelse((x$State.of.Charge.W==614 & is.na(x$Solar.Charger.Battery.watts.W_original)), 0, 
                                                 x$Solar.Charger.Battery.watts.W_Kalman)
  x$Actual.AC.consumption.W <- ifelse((x$State.of.Charge.W==614 & is.na(x$System.overview.AC.Consumption.L1.W_original)), 0, 
                                      x$System.overview.AC.Consumption.L1.W_Kalman)
  x$Actual.Battery.power.W <- ifelse((x$State.of.Charge.W==614 & is.na(x$System.overview.Battery.Power.W_original)), 0, 
                                     x$System.overview.Battery.Power.W_Kalman)
  
  # Bind data for all SL
  na_seadec_correctedData <- rbind(na_seadec_correctedData, x)
}

# Add the +ve/-ve solar charger battery power, +ve/-ve battery power, +ve/-ve actual solar charger battery power,
# +ve/-ve actual battery power
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(Positive.Solar.Charger.Battery.watts.W_Kalman=ifelse(Solar.Charger.Battery.watts.W_Kalman<0, 0,
                                                              Solar.Charger.Battery.watts.W_Kalman),
         Negative.Solar.Charger.Battery.watts.W_Kalman=ifelse(Solar.Charger.Battery.watts.W_Kalman>0,0,
                                                              Solar.Charger.Battery.watts.W_Kalman),
         Positive.Solar.Charger.Battery.watts.W_original=ifelse(Solar.Charger.Battery.watts.W_original<0,0,
                                                                Solar.Charger.Battery.watts.W_original),
         Negative.Solar.Charger.Battery.watts.W_original=ifelse(Solar.Charger.Battery.watts.W_original>0, 0,
                                                                Solar.Charger.Battery.watts.W_original),
         Positive.System.Battery.Power.W_Kalman=ifelse(System.overview.Battery.Power.W_Kalman<0,0,
                                                       System.overview.Battery.Power.W_Kalman),
         Negative.System.Battery.Power.W_Kalman=ifelse(System.overview.Battery.Power.W_Kalman>0, 0,
                                                       System.overview.Battery.Power.W_Kalman),
         Positive.System.Battery.Power.W_original=ifelse(System.overview.Battery.Power.W_original<0, 0,
                                                         System.overview.Battery.Power.W_original),
         Negative.System.Battery.Power.W_original=ifelse(System.overview.Battery.Power.W_original>0, 0,
                                                         System.overview.Battery.Power.W_original),
         Postive.Actual.Solar.Charger.Battery.Power.W=ifelse(Actual.Solar.Charger.Battery.Power.W<0, 0,
                                                             Actual.Solar.Charger.Battery.Power.W),
         Negative.Actual.Solar.Charger.Battery.Power.W=ifelse(Actual.Solar.Charger.Battery.Power.W>0, 0,
                                                              Actual.Solar.Charger.Battery.Power.W),
         Positive.Actual.Battery.Power.W=ifelse(Actual.Battery.power.W<0, 0, Actual.Battery.power.W),
         Negative.Actual.Battery.Power.W=ifelse(Actual.Battery.power.W>0, 0, Actual.Battery.power.W))

# Add light load - calculated by multiplying -ve battery power with battery and solar charger efficiencies (0.88*0.88)
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(Light.demand.W=Negative.System.Battery.Power.W_Kalman*0.7744,
         Actual.Light.laod.W=Negative.Actual.Battery.Power.W*0.7744)
write.csv(na_seadec_correctedData, file=here(filepath,"na_seadec_correctedData.csv"), row.names=FALSE)

# Read data and calculate typical data values
na_seadec_correctedData <- read.csv(here(filepath,"na_seadec_correctedData.csv"), header=TRUE, stringsAsFactors=FALSE)
# Potential PV, Charge power, Discharge power, Actual PV, Total load (actual AC+actual light); capture loss; SoC
system_sub <- na_seadec_correctedData[,c(1,3,16,20,21,23,35,36,38)]
system_sub <- system_sub %>% mutate(load = abs(Actual.AC.consumption.W)+abs(Actual.Light.laod.W),
                                    loss = Potential.PV.power.W - Actual.PV.power.W,
                                    State.of.Charge.W=State.of.Charge.W*100/3072)
system_sub <- system_sub[,-c(6,9)]
colnames(system_sub) <- c("streetlight","timeUse","E_p","SoC","E_a","B_cp","B_dp","E_load","L_c")

# Calculate typical values for each SL
system_sub <- gather(system_sub, id, value, 3:9)
system_typical <- system_sub %>% group_by(streetlight, timeUse, id) %>% summarise(value=mean(value, na.rm=TRUE))
system_typical <- as.data.frame(system_typical)
system_typical <- spread(system_typical, id, value)

# Plot typical values for each SL
plotTypical <- function(df) {
  ggplot(df, aes(x=timeUse)) + geom_line(aes(y=B_cp/1000.0, color="B_cp"),linetype=1) +
    geom_line(aes(y=abs(B_dp)/1000.0, color="B_dp"),linetype=2) + geom_line(aes(y=E_a/1000.0, color="E_a"),linetype=3) +
    geom_line(aes(y=E_load/1000.0, color="E_load"),linetype=4) + geom_line(aes(y=E_p/1000.0, color="E_p"),linetype=5) +
    geom_line(aes(y=L_c/1000.0, color="L_c"),linetype=6) + geom_line(aes(y = SoC/400, color = "SoC", group="SoC"), linetype=7)+ 
    scale_y_continuous(breaks= seq(0,0.25,0.05), sec.axis = sec_axis(~.*400, name = "SoC (%)")) +
    labs(y="Energy (kWh)", x = "Time of day", colour="Parameter") +
    scale_x_continuous(breaks=seq(0,24,by=2)) + theme(plot.title = element_text(size=10), legend.position = "bottom",
                                                      legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), legend.margin = margin(t=0,r=0,b=0,l=0))
}
plotTypical(system_typical[system_typical$streetlight=="SL1",]) + 
  labs(title="Actual Rwanda SL1 power profile for a typical day from July 2019 to Mar 2020")
ggsave(here(plot_dir,"typical_day_sl1_imputed.png"))
plotTypical(system_typical[system_typical$streetlight=="SL2",]) + 
  labs(title="Actual Rwanda SL2 power profile for a typical day from July 2019 to Mar 2020")
ggsave(here(plot_dir,"typical_day_sl2_imputed.png"))
plotTypical(system_typical[system_typical$streetlight=="SL3",]) + 
  labs(title="Actual Rwanda SL3 power profile for a typical day from July 2019 to Mar 2020")
ggsave(here(plot_dir,"typical_day_sl3_imputed.png"))
plotTypical(system_typical[system_typical$streetlight=="SL4",]) + 
  labs(title="Actual Rwanda SL4 power profile for a typical day from July 2019 to Mar 2020")
ggsave(here(plot_dir,"typical_day_sl4_imputed.png"))
#******************************************************************************************#

#******************************************************************************************#
# Calculate daily data - PV power; Positive/Negative solar charger battery watts; AC consumption;
# Positive/Negative battery power; Potential PV power; Actual PV power; Actual AC consumption;
# Positive/Negative actual solar charger battery watts; Positive/Negative actual battery power; light demand
# Actual light load
na_seadec_sub <- na_seadec_correctedData[,c(1:3,6:9,12:19,21:38)]
# Re-arrange the columns
na_seadec_sub <- na_seadec_sub[,c(1:3,14,13,15,12,4,5,16,6,7,17,20:23,28,29,8,9,18,10,11,19,24:27,30,31,32,33)]
# Only keep +ve/-ve power for summing across 24 hours
na_seadec_sub <- na_seadec_sub[,c(1:10,14:22,26:33)]
# Calculate daily loads
na_seadec_sub <- gather(na_seadec_sub, id, value, 7:27)
system_daily <- na_seadec_sub %>% group_by(streetlight, month2, date, id) %>% summarise(value=sum(value, na.rm=TRUE))
system_daily <- as.data.frame(system_daily)
system_daily <- spread(system_daily, id, value)
write.csv(system_daily, file=here(filepath,"system_daily_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Monthly daily avg - remove data between 19th July to 6th Aug
system_daily <- gather(system_daily, id, value, 4:24)
system_daily <- system_daily[!(system_daily$date>="2019-07-19" & system_daily$date<="2019-08-06"),]
system_monthly <- system_daily %>% group_by(streetlight, month2, id) %>% summarise(value=mean(value, na.rm=TRUE))
system_monthly <- as.data.frame(system_monthly)
# Converting power from W to Wh
system_monthly <- system_monthly %>% mutate(value=value/1000.0)

system_monthly <- spread(system_monthly, id, value)
system_monthly <- system_monthly %>% mutate(month = factor(month2, 
                                                      levels=c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar"),
                                                      labels=c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")))
system_monthly <- system_monthly[order(system_monthly$streetlight, system_monthly$month),]
system_monthly <- system_monthly[,-2] # remove month2
system_monthly <- system_monthly[,c(1,23,2:22)]
write.csv(system_monthly, file=here(filepath,"monthly_avg_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#