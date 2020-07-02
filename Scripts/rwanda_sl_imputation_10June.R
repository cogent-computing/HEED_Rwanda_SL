#******************************************************************************************#
# This is the script for imputing missing data for all Rwanda SL                           #
# Author: K Bhargava                                                                       #
# Last updated on: 26th Jun 2020                                                           #
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
# Imputation using na_seadec owing to seasonality - works on univariate time series
methodImpute <- c("interpolation", "kalman")
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
    df <- gather(df, "variable","value",4:6)
    
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
na_seadec_sub <- gather(na_seadec_imputedData, id, value, c(4:22))
stats_na_seadec_sub <- na_seadec_sub %>% group_by(streetlight, id) %>%
  summarise(mean = mean(value, na.rm=TRUE), median = median(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE),
            skew = skewness(value, na.rm=TRUE), kurt = kurtosis(value, na.rm=TRUE))
stats_na_seadec_sub <- as.data.frame(stats_na_seadec_sub)  
stats_na_seadec_sub <- stats_na_seadec_sub[complete.cases(stats_na_seadec_sub),]
stats_na_seadec_sub <- gather(stats_na_seadec_sub, "variable", "value", 3:7)
ggplot(stats_na_seadec_sub[stats_na_seadec_sub$streetlight=="SL2" & stats_na_seadec_sub$variable=="sd",], 
       aes(id, abs(value))) + geom_bar(stat="identity", width=.3, position = "dodge")  + 
  theme(axis.text.x = element_text(angle=90))
write.csv(stats_na_seadec_sub, file=here(filepath,"stats_na_seadec.csv"), row.names=FALSE)

# Plot data for all variables
ggplot(na_seadec_sub[na_seadec_sub$id==unique(na_seadec_sub$id)[c(4,6,19)],], 
       aes(timestamp, value, color=id)) + facet_wrap(~streetlight) + geom_line() + theme(legend.position = "bottom") + 
  labs(x="Timestamp", y="Power (Wh)")
ggsave(here(plot_dir,"imputed_pv_power.png"))

ggplot(na_seadec_sub[na_seadec_sub$id==unique(na_seadec_sub$id)[c(5:6,13)],], 
       aes(timestamp, value, color=id)) + facet_wrap(~streetlight) + geom_line() + theme(legend.position = "bottom") + 
  labs(x="Timestamp", y="Solar Charger Battery power (Wh)")
ggsave(here(plot_dir,"imputed_sc_battery.png"))

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
na_seadec_imputedData <- na_seadec_imputedData %>% mutate(date = as.Date(date), 
                        month2=factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                                      labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")),
                                      timestamp=as.POSIXct(timestamp, origin="1970-01-01",tz="GMT"))

# Plot original and imputed values for AC consumption, load current and PV power
# Plot AC consumption
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1",], 
       aes(timestamp, System.overview.AC.Consumption.L1.W_original)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1",], 
       aes(timestamp, System.overview.AC.Consumption.L1.W_kalman)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1",], 
       aes(timestamp, System.overview.AC.Consumption.L1.W_interpolation)) + facet_wrap(~month2,scales = "free") + geom_line()

ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL2",], 
       aes(timestamp, System.overview.AC.Consumption.L1.W_original)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL2",], 
       aes(timestamp, System.overview.AC.Consumption.L1.W_kalman)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL2",], 
       aes(timestamp, System.overview.AC.Consumption.L1.W_interpolation)) + facet_wrap(~month2,scales = "free") + geom_line()

ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL3",], 
       aes(timestamp, System.overview.AC.Consumption.L1.W_original)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL3",], 
       aes(timestamp, System.overview.AC.Consumption.L1.W_kalman)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL3",], 
       aes(timestamp, System.overview.AC.Consumption.L1.W_interpolation)) + facet_wrap(~month2,scales = "free") + geom_line()

ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL4",], 
       aes(timestamp, System.overview.AC.Consumption.L1.W_original)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL4",], 
       aes(timestamp, System.overview.AC.Consumption.L1.W_kalman)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL4",], 
       aes(timestamp, System.overview.AC.Consumption.L1.W_interpolation)) + facet_wrap(~month2,scales = "free") + geom_line()
length(which(na_seadec_imputedData$System.overview.AC.Consumption.L1.W_orignal<0)) * 100 / 26400
length(which(na_seadec_imputedData$System.overview.AC.Consumption.L1.W_kalman<0)) * 100 / 26400
length(which(na_seadec_imputedData$System.overview.AC.Consumption.L1.W_interpolation<0)) * 100 / 26400
a <- na_seadec_imputedData$System.overview.AC.Consumption.L1.W_kalman[na_seadec_imputedData$System.overview.AC.Consumption.L1.W_Kalman<0]
a <- round(a, digits=0)
length(which(a!=0)) * 100 / 26400 

# Plot Load current
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1",], 
       aes(timestamp, Solar.Charger.Load.current.A_original)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1",], 
       aes(timestamp, Solar.Charger.Load.current.A_kalman)) + facet_wrap(~month2,scales = "free") + geom_line()

ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL2",], 
       aes(timestamp, Solar.Charger.Load.current.A_original)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL2",], 
       aes(timestamp, Solar.Charger.Load.current.A_kalman)) + facet_wrap(~month2,scales = "free") + geom_line()

ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL3",], 
       aes(timestamp, Solar.Charger.Load.current.A_original)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL3",], 
       aes(timestamp, Solar.Charger.Load.current.A_kalman)) + facet_wrap(~month2,scales = "free") + geom_line()

ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL4",], 
       aes(timestamp, Solar.Charger.Load.current.A_original)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL4",], 
       aes(timestamp, Solar.Charger.Load.current.A_kalman)) + facet_wrap(~month2,scales = "free") + geom_line()
length(which(na_seadec_imputedData$Solar.Charger.Load.current.A_origin<0)) * 100 / 26400
length(which(na_seadec_imputedData$Solar.Charger.Load.current.A_kalman<0)) * 100 / 26400
a <- na_seadec_imputedData$Solar.Charger.Load.current.A_Kalman[na_seadec_imputedData$Solar.Charger.Load.current.A_Kalman<0]
a <- round(a, digits=0)
length(which(a!=0)) * 100 / 26400 

# Plot PV power
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1",], 
       aes(timestamp, PV.power.W_original)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1",], 
       aes(x=timestamp)) + facet_wrap(~month2,scales = "free") + 
  geom_line(aes(y=PV.power.W_kalman)) + geom_line(aes(y=Potential.PV.power.W),color="red")
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1",], 
       aes(x=timestamp)) + facet_wrap(~month2,scales = "free") + 
  geom_line(aes(y=PV.power.W_interpolation)) + geom_line(aes(y=Potential.PV.power.W),color="red")

ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL2",], 
       aes(timestamp, PV.power.W_original)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL2",], 
       aes(x=timestamp)) + facet_wrap(~month2,scales = "free") + 
  geom_line(aes(y=PV.power.W_kalman)) + geom_line(aes(y=Potential.PV.power.W),color="red")
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL2",], 
       aes(x=timestamp)) + facet_wrap(~month2,scales = "free") + 
  geom_line(aes(y=PV.power.W_interpolation)) + geom_line(aes(y=Potential.PV.power.W),color="red")

ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL3",], 
       aes(timestamp, PV.power.W_original)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL3",], 
       aes(x=timestamp)) + facet_wrap(~month2,scales = "free") + 
  geom_line(aes(y=PV.power.W_kalman)) + geom_line(aes(y=Potential.PV.power.W),color="red")
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL3",], 
       aes(x=timestamp)) + facet_wrap(~month2,scales = "free") + 
  geom_line(aes(y=PV.power.W_interpolation)) + geom_line(aes(y=Potential.PV.power.W),color="red")

ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL4",], 
       aes(timestamp, PV.power.W_original)) + facet_wrap(~month2,scales = "free") + geom_line()
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL4",], 
       aes(x=timestamp)) + facet_wrap(~month2,scales = "free") + 
  geom_line(aes(y=PV.power.W_kalman)) + geom_line(aes(y=Potential.PV.power.W),color="red")
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL4",], 
       aes(x=timestamp)) + facet_wrap(~month2,scales = "free") + 
  geom_line(aes(y=PV.power.W_interpolation)) + geom_line(aes(y=Potential.PV.power.W),color="red")
length(which(na_seadec_imputedData$PV.power.W_original<0)) * 100 / 26400
length(which(na_seadec_imputedData$PV.power.W_Kalman<0)) * 100 / 26400
a <- na_seadec_imputedData$PV.power.W_Kalman[na_seadec_imputedData$PV.power.W_Kalman<0]
a <- round(a, digits=0)
length(which(a!=0)) * 100 / 26400 

# AC consumption, load current and PV power must always be +ve; change all -ve imputed values to zero
na_seadec_imputedData <- na_seadec_imputedData %>% 
  mutate(PV.power.W_interpolation=ifelse(PV.power.W_interpolation<0,0, PV.power.W_interpolation),
    PV.power.W_kalman=ifelse(PV.power.W_kalman<0,0, PV.power.W_kalman),
         Solar.Charger.Load.current.A_interpolation=ifelse(Solar.Charger.Load.current.A_interpolation<0,0,
                                                    Solar.Charger.Load.current.A_interpolation),
    Solar.Charger.Load.current.A_kalman=ifelse(Solar.Charger.Load.current.A_kalman<0,0,
                                               Solar.Charger.Load.current.A_kalman),
    System.overview.AC.Consumption.L1.W_interpolation=ifelse(System.overview.AC.Consumption.L1.W_interpolation<0, 0,
                                                      System.overview.AC.Consumption.L1.W_interpolation),
         System.overview.AC.Consumption.L1.W_kalman=ifelse(System.overview.AC.Consumption.L1.W_kalman<0, 0,
                                                           System.overview.AC.Consumption.L1.W_kalman))

na_seadec_correctedData <- data.frame()
for(i in seq_along(unique(na_seadec_imputedData$streetlight))) {
  x <- na_seadec_imputedData[na_seadec_imputedData$streetlight==unique(na_seadec_imputedData$streetlight)[i],]
  
  # Calculate state of charge of battery and apply upper and lower thresholds - discharge of up to 20% acceptable
  # Using Kalman smoothing
  state <- 3072 + x$System.overview.Battery.Power.W_kalman[1]
  if(state>3072) {
    state <- 3072
  } else if(state<614) {
    state <- 614
  }
  for(j in 2:length(x$date)) {
    soc <- state[j-1] + x$System.overview.Battery.Power.W_kalman[j]
    if(soc>3072) {
      soc <- 3072
    } else if(soc<614) {
      soc <- 614
    }
    state <- append(state, soc)
  }
  x$State.of.Charge.W_kalman <- state
  
  # Using interpolation
  state <- 3072 + x$System.overview.Battery.Power.W_interpolation[1]
  if(state>3072) {
    state <- 3072
  } else if(state<614) {
    state <- 614
  }
  for(j in 2:length(x$date)) {
    soc <- state[j-1] + x$System.overview.Battery.Power.W_interpolation[j]
    if(soc>3072) {
      soc <- 3072
    } else if(soc<614) {
      soc <- 614
    }
    state <- append(state, soc)
  }
  x$State.of.Charge.W_interpolation <- state
  
  # Use SoC to correct Actual Battery power, Actual PV power and Actual AC consumption
  x <- x %>% mutate(Actual.PV.power.W_kalman=ifelse((State.of.Charge.W_kalman==614 & is.na(PV.power.W_original)), 0, 
                                                    PV.power.W_kalman),
                    Actual.PV.power.W_interpolation=ifelse((State.of.Charge.W_interpolation==614 & 
                                                              is.na(PV.power.W_original)), 0, PV.power.W_interpolation),
                    Actual.Solar.Charger.Battery.Power.W_kalman=ifelse((State.of.Charge.W_kalman==614 & 
                      is.na(Solar.Charger.Battery.watts.W_original)), 0, Solar.Charger.Battery.watts.W_kalman),
                    Actual.Solar.Charger.Battery.Power.W_interpolation=ifelse((State.of.Charge.W_interpolation==614 & 
                       is.na(Solar.Charger.Battery.watts.W_original)), 0, Solar.Charger.Battery.watts.W_interpolation),
                    Actual.AC.consumption.W_kalman=ifelse((State.of.Charge.W_kalman==614 & 
                      is.na(System.overview.AC.Consumption.L1.W_original)), 0,System.overview.AC.Consumption.L1.W_kalman),
                    Actual.AC.consumption.W_interpolation=ifelse((State.of.Charge.W_interpolation==614 & 
                      is.na(System.overview.AC.Consumption.L1.W_original)), 0,System.overview.AC.Consumption.L1.W_interpolation),
                    Actual.Battery.power.W_kalman=ifelse((State.of.Charge.W_kalman==614 & 
                      is.na(System.overview.Battery.Power.W_original)), 0, System.overview.Battery.Power.W_kalman),
                    Actual.Battery.power.W_interpolation=ifelse((State.of.Charge.W_interpolation==614 & 
                      is.na(System.overview.Battery.Power.W_original)), 0, System.overview.Battery.Power.W_interpolation))

  # Bind data for all SL
  na_seadec_correctedData <- rbind(na_seadec_correctedData, x)
}

# Add the +ve/-ve solar charger battery power, +ve/-ve battery power, +ve/-ve actual solar charger battery power,
# +ve/-ve actual battery power for both kalman and interpolation imputation
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(Positive.Solar.Charger.Battery.watts.W_interpolation=ifelse(Solar.Charger.Battery.watts.W_interpolation<0, 0,
                                                              Solar.Charger.Battery.watts.W_interpolation),
         Negative.Solar.Charger.Battery.watts.W_interpolation=ifelse(Solar.Charger.Battery.watts.W_interpolation>0,0,
                                                              Solar.Charger.Battery.watts.W_interpolation),
         Positive.Solar.Charger.Battery.watts.W_kalman=ifelse(Solar.Charger.Battery.watts.W_kalman<0, 0,
                                                              Solar.Charger.Battery.watts.W_kalman),
         Negative.Solar.Charger.Battery.watts.W_kalman=ifelse(Solar.Charger.Battery.watts.W_kalman>0,0,
                                                              Solar.Charger.Battery.watts.W_kalman),
         Positive.Solar.Charger.Battery.watts.W_original=ifelse(Solar.Charger.Battery.watts.W_original<0,0,
                                                                Solar.Charger.Battery.watts.W_original),
         Negative.Solar.Charger.Battery.watts.W_original=ifelse(Solar.Charger.Battery.watts.W_original>0, 0,
                                                                Solar.Charger.Battery.watts.W_original),
         Positive.System.Battery.Power.W_interpolation=ifelse(System.overview.Battery.Power.W_interpolation<0,0,
                                                       System.overview.Battery.Power.W_interpolation),
         Negative.System.Battery.Power.W_interpolation=ifelse(System.overview.Battery.Power.W_interpolation>0, 0,
                                                       System.overview.Battery.Power.W_interpolation),
         Positive.System.Battery.Power.W_kalman=ifelse(System.overview.Battery.Power.W_kalman<0,0,
                                                       System.overview.Battery.Power.W_kalman),
         Negative.System.Battery.Power.W_kalman=ifelse(System.overview.Battery.Power.W_kalman>0, 0,
                                                       System.overview.Battery.Power.W_kalman),
         Positive.System.Battery.Power.W_original=ifelse(System.overview.Battery.Power.W_original<0, 0,
                                                         System.overview.Battery.Power.W_original),
         Negative.System.Battery.Power.W_original=ifelse(System.overview.Battery.Power.W_original>0, 0,
                                                         System.overview.Battery.Power.W_original),
         Postive.Actual.Solar.Charger.Battery.Power.W_interpolation=ifelse(Actual.Solar.Charger.Battery.Power.W_interpolation<0, 0,
                                                                           Actual.Solar.Charger.Battery.Power.W_interpolation),
         Negative.Actual.Solar.Charger.Battery.Power.W_interpolation=ifelse(Actual.Solar.Charger.Battery.Power.W_interpolation>0, 0,
                                                                            Actual.Solar.Charger.Battery.Power.W_interpolation),
         Postive.Actual.Solar.Charger.Battery.Power.W_kalman=ifelse(Actual.Solar.Charger.Battery.Power.W_kalman<0, 0,
                                                             Actual.Solar.Charger.Battery.Power.W_kalman),
         Negative.Actual.Solar.Charger.Battery.Power.W_kalman=ifelse(Actual.Solar.Charger.Battery.Power.W_kalman>0, 0,
                                                              Actual.Solar.Charger.Battery.Power.W_kalman),
         Positive.Actual.Battery.Power.W_interpolation=ifelse(Actual.Battery.power.W_interpolation<0, 0, 
                                                              Actual.Battery.power.W_interpolation),
         Negative.Actual.Battery.Power.W_interpolation=ifelse(Actual.Battery.power.W_interpolation>0, 0, 
                                                              Actual.Battery.power.W_interpolation),
         Positive.Actual.Battery.Power.W_kalman=ifelse(Actual.Battery.power.W_kalman<0, 0, Actual.Battery.power.W_kalman),
         Negative.Actual.Battery.Power.W_kalman=ifelse(Actual.Battery.power.W_kalman>0, 0, Actual.Battery.power.W_kalman))

# Add light load - calculated by multiplying -ve battery power with battery and solar charger efficiencies (0.88*0.88)
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(Light.demand.W_kalman=Negative.System.Battery.Power.W_kalman*0.77,
         Actual.Light.laod.W_kalman=Negative.Actual.Battery.Power.W_kalman*0.77,
         Light.demand.W_interpolation=Negative.System.Battery.Power.W_interpolation*0.77,
         Actual.Light.laod.W_interpolation=Negative.Actual.Battery.Power.W_interpolation*0.77)
write.csv(na_seadec_correctedData, file=here(filepath,"na_seadec_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#