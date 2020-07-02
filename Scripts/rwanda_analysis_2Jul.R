#******************************************************************************************#
# This is the script for analysing Rwanda SL data                                           #
# Author: K Bhargava                                                                       #
# Last updated on: 2nd July 2020                                                           #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(wesanderson)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data"
plot_dir <- "Plots/Paper 7"
#******************************************************************************************#

#******************************************************************************************#
# Read data and calculate typical data values
na_seadec_correctedData <- read.csv(here(filepath,"na_seadec_correctedData.csv"), header=TRUE, stringsAsFactors=FALSE)
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(date=as.Date(date),timestamp=as.POSIXct(timestamp, tz="GMT", origin="1970-01-01"),
         month2=factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                       labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))
#******************************************************************************************#

#******************************************************************************************#
# Get full data days from original data - all variables needed
system_sub_original <- na_seadec_correctedData[,c(1,2,3,9,18,12,21,25)]
system_sub_original <- system_sub_original[complete.cases(system_sub_original),]
onHours <- system_sub_original %>% group_by(streetlight, month2, date) %>% 
  summarise(hours=length(PV.power.W_original))
onHours <- as.data.frame(onHours[onHours$hours==24,])
full_days <- onHours %>% group_by(streetlight, month2) %>% summarise(days=length(date)) 
write.csv(full_days, file=here(filepath,"full_days_all_data_rwanda.csv"), row.names=FALSE)

# Get full data days from original data - all variables needed except AC load
system_sub_original <- na_seadec_correctedData[,c(1,2,3,9,12,21,25)]
system_sub_original <- system_sub_original[complete.cases(system_sub_original),]
onHours <- system_sub_original %>% group_by(streetlight, month2, date) %>% 
  summarise(hours=length(PV.power.W_original))
onHours <- as.data.frame(onHours[onHours$hours==24,])
full_days <- onHours %>% group_by(streetlight, month2) %>% summarise(days=length(date)) 
write.csv(full_days, file=here(filepath,"full_days_all_except_AC_rwanda.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Get typical day values
# Subset data to get SL, Time, Potential PV, SoC, Actual PV power, Actual AC load, 
# Positive actual Solar battery power (E_a), Positive and negative actual battery power,
# Actual light load
system_sub_kalman <- na_seadec_correctedData[,c(1,3,22,26,28,32,50,54,55,57)]
system_sub_interpolation <- na_seadec_correctedData[,c(1,3,22,27,29,33,48,52,53,59)]

# Calculate Total load (actual AC + actual light), capture loss (potential-actual PV), SoC=SoC*100/3072 - Kalman
system_sub_kalman <- system_sub_kalman %>% 
  mutate(load = abs(Actual.AC.consumption.W_kalman) + abs(Actual.Light.laod.W_kalman),
         loss = Potential.PV.power.W - Postive.Actual.Solar.Charger.Battery.Power.W_kalman,
         State.of.Charge.W_kalman=State.of.Charge.W_kalman*100/3072)
system_sub_kalman <- system_sub_kalman[,-c(6,10)] # Remove socket and light load
colnames(system_sub_kalman) <- c("streetlight","timeUse","E_p","SoC","PV","E_a",
                                 "B_cp","B_dp","E_load","L_c")

# Calculate Total load (actual AC + actual light), capture loss (potential-actual PV), SoC=SoC*100/3072 - Interpolation
system_sub_interpolation <- system_sub_interpolation %>% 
  mutate(load = abs(Actual.AC.consumption.W_interpolation) + abs(Actual.Light.laod.W_interpolation),
         loss = Potential.PV.power.W - Postive.Actual.Solar.Charger.Battery.Power.W_interpolation,
         State.of.Charge.W_interpolation=State.of.Charge.W_interpolation*100/3072)
system_sub_interpolation <- system_sub_interpolation[,-c(6,10)] # Remove socket and light load
colnames(system_sub_interpolation) <- c("streetlight","timeUse","E_p","SoC","PV","E_a",
                                        "B_cp","B_dp","E_load","L_c")

# Correct E_a value for SL3 - replace it with Actual PV* 0.94
system_sub_kalman <- system_sub_kalman %>% 
  mutate(E_a=ifelse(streetlight=="SL3", PV*0.94, E_a))
system_sub_interpolation <- system_sub_interpolation %>%
  mutate(E_a=ifelse(streetlight=="SL3", PV*0.94, E_a))

# Calculate typical values for each SL
system_sub_kalman <- gather(system_sub_kalman, id, value, 3:10)
system_typical_kalman <- system_sub_kalman %>% group_by(streetlight, timeUse, id) %>% 
  summarise(value=mean(value, na.rm=TRUE))
system_typical_kalman <- as.data.frame(system_typical_kalman)
system_typical_kalman <- spread(system_typical_kalman, id, value)

system_sub_interpolation <- gather(system_sub_interpolation, id, value, 3:10)
system_typical_interpolation <- system_sub_interpolation %>% group_by(streetlight, timeUse, id) %>% 
  summarise(value=mean(value, na.rm=TRUE))
system_typical_interpolation <- as.data.frame(system_typical_interpolation)
system_typical_interpolation <- spread(system_typical_interpolation, id, value)

# Plot typical values for each SL
plotTypical <- function(df) {
  ggplot(df, aes(x=timeUse)) + geom_line(aes(y=B_cp/1000.0, color="B_cp", linetype="B_cp")) +
    geom_line(aes(y=abs(B_dp)/1000.0, color="B_dp",linetype="B_dp")) + 
    geom_line(aes(y=E_a/1000.0, color="E_a",linetype="E_a")) +
    geom_line(aes(y=E_load/1000.0, color="E_load",linetype="E_load")) + 
    geom_line(aes(y=E_p/1000.0, color="E_p",linetype="E_p")) +
    geom_line(aes(y=L_c/1000.0, color="L_c",linetype="L_c")) + 
    geom_line(aes(y = SoC/400, color = "SoC",linetype="SoC")) + 
    scale_y_continuous(breaks= seq(0,0.25,0.05), sec.axis = sec_axis(~.*400, 
                                                                     name = "State of Charge (%)")) +
    labs(y="Energy (kWh)", x = "Time of day", colour="Parameter", linetype="Parameter") +
    scale_x_continuous(breaks=seq(0,24,by=2)) + theme(plot.title = element_text(size=10), 
                                                      legend.position = "bottom",legend.box = "horizontal",
                                                      legend.key.size = unit(0.6, "cm"), legend.margin = margin(t=0,r=0,b=0,l=0),
                                                      axis.text = element_text(size=10), axis.title = element_text(size=12))
}
plotTypical(system_typical_kalman[system_typical_kalman$streetlight=="SL1",]) + 
  labs(title="Actual typical day profile for Rwanda SL1 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"typical_day_sl1_imputed_kalman.png"))
plotTypical(system_typical_kalman[system_typical_kalman$streetlight=="SL2",]) + 
  labs(title="Actual typical day profile for Rwanda SL2 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"typical_day_sl2_imputed_kalman.png"))
plotTypical(system_typical_kalman[system_typical_kalman$streetlight=="SL3",]) + 
  labs(title="Actual typical day profile for Rwanda SL3 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"typical_day_sl3_imputed_kalman.png"))
plotTypical(system_typical_kalman[system_typical_kalman$streetlight=="SL4",]) + 
  labs(title="Actual typical day profile for Rwanda SL4 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"typical_day_sl4_imputed_kalman.png"))

plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL1",]) + 
  labs(title="Actual typical day profile for Rwanda SL1 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"typical_day_sl1_imputed_interpolation.png"))
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL2",]) + 
  labs(title="Actual typical day profile for Rwanda SL2 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"typical_day_sl2_imputed_interpolation.png"))
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL3",]) + 
  labs(title="Actual typical day profile for Rwanda SL3 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"typical_day_sl3_imputed_interpolation.png"))
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL4",]) + 
  labs(title="Actual typical day profile for Rwanda SL4 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"typical_day_sl4_imputed_interpolation.png"))
#******************************************************************************************#

#*****************************************************************************************#
# Box plots for Socket consumption
plotACLoad <- function(df) {
  ggplot(df[df$date>"2019-07-19",], aes(as.factor(timeUse), Actual.AC.consumption.W_interpolation/1000.0)) + 
    geom_boxplot() + labs(x="Time of day", y="Socket consumption (kWh)") +
    theme(plot.title = element_text(size=10))
}
plotACLoad(na_seadec_correctedData[na_seadec_correctedData$streetlight=="SL1",]) + 
  labs(title="Hourly socket consumption at Rwanda SL1 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"acLoad_sl1_rwanda.png"))
plotACLoad(na_seadec_correctedData[na_seadec_correctedData$streetlight=="SL2",]) + 
  labs(title="Hourly socket consumption at Rwanda SL2 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"acLoad_sl2_rwanda.png"))
plotACLoad(na_seadec_correctedData[na_seadec_correctedData$streetlight=="SL3",]) + 
  labs(title="Hourly socket consumption at Rwanda SL3 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"acLoad_sl3_rwanda.png"))
plotACLoad(na_seadec_correctedData[na_seadec_correctedData$streetlight=="SL4",]) + 
  labs(title="Hourly socket consumption at Rwanda SL4 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"acLoad_sl4_rwanda.png"))
#*****************************************************************************************#

#******************************************************************************************#
# Calculate daily data - PV power (original and imputed), AC load (original and imputed), 
# Potential PV, actual PV power, actual AC load, 
# +ve/-ve solar charger battery watts (original and imputed), +ve/-ve system battery power 
# (original and imputed), +ve/-ve actual solar charger battery power, 
# +ve/-ve actual battery power, light demand and actual light load
na_seadec_sub <- na_seadec_correctedData[,c(1:3,7:9,16:18,22:25,28:29,32:33,36:59)]
# Calculate daily loads
na_seadec_sub <- gather(na_seadec_sub, id, value, c(4:10,14:41))
system_daily <- na_seadec_sub %>% group_by(streetlight, month2, date, id) %>% 
  summarise(value=sum(value, na.rm=TRUE))
system_daily <- as.data.frame(system_daily)

# Calculate daily value of SoC -  take mean for the day
na_seadec_sub <- na_seadec_correctedData[,c(1:3,25,26:27)]
# Calculate daily loads
na_seadec_sub <- gather(na_seadec_sub, id, value, c(5:6))
system_daily_soc <- na_seadec_sub %>% group_by(streetlight, month2, date, id) %>% 
  summarise(value=mean(value, na.rm=TRUE))
system_daily_soc <- as.data.frame(system_daily_soc)

# Bind data sets
system_daily <- rbind(system_daily, system_daily_soc)

system_daily <- spread(system_daily, id, value)
write.csv(system_daily, file=here(filepath,"system_daily_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#

#*****************************************************************************************#
# Monthly daily avg - remove AC load from 1st to 19th July as no inverter was installed
system_daily <- read.csv(here(filepath,"system_daily_correctedData.csv"), header=TRUE, 
                         stringsAsFactors=FALSE)
system_daily <- system_daily %>% 
  mutate(date=as.Date(date), 
         month2=factor(month2, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                       labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))
system_daily$Actual.AC.consumption.W_interpolation[system_daily$date<="2019-07-19"] <- NA
system_daily$Actual.AC.consumption.W_kalman[system_daily$date<="2019-07-19"] <- NA
system_daily$System.overview.AC.Consumption.L1.W_interpolation[system_daily$date<="2019-07-19"] <- NA
system_daily$System.overview.AC.Consumption.L1.W_kalman[system_daily$date<="2019-07-19"] <- NA
system_daily$System.overview.AC.Consumption.L1.W_original[system_daily$date<="2019-07-19"] <- NA

system_daily <- gather(system_daily, id, value, 4:40)
system_monthly <- system_daily %>% group_by(streetlight, month2, id) %>% 
  summarise(value=mean(value, na.rm=TRUE))
system_monthly <- as.data.frame(system_monthly)
# Converting power from W to Wh
system_monthly <- system_monthly %>% 
  mutate(value=ifelse(id=="State.of.Charge.W_interpolation" |
                        id=="State.of.Charge.W_kalman", value*100/3072, value/1000.0))
# Consider absolute values for all variables
system_monthly <- system_monthly %>% mutate(value=abs(value))
system_monthly <- spread(system_monthly, id, value)
system_monthly <- system_monthly[order(system_monthly$streetlight, system_monthly$month2),]
write.csv(system_monthly, file=here(filepath,"monthly_avg_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#