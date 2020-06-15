#******************************************************************************************#
# This is the script for analysis of data for all Rwanda SL                                #
# Author: K Bhargava                                                                       #
# Last updated on: 10th Jun 2020                                                           #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(wesanderson)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Define MACROS
original <- c("Battery.Monitor.Voltage.V_original", "Potential.PV.power.W", "PV.power.W_original", 
              "Solar.Charger.Battery.watts.W_original", "Solar.Charger.Load.current.A_original",
              "System.overview.AC.Consumption.L1.W_original", "System.overview.Battery.Power.W_original") 
imputed <- c("Battery.Monitor.Voltage.V_Kalman", "Potential.PV.power.W", "PV.power.W_Kalman", 
             "Solar.Charger.Battery.watts.W_Kalman", "Solar.Charger.Load.current.A_Kalman",
             "System.overview.AC.Consumption.L1.W_Kalman", "System.overview.Battery.Power.W_Kalman")
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "../Data"
plot_dir <- "../Plots/Paper 7"
#******************************************************************************************#

#**********************************************************************************************#
# Read hourly data and subset data to choose only columns we need to impute
system_hourly <- read.csv(here(filepath,"na_seadec_imputed_data.csv"), header = TRUE, stringsAsFactors=FALSE)
system_hourly <- system_hourly %>% mutate(date = as.Date(date),
                                          timestamp = as.POSIXct(timestamp, tz="GMT", origin="1970-01-01"))
system_hourly 
# Split data into 1st set - 19th July to 6th Aug
system1 <- system_hourly[system_hourly$date>="2019-07-19" & system_hourly$date<="2019-08-06",]
# Split data into 2nd set - 1st to 18th July and 7th Aug to end
system2 <- system_hourly[system_hourly$date<"2019-07-19" | system_hourly$date>"2019-08-06",]

# Add charged and discharged energy; SoC; Light load
system1 <- system1 %>% mutate(Charged.energy_=ifelse(System.overview.Battery.Power.W_Kalman))
#**********************************************************************************************#

#**********************************************************************************************#
# Get typical hourly data and see how it varies over time
system <- gather(system_hourly, id, value, 4:16)
system_typical <- system %>% group_by(streetlight, timeUse, id) %>% summarise(value = mean(value, na.rm=TRUE))
system_typical <- as.data.frame(system_typical)
system_original <- system_typical[system_typical$id %in% original,]
system_imputed <- system_typical[system_typical$id %in% imputed,]

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
  labs(title="Typical day values for Rwanda SL: 01 Jul'19 to 30 Apr'20") 
ggsave(here(plot_dir,"typical_day_sl_all.png"))
#**********************************************************************************************#
