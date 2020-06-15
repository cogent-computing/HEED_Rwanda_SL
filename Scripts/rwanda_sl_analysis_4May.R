#****************************************************************************************************#
# Import packages
library(tidyverse)
library(lubridate)
library(readxl)
library(rlang)
library(openxlsx)
library(wesanderson)
#**********************************************************************************************#

#**********************************************************************************************#
# Read in the hourly data for full days and daytime days - all and 95% CI
setwd("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/")
system_hourly <- read.csv(file="System_hourly_fullday_95_jul_mar.csv", header=TRUE, stringsAsFactors = FALSE)
system_hourly2 <- read.csv(file="System_hourly_fullday_all_jul_mar.csv", header=TRUE, stringsAsFactors = FALSE)
system_hourly3 <- read.csv(file="System_hourly_daytime_95_jul_mar.csv", header=TRUE, stringsAsFactors = FALSE)
system_hourly4 <- read.csv(file="System_hourly_daytime_all_jul_mar.csv", header=TRUE, stringsAsFactors = FALSE)

system_hourly$timestamp <- as.POSIXct(system_hourly$timestamp, tz="GMT", origin="1970-01-01")
system_hourly$date <- date(system_hourly$timestamp)
system_hourly$timeUse <- hour(system_hourly$timestamp)
system_hourly$month <- as.character(month(system_hourly$timestamp, label=TRUE, abbr=TRUE))

system_hourly2$timestamp <- as.POSIXct(system_hourly2$timestamp, tz="GMT", origin="1970-01-01")
system_hourly2$date <- date(system_hourly2$timestamp)
system_hourly2$timeUse <- hour(system_hourly2$timestamp)
system_hourly2$month <- as.character(month(system_hourly2$timestamp, label=TRUE, abbr=TRUE))

system_hourly3$timestamp <- as.POSIXct(system_hourly3$timestamp, tz="GMT", origin="1970-01-01")
system_hourly3$date <- date(system_hourly3$timestamp)
system_hourly3$timeUse <- hour(system_hourly3$timestamp)
system_hourly3$month <- as.character(month(system_hourly3$timestamp, label=TRUE, abbr=TRUE))

system_hourly4$timestamp <- as.POSIXct(system_hourly4$timestamp, tz="GMT", origin="1970-01-01")
system_hourly4$date <- date(system_hourly4$timestamp)
system_hourly4$timeUse <- hour(system_hourly4$timestamp)
system_hourly4$month <- as.character(month(system_hourly4$timestamp, label=TRUE, abbr=TRUE))
#*********************************************************************************************#

#*********************************************************************************************#
# Read in the weather data - corrected and stitched in rwanda_sl_stitching_24Mar file.
setwd("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/")
weather_data <- read.csv("weather_hourly_jul_mar.csv", header=TRUE, stringsAsFactors = FALSE)
weather_data$timestamp <- as.POSIXct(weather_data$timestamp, tz="GMT", origin="1970-01-01")
weather_data <- gather(weather_data, "streetlight", "Potential_PV_power_W", 2:5)
weather_data$date <- date(weather_data$timestamp)
weather_data$month <- as.character(month(weather_data$date, label=TRUE, abbr=TRUE))
weather_data$timeUse <- hour(weather_data$timestamp)
#*********************************************************************************************#

#********************************************************************************************#
# Yield calculation - For each streetlight see what check for missing data - 1 Jul'19 to 30 Apr'20
all_days <- seq(as.Date("2019-07-01"), as.Date("2020-04-30"), by="days")
all_hours <- format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1),by = "1 hour"),"%H",tz="GMT")
all_hours <- all_hours[-25]

#Number of on hours per day for each streetlight id and each variable
sl_gather <- gather(system_hourly2, "variable", "value", 3:15)
# Yield excluding NA values for all variables
sl_on_hours <- sl_gather %>%
  group_by(streetlight, date, variable) %>%
  summarise(onHours = length(na.omit(value)))
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
sl_on_hours2$yield <- sl_on_hours2$onHours * 100.0 / 24.0

# Plot yield map for yield - yield calculated as % hours of total per day i.e. 24
pal <- wes_palette("Zissou1", 100, type = "continuous")
sl_on_hours2 <- sl_on_hours2[sl_on_hours2$variable=="Battery_power_W",]
sl_on_hours2 %>%
  ggplot(aes(date, streetlight)) + geom_tile(aes(fill = yield)) +
  scale_fill_gradientn(colours = pal) + 
  xlab("X axis") +
  ylab("Y axis") +
  labs(title="Yield for Rwanda Streetlights: 01 Jul'19 - 30 Apr'20" , 
       y="Streetlight", x = "Day of study",fill="Yield") 
ggsave("../Plots/Paper 7/yield_before_95.png")
#****************************************************************************************************#

#****************************************************************************************************#
# Mean hourly box plots for AC consumption per SL
plotACConsumption <- function(df, lim, br) {
  df %>%
    ggplot(aes(x=as.factor(timeUse), y=AC_consumption_W)) +
    geom_boxplot(aes(fill=timeUse))  +
    labs(y="AC consumption (W)", x = "Hour of day") + 
    theme(legend.position = "none") + 
    scale_y_continuous(limits=c(0, lim), breaks=seq(0,lim,by=br))
}

plotACConsumption(system_hourly2[system_hourly2$streetlight=="SL1",],100, 10) +
  labs(title="Mean hourly AC consumption of SL1 from 01 Jul'19 to 30 Apr'20") 
ggsave("../Plots/Paper 7/acLoad_sl1.png")
plotACConsumption(system_hourly2[system_hourly2$streetlight=="SL2",],120, 10) +
  labs(title="Mean hourly AC consumption of SL2 from 01 Jul'19 to 30 Apr'20")
ggsave("../Plots/Paper 7/acLoad_sl2.png")
plotACConsumption(system_hourly2[system_hourly2$streetlight=="SL3",], 300, 20) +
  labs(title="Mean hourly AC consumption of SL3 from 01 Jul'19 to 30 Apr'20")
ggsave("../Plots/Paper 7/acLoad_sl3.png")
plotACConsumption(system_hourly2[system_hourly2$streetlight=="SL4",], 70, 10) +
  labs(title="Mean hourly AC consumption of SL4 from 01 Jul'19 to 30 Apr'20")
ggsave("../Plots/Paper 7/acLoad_sl4.png")
#****************************************************************************************************#

#****************************************************************************************************#
# Daily AC consumption per SL since commissioning
system_gather <- gather(system_hourly2, "id", "value", 3:15) 
daily_system_hourly2 <- system_gather %>%
  group_by(streetlight, date, id) %>%
  summarise(value = sum(value, na.rm=TRUE))
daily_system_hourly2 <- as.data.frame(daily_system_hourly2)
daily_system_hourly2 <- spread(daily_system_hourly2, id, value)

plot_daily_ACLoad <- function(df, lim, br) {
  df %>%
    ggplot(aes(x=date, y=AC_consumption_W)) +
    geom_point(shape=4) +
    labs(y="AC consumption (W)", x = "DATE") + 
    theme(legend.position = "none", axis.text.x = element_text(angle = 45,size=8)) +
    scale_x_date(date_breaks ="2 weeks" ) +
    scale_y_continuous(limits=c(0, lim), breaks=seq(0,lim,by=br)) 
}

plot_daily_ACLoad(daily_system_hourly2[daily_system_hourly2$streetlight=="SL1",],100,10) +
  labs(title="Daily AC consumption of SL1 from 01 Jul'19 to 30 Apr'20")
ggsave("../Plots/Paper 7/daily_acLoad_sl1.png")
plot_daily_ACLoad(daily_system_hourly2[daily_system_hourly2$streetlight=="SL2",],300,20) +
  labs(title="Daily AC consumption of SL2 from 01 Jul'19 to 30 Apr'20")
ggsave("../Plots/Paper 7/daily_acLoad_sl2.png")
plot_daily_ACLoad(daily_system_hourly2[daily_system_hourly2$streetlight=="SL3",],300,20) +
  labs(title="Daily AC consumption of SL3 from 01 Jul'19 to 30 Apr'20")
ggsave("../Plots/Paper 7/daily_acLoad_sl3.png")
plot_daily_ACLoad(daily_system_hourly2[daily_system_hourly2$streetlight=="SL4",],200,10) +
  labs(title="Daily AC consumption of SL4 from 01 Jul'19 to 30 Apr'20")
ggsave("../Plots/Paper 7/daily_acLoad_sl4.png")
#****************************************************************************************************#

#****************************************************************************************************#
plotMeans <- function(df, lim1, lim2) {
  df %>%
    ggplot(aes(x=as.factor(timeUse), y=Socket_load_W)) +
    geom_boxplot(aes(fill=timeUse))  +
    labs(y="Power consumption (W)", x = "Hour of day") + 
    theme(legend.position = "none") 
  
    #scale_y_continuous(limits=c(lim1, lim2))
}

# Mean hourly box plots for System and socket load per month per SL - daytime data
# Box Plots for all SL1
plotMeans(system_hourly4[system_hourly4$streetlight=="SL1" & system_hourly4$month=="Jul",]) + 
  labs(title="Mean hourly system and socket load at SL1 in Jul 2019")
ggsave("../Plots/Paper 7/sl1_jul.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL1" & system_hourly4$month=="Aug",]) + 
  labs(title="Mean hourly system and socket load at SL1 in Aug 2019")
ggsave("../Plots/Paper 7/sl1_aug.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL1" & system_hourly4$month=="Sep",]) + 
  labs(title="Mean hourly system and socket load at SL1 in Sep 2019")
ggsave("../Plots/Paper 7/sl1_sep.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL1" & system_hourly4$month=="Oct",]) + 
  labs(title="Mean hourly system and socket load at SL1 in Oct 2019")
ggsave("../Plots/Paper 7/sl1_oct.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL1" & system_hourly4$month=="Nov",]) + 
  labs(title="Mean hourly system and socket load at SL1 in Nov 2019")
ggsave("../Plots/Paper 7/sl1_nov.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL1" & system_hourly4$month=="Dec",]) + 
  labs(title="Mean hourly system and socket load at SL1 in Dec 2019")
ggsave("../Plots/Paper 7/sl1_dec.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL1" & system_hourly4$month=="Jan",]) + 
  labs(title="Mean hourly system and socket load at SL1 in Jan 2020")
ggsave("../Plots/Paper 7/sl1_jan.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL1" & system_hourly4$month=="Feb",]) + 
  labs(title="Mean hourly system and socket load at SL1 in Feb 2020")
ggsave("../Plots/Paper 7/sl1_feb.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL1" & system_hourly4$month=="Mar",]) + 
  labs(title="Mean hourly system and socket load at SL1 in Mar 2020")
ggsave("../Plots/Paper 7/sl1_mar.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL1" & system_hourly4$month=="Apr",]) + 
  labs(title="Mean hourly system and socket load at SL1 in Apr 2020")
ggsave("../Plots/Paper 7/sl1_apr.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL1",]) + 
  labs(title="Mean hourly system and socket load at SL1 from Jul 2019 to Apr 2020")
ggsave("../Plots/Paper 7/sl1.png")

#### Box Plots for all SL2
plotMeans(system_hourly4[system_hourly4$streetlight=="SL2" & system_hourly4$month=="Jul",]) + 
  labs(title="Mean hourly system and socket load at SL2 in Jul 2019")
ggsave("../Plots/Paper 7/sl2_jul.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL2" & system_hourly4$month=="Aug",]) + 
  labs(title="Mean hourly system and socket load at SL2 in Aug 2019")
ggsave("../Plots/Paper 7/sl2_aug.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL2" & system_hourly4$month=="Sep",]) + 
  labs(title="Mean hourly system and socket load at SL2 in Sep 2019")
ggsave("../Plots/Paper 7/sl2_sep.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL2" & system_hourly4$month=="Oct",]) + 
  labs(title="Mean hourly system and socket load at SL2 in Oct 2019")
ggsave("../Plots/Paper 7/sl2_oct.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL2" & system_hourly4$month=="Nov",]) + 
  labs(title="Mean hourly system and socket load at SL2 in Nov 2019")
ggsave("../Plots/Paper 7/sl2_nov.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL2" & system_hourly4$month=="Dec",]) + 
  labs(title="Mean hourly system and socket load at SL2 in Dec 2019")
ggsave("../Plots/Paper 7/sl2_dec.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL2" & system_hourly4$month=="Jan",]) + 
  labs(title="Mean hourly system and socket load at SL2 in Jan 2020")
ggsave("../Plots/Paper 7/sl2_jan.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL2" & system_hourly4$month=="Feb",]) + 
  labs(title="Mean hourly system and socket load at SL2 in Feb 2020")
ggsave("../Plots/Paper 7/sl2_feb.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL2" & system_hourly4$month=="Mar",]) + 
  labs(title="Mean hourly system and socket load at SL2 in Mar 2020")
ggsave("../Plots/Paper 7/sl2_mar.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL2" & system_hourly4$month=="Apr",]) + 
  labs(title="Mean hourly system and socket load at SL2 in Apr 2020")
ggsave("../Plots/Paper 7/sl2_apr.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL2",]) + 
  labs(title="Mean hourly system and socket load at SL2 from Jul 2019 to Apr 2020")
ggsave("../Plots/Paper 7/sl2.png")

#### Box Plots for all SL3
plotMeans(system_hourly4[system_hourly4$streetlight=="SL3" & system_hourly4$month=="Jul",]) + 
  labs(title="Mean hourly system and socket load at SL3 in Jul 2019")
ggsave("../Plots/Paper 7/sl3_jul.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL3" & system_hourly4$month=="Aug",]) + 
  labs(title="Mean hourly system and socket load at SL3 in Aug 2019")
ggsave("../Plots/Paper 7/sl3_aug.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL3" & system_hourly4$month=="Sep",]) + 
  labs(title="Mean hourly system and socket load at SL3 in Sep 2019")
ggsave("../Plots/Paper 7/sl3_sep.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL3" & system_hourly4$month=="Oct",]) + 
  labs(title="Mean hourly system and socket load at SL3 in Oct 2019")
ggsave("../Plots/Paper 7/sl3_oct.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL3" & system_hourly4$month=="Nov",]) + 
  labs(title="Mean hourly system and socket load at SL3 in Nov 2019")
ggsave("../Plots/Paper 7/sl3_nov.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL3" & system_hourly4$month=="Dec",]) + 
  labs(title="Mean hourly system and socket load at SL3 in Dec 2019")
ggsave("../Plots/Paper 7/sl3_dec.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL3" & system_hourly4$month=="Jan",]) + 
  labs(title="Mean hourly system and socket load at SL3 in Jan 2020")
ggsave("../Plots/Paper 7/sl3_jan.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL3" & system_hourly4$month=="Feb",]) + 
  labs(title="Mean hourly system and socket load at SL3 in Feb 2020")
ggsave("../Plots/Paper 7/sl3_feb.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL3" & system_hourly4$month=="Mar",]) + 
  labs(title="Mean hourly system and socket load at SL3 in Mar 2020")
ggsave("../Plots/Paper 7/sl3_mar.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL3" & system_hourly4$month=="Apr",]) + 
  labs(title="Mean hourly system and socket load at SL3 in Apr 2020")
ggsave("../Plots/Paper 7/sl3_apr.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL3",]) + 
  labs(title="Mean hourly system and socket load at SL3 from Jul 2019 to Apr 2020")
ggsave("../Plots/Paper 7/sl3.png")

#### Box Plots for all SL4
plotMeans(system_hourly4[system_hourly4$streetlight=="SL4" & system_hourly4$month=="Jul",]) + 
  labs(title="Mean hourly system and socket load at SL4 in Jul 2019")
ggsave("../Plots/Paper 7/sl4_jul.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL4" & system_hourly4$month=="Aug",]) + 
  labs(title="Mean hourly system and socket load at SL4 in Aug 2019")
ggsave("../Plots/Paper 7/sl4_aug.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL4" & system_hourly4$month=="Sep",]) + 
  labs(title="Mean hourly system and socket load at SL4 in Sep 2019")
ggsave("../Plots/Paper 7/sl4_sep.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL4" & system_hourly4$month=="Oct",]) + 
  labs(title="Mean hourly system and socket load at SL4 in Oct 2019")
ggsave("../Plots/Paper 7/sl4_oct.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL4" & system_hourly4$month=="Nov",]) + 
  labs(title="Mean hourly system and socket load at SL4 in Nov 2019")
ggsave("../Plots/Paper 7/sl4_nov.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL4" & system_hourly4$month=="Dec",]) + 
  labs(title="Mean hourly system and socket load at SL4 in Dec 2019")
ggsave("../Plots/Paper 7/sl4_dec.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL4" & system_hourly4$month=="Jan",]) + 
  labs(title="Mean hourly system and socket load at SL4 in Jan 2020")
ggsave("../Plots/Paper 7/sl4_jan.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL4" & system_hourly4$month=="Feb",]) + 
  labs(title="Mean hourly system and socket load at SL4 in Feb 2020")
ggsave("../Plots/Paper 7/sl4_feb.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL4" & system_hourly4$month=="Mar",]) + 
  labs(title="Mean hourly system and socket load at SL4 in Mar 2020")
ggsave("../Plots/Paper 7/sl4_mar.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL4" & system_hourly4$month=="Mar",]) + 
  labs(title="Mean hourly system and socket load at SL4 in Mar 2020")
ggsave("../Plots/Paper 7/sl4_mar.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL4" & system_hourly4$month=="Apr",]) + 
  labs(title="Mean hourly system and socket load at SL4 in Apr 2020")
ggsave("../Plots/Paper 7/sl4_apr.png")

plotMeans(system_hourly4[system_hourly4$streetlight=="SL4",]) + 
  labs(title="Mean hourly system and socket load at SL4 from Jul 2019 to Apr 2020")
ggsave("../Plots/Paper 7/sl4.png")

#   Mean hourly box plots for System and socket load per month per SL - all day data
# Box Plots for all SL
plotMeans(system_hourly2[system_hourly2$streetlight=="SL1",]) + 
  labs(title="Mean hourly system and socket load at SL1 from Jul 2019 to Apr 2020")
ggsave("../Plots/Paper 7/sl1_fullday.png")

plotMeans(system_hourly2[system_hourly2$streetlight=="SL2",]) + 
  labs(title="Mean hourly system and socket load at SL2 from Jul 2019 to Apr 2020")
ggsave("../Plots/Paper 7/sl2_fullday.png")

plotMeans(system_hourly2[system_hourly2$streetlight=="SL3",]) + 
  labs(title="Mean hourly system and socket load at SL3 from Jul 2019 to Apr 2020")
ggsave("../Plots/Paper 7/sl3_fullday.png")

plotMeans(system_hourly2[system_hourly2$streetlight=="SL4",]) + 
  labs(title="Mean hourly system and socket load at SL4 from Jul 2019 to Apr 2020")
ggsave("../Plots/Paper 7/sl4_fullday.png")

#*********************************************************************************************#

#*********************************************************************************************#
# Get daily avg values for Irradiance, Potential PV, Actual PV power, Total load 
# Capture loss, System loss on full data days
sl_sub <- system_hourly2[,c(1,2,4,5,13,16,17,18)]
sl_sub$Positive_Battery_Watts <- ifelse(sl_sub$Solar_charger_Battery_watts_W>=0,
                                        sl_sub$Solar_charger_Battery_watts_W, 0)
sl_sub$Positive_Battery_Power <- ifelse(sl_sub$Battery_power_W>=0,
                                        sl_sub$Battery_power_W, 0)
sl_sub$Negative_Battery_Power <- ifelse(sl_sub$Battery_power_W<0,
                                        sl_sub$Battery_power_W, 0)
sl_sub <- sl_sub[complete.cases(sl_sub),]
full_days <- sl_sub %>%
  group_by(streetlight, date) %>%
  summarise(hours = length(timeUse))
full_days <- as.data.frame(full_days)
full_days <- full_days[full_days$hours==24,]

# REMOVE APRIL DATA FOR NOW
full_days <- full_days[full_days$date<"2020-04-01",]
#Subset data to get only full days
sl_full <- data.frame()
for(i in 1:length(unique(full_days$streetlight))) {
  df <- sl_sub[sl_sub$streetlight == unique(full_days$streetlight)[i], ]
  weather <- weather_data[weather_data$streetlight == unique(full_days$streetlight)[i], ]
  df_days <- full_days$date[full_days$streetlight == unique(full_days$streetlight)[i]]
  
  df <- df[df$date %in% df_days,]
  weather <- weather[weather$date %in% df_days,]
  df$Potential_PV_power_W <- weather$Potential_PV_power_W
  sl_full <- rbind(sl_full, df)
}

#Calculate Capture loss
sl_full$Capture_loss_W <- sl_full$Potential_PV_power_W - sl_full$PV_power_W

# Plotting box plots for Capture loss for SL4
ggplot(sl_full[sl_full$streetlight=="SL4",], aes(as.factor(timeUse),Capture_loss_W)) +
  geom_boxplot(aes(fill=timeUse))  +
  labs(title="Capture loss at SL4 from 1st July 2019 to 31st Mar 2020",y="Capture loss (W)", x = "Hour of day") + 
  theme(legend.position = "none") + 
  scale_y_continuous(limits=c(-225, 325), breaks=seq(-225,325,by=50)) 
ggsave("../Plots/Paper 7/Capture_loss_SL4.png")

# Saving the full data days
sl_write <- sl_full
sl_write <- sl_write[,-c(6:8)]
write.csv(sl_write, "~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/full_data_days.csv",
          row.names=FALSE)
###########################################################################################
#Calculate daily sum
sl_full <- gather(sl_full, "id", "value", c(3:5,9:13))
sl_full_daily <- sl_full %>%
  group_by(streetlight, month, date, id) %>%
  summarise(value = sum(value))
sl_full_daily <- as.data.frame(sl_full_daily)
sl_full_daily <- spread(sl_full_daily, id, value)

#Calculate Total Load
sl_full_daily$Total_load_W <- sl_full_daily$Positive_Battery_Watts

#Calculate System loss
sl_full_daily$System_loss_W <- sl_full_daily$PV_power_W - sl_full_daily$Total_load_W

# Typical day per month
sl_full_daily <- gather(sl_full_daily, "id", "value", c(4:13))
sl_full_typical <- sl_full_daily %>%
  group_by(streetlight, month, id) %>%
  summarise(value = mean(value))
sl_full_typical <- as.data.frame(sl_full_typical)

# Add year of study
sl_full_typical$year <- ifelse(sl_full_typical$month %in% c("Jul","Aug","Sep","Oct","Nov","Dec"),
                               2019,2020)
sl_full_typical$month2 <- ifelse(match(sl_full_typical$month, month.abb)<10,
                                 paste("0",as.character(match(sl_full_typical$month, month.abb)),sep=""), 
                                 as.character(match(sl_full_typical$month, month.abb)))
sl_full_typical$month2 <- paste(sl_full_typical$year, sl_full_typical$month2, sep="-")

# Bar plot for daily avg per month
sl <- sl_full_typical[sl_full_typical$id=="Capture_loss_W" | 
                        sl_full_typical$id=="Total_load_W" | sl_full_typical$id=="System_loss_W", ]
sl$value[sl$id=="Total_load_W"] <- abs(sl$value[sl$id=="Total_load_W"])
plotSL <- function(df) {
  df %>%
    ggplot(aes(x = month2, y= value, fill = id)) +
    geom_bar(stat="identity", width=.3, position = "stack") + 
    labs(y="Consumed and potential electrical energy (Wh)",
         x = "Month",
         fill="Parameter")  +
    theme(legend.position = "bottom", legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
          legend.margin = margin(t=0,r=0,b=0,l=0), axis.text.x = element_text(angle = 0))
}

plotSL(sl[sl$streetlight=="SL1",]) + 
  labs(title="Daily average electrical energy values at SL1: 1st Jul'19 to 31 Mar'20")
ggsave("../Plots/Paper 7/barPlot_sl1.png")

plotSL(sl[sl$streetlight=="SL2",]) + 
  labs(title="Daily average electrical energy values at SL2: 1st Jul'19 to 31 Mar'20")
ggsave("../Plots/Paper 7/barPlot_sl2.png")

plotSL(sl[sl$streetlight=="SL3",]) + 
  labs(title="Daily average electrical energy values at SL3: 1st Jul'19 to 31 Mar'20")
ggsave("../Plots/Paper 7/barPlot_sl3.png")

plotSL(sl[sl$streetlight=="SL4",]) + 
  labs(title="Daily average electrical energy values at SL4: 1st Jul'19 to 31 Mar'20")
ggsave("../Plots/Paper 7/barPlot_sl4.png")

# Putting the data together for the summary table
# For each SL get the number of full days per month from sl_full_daily
sl_days <- sl_full_daily[,-c(4:5)] %>%
  group_by(streetlight, month) %>%
  summarise(full_days = length(unique(date)))
sl_days <- as.data.frame(sl_days)

# Using sl_full_typical
sl <- sl_full_typical
sl$value <- sl$value / 1000.0 #Converitng to kWh
sl <- spread(sl, id, value)
sl$Irradiance_kWhm2 <- sl$Potential_PV_power_W / (1.94 * 0.1649)
#sl$System_efficiency <- sl$Total_load_W / (sl$Potential_PV_power_W / 0.152)
sl$Full_days <- sl_days$full_days
sl <- sl[order(sl$streetlight, sl$month2),]
sl <- sl[,c(1,2,16,15,10,11,14,13,6)]
colnames(sl) <- c("Streetlight","Month", "Full_days","Irradiance_kWh/m2","Potential_PV_power_kWh",
                  "Actual_PV_power_kWh","Total_load_kWh","System_losses_kWh","Capture_losses_kWh")
write.csv(sl,"~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/Data_summary_Rwanda.csv",
          row.names=FALSE)
#*********************************************************************************************#

#*********************************************************************************************#
# Daily AC consumption for full data days per streetlight
# Subset data to see full days for which we have AC_consumption values
sl_sub <- system_hourly2[,c(1,2,3,16,17,18)]
sl_sub <- sl_sub[complete.cases(sl_sub),]
full_days <- sl_sub %>%
  group_by(streetlight, date) %>%
  summarise(hours = length(timeUse))
full_days <- as.data.frame(full_days)
full_days <- full_days[full_days$hours==24,]

# Subset data to get only full days
sl_full <- data.frame()
for(i in 1:length(unique(full_days$streetlight))) {
  df <- sl_sub[sl_sub$streetlight == unique(full_days$streetlight)[i], ]
  df_days <- full_days$date[full_days$streetlight == unique(full_days$streetlight)[i]]
  df <- df[df$date %in% df_days,]
  sl_full <- rbind(sl_full, df)
}

# Calculate daily sum
sl_full <- gather(sl_full, "id", "value", 3)
sl_daily <- sl_full %>%
  group_by(streetlight, month, date, id) %>%
  summarise(value = sum(value))
sl_daily <- as.data.frame(sl_daily)
sl_daily <- spread(sl_daily, id, value)

plot_daily_ACLoad(sl_daily[sl_daily$streetlight=="SL1",],50,10) +
  labs(title="Daily AC consumption of SL1 for full data days from 01 Jul'19 to 30 Apr'20")
ggsave("../Plots/Paper 7/acLoad_full_days_sl1.png")
plot_daily_ACLoad(sl_daily[sl_daily$streetlight=="SL2",],220,20) +
  labs(title="Daily AC consumption of SL2 for full data days from 01 Jul'19 to 30 Apr'20")
ggsave("../Plots/Paper 7/acLoad_full_days_sl2.png")
plot_daily_ACLoad(sl_daily[sl_daily$streetlight=="SL3",],100,10) +
  labs(title="Daily AC consumption of SL3 for full data days from 01 Jul'19 to 30 Apr'20")
ggsave("../Plots/Paper 7/acLoad_full_days_sl3.png")
plot_daily_ACLoad(sl_daily[sl_daily$streetlight=="SL4",],200,10) +
  labs(title="Daily AC consumption of SL4 for full data days from 01 Jul'19 to 30 Apr'20")
ggsave("../Plots/Paper 7/acLoad_full_days_sl4.png")
#*********************************************************************************************#

#*********************************************************************************************#
# Daytime system and socket consumption for full daytime days
# Subset data to see full days for which we have AC_consumption values
sl_sub <- system_hourly4[,c(1,2,14,16,17,18)]
sl_sub <- sl_sub[complete.cases(sl_sub),]
full_days <- sl_sub %>%
  group_by(streetlight, date) %>%
  summarise(hours = length(timeUse))
full_days <- as.data.frame(full_days)
full_days <- full_days[full_days$hours==12,]

# Subset data to get only full days
sl_full <- data.frame()
for(i in 1:length(unique(full_days$streetlight))) {
  df <- sl_sub[sl_sub$streetlight == unique(full_days$streetlight)[i], ]
  df_days <- full_days$date[full_days$streetlight == unique(full_days$streetlight)[i]]
  df <- df[df$date %in% df_days,]
  sl_full <- rbind(sl_full, df)
}

# Calculate daily sum
sl_full <- gather(sl_full, "id", "value", 3)
sl_daytime <- sl_full %>%
  group_by(streetlight, month, date, id) %>%
  summarise(value = sum(value))
sl_daytime <- as.data.frame(sl_daytime)
sl_daytime <- spread(sl_daytime, id, value)

plot_daily_sysSocketLoad <- function(df, lim, br) {
  df %>%
    ggplot(aes(x=date, y=Socket_load_W)) +
    geom_point(shape=4) +
    labs(y="Socket load (W)", x = "DATE") + 
    theme(legend.position = "none", axis.text.x = element_text(angle = 45,size=8)) +
    scale_x_date(date_breaks ="2 weeks" ) +
    scale_y_continuous(limits=c(0, lim), breaks=seq(0,lim,by=br)) 
}

plot_daily_sysSocketLoad(sl_daytime[sl_daytime$streetlight=="SL1",],450,50) +
  labs(title="Daily System and Socket Load at SL1 for full daytime data days from 01 Jul'19 to 30 Apr'20")
ggsave("../Plots/Paper 7/socketLoad_sl1.png")
plot_daily_sysSocketLoad(sl_daytime[sl_daytime$streetlight=="SL2",],700,50) +
  labs(title="Daily System and Socket Load at SL2 for full daytime data days from 01 Jul'19 to 30 Apr'20")
ggsave("../Plots/Paper 7/socketLoad_sl2.png")
plot_daily_sysSocketLoad(sl_daytime[sl_daytime$streetlight=="SL3",],300,20) +
  labs(title="Daily System and Socket Load at SL3 for full daytime data days from 01 Jul'19 to 30 Apr'20")
ggsave("../Plots/Paper 7/socketLoad_sl3.png")
plot_daily_sysSocketLoad(sl_daytime[sl_daytime$streetlight=="SL4",],650,50) +
  labs(title="Daily System and Socket Load at SL4 for full daytime data days from 01 Jul'19 to 30 Apr'20")
ggsave("../Plots/Paper 7/socketLoad_sl4.png")
#*********************************************************************************************#

#*********************************************************************************************#
# Typical hourly values to calculate daily avg bar plot
# Get daily avg values for Potential PV, Actual PV power, Total load 
# Capture loss, System loss on full data days
sl_sub <- system_hourly2[,c(1,2,4,5,13,16,17,18)]
sl_sub$Positive_Battery_Watts <- ifelse(sl_sub$Solar_charger_Battery_watts_W>=0,
                                        sl_sub$Solar_charger_Battery_watts_W, 0)
sl_sub$Positive_Battery_Power <- ifelse(sl_sub$Battery_power_W>=0,
                                        sl_sub$Battery_power_W, 0)
sl_sub$Negative_Battery_Power <- ifelse(sl_sub$Battery_power_W<0,
                                        sl_sub$Battery_power_W, 0)
sl_sub <- gather(sl_sub, "id", "value", c(3:5,9:11))

sl_typical_hourly <- sl_sub %>%
  group_by(streetlight, month, timeUse, id) %>%
  summarise(value = mean(value, na.rm=TRUE))
sl_typical_hourly <- as.data.frame(sl_typical_hourly)
sl_typical_hourly <- spread(sl_typical_hourly, id, value)

weather_typical_hourly <- weather_data %>%
  group_by(streetlight, month, timeUse) %>%
  summarise(value=mean(Potential_PV_power_W, na.rm=TRUE))
weather_typical_hourly <- as.data.frame(weather_typical_hourly)
weather_typical_hourly <- weather_typical_hourly[!(weather_typical_hourly$month=="Jun" | 
                                                   weather_typical_hourly$month=="May"),]

# Add potential PV power
sl_typical_hourly$Potential_PV_power_W <- weather_typical_hourly$value

# Calculate Capture loss
sl_typical_hourly$Capture_loss_W <- sl_typical_hourly$Potential_PV_power_W - 
                                      sl_typical_hourly$PV_power_W

# Calculate daily sum
sl_full <- gather(sl_typical_hourly, "id", "value", c(4:11))
sl_typical_daily <- sl_full %>%
  group_by(streetlight, month, id) %>%
  summarise(value = sum(value))
sl_typical_daily <- as.data.frame(sl_typical_daily)
sl_typical_daily <- spread(sl_typical_daily, id, value)

#Calculate Total Load
sl_typical_daily$Total_load_W <- sl_typical_daily$Positive_Battery_Watts - 
  (sl_typical_daily$Positive_Battery_Power - sl_typical_daily$Negative_Battery_Power)

#Calculate System loss
sl_typical_daily$System_loss_W <- sl_typical_daily$PV_power_W - abs(sl_typical_daily$Total_load_W)

# Add year of study
sl_typical_daily$year <- ifelse(sl_typical_daily$month %in% c("Jul","Aug","Sep","Oct","Nov","Dec"),
                               2019,2020)
sl_typical_daily$month2 <- ifelse(match(sl_typical_daily$month, month.abb)<10,
                                 paste("0",as.character(match(sl_typical_daily$month, month.abb)),sep=""), 
                                 as.character(match(sl_typical_daily$month, month.abb)))
sl_typical_daily$month2 <- paste(sl_typical_daily$year, sl_typical_daily$month2, sep="-")

# Bar plot for daily avg per month
sl_typical_daily <- gather(sl_typical_daily, "id", "value",c(3:12))
sl <- sl_typical_daily[sl_typical_daily$id=="Capture_loss_W" | 
                      sl_typical_daily$id=="Total_load_W" | sl_typical_daily$id=="System_loss_W", ]
sl$value[sl$id=="Total_load_W"] <- abs(sl$value[sl$id=="Total_load_W"])
plotSL <- function(df) {
  df %>%
    ggplot(aes(x = month2, y= value/1000.0, fill = id)) +
    geom_bar(stat="identity", width=.3, position = "stack") + 
    labs(y="Consumed and potential electrical energy (kWh)",
         x = "Month",
         fill="Parameter")  +
    theme(legend.position = "bottom", legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
          legend.margin = margin(t=0,r=0,b=0,l=0), axis.text.x = element_text(angle = 0))
}

plotSL(sl[sl$streetlight=="SL1",]) + 
  labs(title="Daily average electrical energy values at Nepal SL1: 1st Jul'19 to 31 Mar'20")
ggsave("../Plots/Paper 7/barPlot_sl1.png")

plotSL(sl[sl$streetlight=="SL2",]) + 
  labs(title="Daily average electrical energy values at Nepal SL2: 1st Jul'19 to 31 Mar'20")
ggsave("../Plots/Paper 7/barPlot_sl2.png")

plotSL(sl[sl$streetlight=="SL3",]) + 
  labs(title="Daily average electrical energy values at Nepal SL3: 1st Jul'19 to 31 Mar'20")
ggsave("../Plots/Paper 7/barPlot_sl3.png")

plotSL(sl[sl$streetlight=="SL4",]) + 
  labs(title="Daily average electrical energy values at Nepal SL4: 1st Jul'19 to 31 Mar'20")
ggsave("../Plots/Paper 7/barPlot_sl4.png")

#*********************************************************************************************#