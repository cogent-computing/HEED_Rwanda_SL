#******************************************************************************************#
# This is the script for plotting typical day behaviour using sim data for Nepal & Rwanda  #
# Author: K Bhargava                                                                       #
# Last updated on: 16th Jul 2020                                                           #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(wesanderson)
library(extrafont)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Define macros - theme for all plots
font_import()
THEME <- theme(legend.position = "bottom", legend.text=element_text(size=10, family="Times New Roman"),
               legend.key.size = unit(0.5, "cm"),legend.margin = margin(t=0,r=0,b=0,l=0), 
               panel.grid.major.y = element_line(colour="grey"), 
               panel.grid.minor = element_blank(), panel.background = element_blank(), 
               axis.line = element_line(colour = "black"), 
               axis.text = element_text(size=9, family="Times New Roman"),
               axis.title = element_text(size=10, family="Times New Roman")) 
#******************************************************************************************#

#******************************************************************************************#
# Defining macros
EFF <- c(1.968*0.152, 1.94*0.1649)
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data/Simulated_data"
output_dir <- "Data"
file_list <- list.files(here(filepath))
plot_dir <- "Plots/Paper 7"
#******************************************************************************************#

#******************************************************************************************#
# Read simulated data
system <- data.frame()
for(i in seq_along(file_list)) {
  headers <- read_csv(here(filepath,file_list[i]), col_names = FALSE, na="..", n_max = 2)
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  
  df <- read_csv(here(filepath,file_list[i]), col_names = headers, na="..", skip = 2)
  df <- df %>% mutate(`Time `=as.POSIXct(`Time `, tz="GMT",origin="1970-01-01",format="%d/%m/%Y %H:%M"),
                      month=as.character(month(`Time `, label=TRUE, abbr=TRUE)), timestamp=hour(`Time `),
                      country=substr(file_list[i], 1, 6))
  
  # Consider months Jan to Mar and July to Dec
  df <- df[!(df$month=="Apr" | df$month=="May" | df$month=="Jun"),]
  
  # Select columns needed
  df <- df[,c(37,35,36,6,7,12,14,24,25,28)]
  
  #Bind data
  system <- rbind(system, df)
}
write.csv(system, file=here(output_dir,"simulated_hourly_jul_mar.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Calculate variables needed
system <- read_csv(here(output_dir,"simulated_hourly_jul_mar.csv"), col_names=TRUE)
system_sub <- data.frame()
for(i in 1:2) {
  df <- system[system$country==unique(system$country)[i],]
  df <- df %>% mutate(E_p = `Generic flat plate PV Incident Solar kW/m2`*EFF[i],
                      B_cp = `Generic 1kWh Li-Ion Charge Power kW`,
                      B_dp = `Generic 1kWh Li-Ion Discharge Power kW`,
                      E_a = `Generic flat plate PV Power Output kW` - `Excess Electrical Production kW`,
                      E_load = `Total Electrical Load Served kW`, SoC=`Generic 1kWh Li-Ion State of Charge %`,
                      L_c=E_p-E_a)
  df <- df[,c(1,3,11:17)]
  system_sub <- rbind(system_sub, df)
}

# Subtract 0.0025 from E_load values
system_sub <- system_sub %>% mutate(E_load = E_load - 0.0025)

# Calculate typical data for each country
system_sub <- gather(system_sub,id, value, 3:9)
system_typical <- system_sub %>% group_by(country, timestamp, id) %>% summarise(value=mean(value, na.rm=TRUE))
system_typical <- as.data.frame(system_typical)
system_typical <- spread(system_typical, id, value)

# Plot typical values for Nepal and Rwanda 
plotTypical <- function(df) {
  ggplot(df, aes(x=timestamp)) + geom_line(aes(y=B_cp, color="B_cp", linetype="B_cp")) +
    geom_line(aes(y=B_dp, color="B_dp",linetype="B_dp")) + geom_line(aes(y=E_a, color="E_a",linetype="E_a")) +
    geom_line(aes(y=E_load, color="E_load",linetype="E_load")) + geom_line(aes(y=E_p, color="E_p",linetype="E_p")) +
    geom_line(aes(y=L_c, color="L_c",linetype="L_c")) + geom_line(aes(y = SoC/400, color = "SoC",linetype="SoC"))+ 
    scale_y_continuous(breaks= seq(0,0.25,0.05), sec.axis = sec_axis(~.*400, name = "State of Charge (%)")) +
    labs(y="Power (kW)", x = "Time of day", colour="", linetype="") +
    scale_x_continuous(breaks=seq(0,24,by=2)) + THEME
}
# labs(title="Simulated typical day profile for Nepal SL between Jul 2019 and Mar 2020")
plotTypical(system_typical[system_typical$country=="Nepal",]) 
ggsave(here(plot_dir,"typical_day_nepal_sl_sim.pdf"), width = 8, height = 6, units = "cm")
# labs(title="Simulated typical day profile for Rwanda SL between Jul 2019 and Mar 2020")
plotTypical(system_typical[system_typical$country=="Rwanda",]) 
ggsave(here(plot_dir,"typical_day_rwanda_sl_sim.pdf"), width = 8, height = 6, units = "cm")
#******************************************************************************************#