
# time series with hourly UAE pollution data

library(threadr)
library(dygraphs)
library(tidyr)
library(leaflet)
library(readr)
library(lubridate)
library(ggplot2)

# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data")
# setwd("E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/Interactive_plots_R")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Rcourse")
# setwd("D:/AQ_data/hourly_FK_new/hourly_data/test_FK")

# NCMS <- read_csv("database_NCMS_2016_hourly.csv")
EAD <- read_csv("database_EAD data 2016_hourly.csv")

# replace NaN (not a Number with NA that is a missing value)
EAD[sapply(EAD,is.na)] = NA 

EAD <- EAD %>%
mutate(date = ymd_hms(DateTime, tz = "UTC"))
  
# shift time back of 4 hours (UAE time)    
EAD <- EAD %>%
    mutate(date = date - 14400)                

 # EAD$date <- (EAD$date) - 4*60*60 #### 3 hours

data_time <- EAD %>%
  filter(Site == "Bain Aljesrain") %>%
  select(date,
         Site,
         Pollutant,
         Value) 

# remove line with date == NA
# data_time <- data_time[!is.na(data_time$date),]

# spread values to have one column for each pollutant
data_time <- data_time %>%
  spread(Pollutant, Value)

############################################################################
## static time series ######################################################
###################################################################################################################
######### plot TIME-SERIES of AQ PM10 data and WRF PM10 data ######################################################
###################################################################################################################


jpeg('time_series_hourly.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

min <- as.POSIXct("2016-03-01 09:00:00") 
max <- as.POSIXct("2016-10-03 22:00:00") 

plot <- ggplot(data_time, aes(date, value)) + 
  theme_bw() +
  geom_line(aes(y = PM2.5, col = "PM2.5"), alpha=1, col="red") +
  geom_line(aes(y = PM10, col = "PM10"), alpha=1, col="blue") +
#  facet_wrap( ~ Site, ncol=4) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste(PM[10], " (µg/",m^3, ")", " AQ & WRFChem (hourly)"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=14, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14, colour = "black")) +
  ylim(0, 3000) +
  xlim(min, max)
plot


par(oldpar)
dev.off()




#############################################################################
## dynamic time series ######################################################



# Build timeseries for plots
time_series <- data_frame_to_timeseries(data_time)

# Return
time_series

# make interactive time-series plot
colour_vector <- threadr::ggplot2_colours(45)


plot <- dygraph(time_series$`Lower.Ambient.Temperature`) %>% 
  dyOptions(colors = colour_vector[1]) %>% 
  dySeries(label = "Lower Ambient Temperature") %>% 
  dyAxis("y", label = "Hourly Temp. <sup>o</sup>C") %>% 
  dyRangeSelector()
plot


plot <- dygraph(time_series$PM10) %>% 
  dyOptions(colors = colour_vector[1]) %>% 
  dySeries(label = "PM10") %>% 
  dyAxis("y", label = "Daily PM<sub>10</sub> (&#956;g m<sup>-3</sup>)") %>% 
  dyRangeSelector()
plot

plot <- dygraph(time_series$PM10) %>% 
  dyOptions(colors = colour_vector[1]) %>% 
  dySeries(label = "PM10") %>% 
  dyAxis("y", label = "Daily PM<sub>10</sub> (&#956;g m<sup>-3</sup>)") %>% 
  dyRangeSelector()
plot


plot <- dygraph(time_series$SO2) %>% 
  dyOptions(colors = colour_vector[1]) %>% 
  dySeries(label = "SO2") %>% 
  dyAxis("y", label = "Daily SO<sub>2</sub> (&#956;g m<sup>-3</sup>)") %>% 
  dyRangeSelector()
plot




