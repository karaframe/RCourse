
# time series with hourly UAE pollution data

library(threadr)
library(dygraphs)
library(tidyr)
library(leaflet)
library(readr)
library(lubridate)
library(ggplot2)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Rcourse")

# NCMS <- read_csv("export_dataset_NCMS_1_Quar_2015.csv")
NCMS <- read.csv("database_NCMS_2016_daily_mean.csv")
# replace NaN (not a Number with NA that is a missing value)
NCMS[sapply(NCMS,is.na)] = NA 


NCMS <- NCMS %>%
#  mutate(date = ymd(Date, tz = "UTC", locale = Sys.getlocale("LC_TIME")))
  mutate(date = ymd(Date, tz = "UTC")) 


data_time <- NCMS %>%
  filter(Site == "Al Hamriyah") %>%
  select(date,
         Site,
         Pollutant,
         Daily_mean) 

# remove line with date == NA
# data_time <- data_time[!is.na(data_time$date),]

data_time <- data_time %>%
  spread(Pollutant, Daily_mean)

# Build timeseries for plots
time_series <- data_frame_to_timeseries(data_time)

# Return
time_series

# make interactive time-series plot
colour_vector <- threadr::ggplot2_colours(45)

plot <- dygraph(time_series$NO2) %>% 
  dyOptions(colors = colour_vector[1]) %>% 
  dySeries(label = "NO2") %>% 
  dyAxis("y", label = "Daily NO<sub>2</sub> (&#956;g m<sup>-3</sup>)") %>% 
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




