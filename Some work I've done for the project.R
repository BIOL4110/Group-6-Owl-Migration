#You will have to import the data, I did it manually but that's your call 
library(tidyverse)
snowy <- Ebird_snowy_owl_Canada

#Converting the observation and date data into numeric and date formats
snowy$OBSERVATION.COUNT <- as.numeric(snowy$OBSERVATION.COUNT) 
snowy$OBSERVATION.DATE <- as.Date(snowy$OBSERVATION.DATE, format = "%Y-%m-%d")

#Adding a year column and filtering the range of data included from 2000 to 2020 
snowy <- snowy %>%
  mutate(year = year(OBSERVATION.DATE))
snowy <- snowy %>%
  filter(year >= 2000 & year <= 2020)

#Creating a column for latitude ranges (I only )
snowy <- snowy %>%
  mutate(lat_bin = cut(LATITUDE,
                       breaks = seq(min(41), max(82), by = 1),
                       labels = seq(41, 81, by = 1),
                       include.lowest = TRUE))

#Packages for working with dates
library(dplyr)
library(lubridate)

#Creating month periods that mark breeding and non-breeding timespans
northern_months <- 5:10
southern_months <- c(11, 12, 1, 2,3, 4)

#Separating the row into the two breeding and non-breeding parts 
snowy <- snowy %>%
  mutate(month = month(OBSERVATION.DATE),
         location_period = case_when(
           month %in% northern_months ~ "North",
           month %in% southern_months ~ "South",
           TRUE ~ "Other"))

#Observations that occured in the breeding months put into latitude bins ranging from the lowest to the highest latitudes of observations
north_lat_abundance_by_year <- snowy %>%
  filter(location_period == "North") %>%
  mutate(lat_bin = cut(LATITUDE,
                       breaks = seq(min(41), max(82), by = 1),
                       labels = seq(41, 81, by = 1),
                       include.lowest = TRUE)) %>%
  group_by(lat_bin, year) %>%
  summarise(total_count = sum(OBSERVATION.COUNT, na.rm = TRUE))

#Observations that occured in the non-breeding months
south_lat_abundance_by_year <- snowy %>%
  filter(location_period == "South") %>%
  mutate(lat_bin = cut(LATITUDE,
                       breaks = seq(min(41), max(82), by = 10),
                       labels = seq(41, 81, by = 10),
                       include.lowest = TRUE)) %>%
  group_by(lat_bin, year) %>%
  summarise(total_count = sum(OBSERVATION.COUNT, na.rm = TRUE))
