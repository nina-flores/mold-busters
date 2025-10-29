# Load required libraries
library(fst)
library(lubridate)
library(tidyverse)
library(MMWRweek)

setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/DAYMET-2016-2018/historical") 

#Read data
nycha_daymet <- read.fst("daymet-nycha-historical-bin.fst")

head(nycha_daymet)

# Clean up and create week variable
nycha_daymet_clean <- nycha_daymet %>%
  # Separate date
  separate("system:index", into = c("date", NA), sep = "_") %>%
  # Convert date and aggregate to building level
  mutate(date = ymd(date)) %>%
  group_by(tds_nmb, bldng_n, date) %>%
  summarise(
    precip_building = mean(prcp, na.rm = TRUE),
    tmax_building = mean(tmax, na.rm = TRUE),
    tmin_building = mean(tmin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Add MMWR week variables
  mutate(
    MMWRweek(date),
    year_week_date = MMWRweek2Date(MMWRyear, MMWRweek, 1)
  ) %>%
  # Select relevant columns and ensure distinct rows
  distinct(tds_nmb, bldng_n, date, precip_building, tmax_building, tmin_building, MMWRweek, MMWRyear, year_week_date)

# Write output
write_fst(nycha_daymet_clean, "daymet-nycha-historical-bin-clean.fst")

