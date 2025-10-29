require(data.table)
require(fst)
require(lubridate)
require(dplyr)
require(tidyverse)
library(MMWRweek)


# key note is that daymet for V3 and beyond that day is defined at the local time
# from midnight to midnight. Need to link other datasets accordingly.

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/daymet")
nycha_daymet <- fread("daymet-nycha-2016-2018.csv")

nycha_daymet <-  nycha_daymet %>%
  # Separate date
  separate("system:index", into = c("date", NA), sep = "_") %>%
  # Convert date and aggregate to building level
  mutate(date = ymd(date)) %>%
  group_by(tds_nmb, bldng_n, date) %>%
  summarise(
    precip_building = mean(prcp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Add MMWR week variables
  mutate(
    MMWRweek(date),
    year_week_date = MMWRweek2Date(MMWRyear, MMWRweek, 1)
  ) %>%
  # Select relevant columns and ensure distinct rows
  distinct(tds_nmb, bldng_n, date, precip_building, MMWRweek, MMWRyear, year_week_date)

write_fst(nycha_daymet, "daymet-nycha-bin-PRE.fst")
