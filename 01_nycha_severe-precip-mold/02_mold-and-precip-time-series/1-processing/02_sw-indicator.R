### The objective of this code is to create extreme precipitation and heat 
### indicators for weeks/buildings that are above the 99th/95th percentile.

### To do this, we are using historical daymet data from 1980-2010 to get a weekly
### distribution from which we can identify the 99th/95th percentile. Then, we are using the
### daymet data from 2016-2018 to identify building-weeks that are above the 
### 99th/95th percentile.

### ASK joan thoughts on keeping rad/pact data in this exposure assignment.
### shouldnt matter since this assignment is location specific


# Load required libraries
library(data.table)
library(fst)
library(lubridate)
library(tidyverse)
library(dplyr)

# Set working directory
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/DAYMET-2016-2018/historical") 

nycha_daymet <- read.fst("daymet-nycha-historical-bin-clean.fst") 

head(nycha_daymet)

# Create weekly indicator
week_99th <- nycha_daymet %>%
  group_by(tds_nmb, bldng_n, MMWRweek, MMWRyear) %>% ### first calculate the building aggregate values - already aggregated bins to buildings
  mutate(
    sum_week_precip = sum(precip_building),
    max_week_temp = mean(tmax_building),
    min_week_temp = mean(tmin_building)
  ) %>% ### now calculate the 99th and 95th percentile for each week
  ungroup() %>%
  group_by(tds_nmb, bldng_n) %>%
  mutate(
    week_99th_precip = quantile(sum_week_precip, .99),
    week_99th_max_temp = quantile(max_week_temp, .99),
    week_99th_min_temp = quantile(min_week_temp, .99), 
    week_95th_precip = quantile(sum_week_precip, .95),
    week_95th_max_temp = quantile(max_week_temp, .95),
    week_95th_min_temp = quantile(min_week_temp, .95)
  ) %>%
  select(week_99th_precip, week_99th_max_temp, week_99th_min_temp,week_95th_precip,week_95th_max_temp, week_95th_min_temp) %>%
  unique() %>%
  ungroup()

# Read in 2021-2023 data
setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/DAYMET-2016-2018")
nycha_daymet_2021_2023 <- read.fst("daymet-nycha-bin-POST.fst") 

# Create building weekly variable
nycha_daymet_2021_2023 <- nycha_daymet_2021_2023 %>%
  group_by(tds_nmb, bldng_n, MMWRweek, MMWRyear, year_week_date) %>% ### first calculate the building-week aggregate values
  mutate(
    sum_week_precip = sum(precip_building), # should be the building variables
    max_week_temp = mean(tmax_building),
    min_week_temp = mean(tmin_building)
    
  ) %>%
  ungroup()


# Join indicators to data
nycha_daymet_2021_2023_indicators <- nycha_daymet_2021_2023 %>%
  left_join(week_99th, by = c(  "tds_nmb", "bldng_n")) %>%
  group_by(tds_nmb, bldng_n, MMWRweek, MMWRyear) %>%
  mutate(
    week_99th_ind_precip = ifelse(sum_week_precip >= week_99th_precip , 1, 0),
    week_99th_ind_temp_max = ifelse(max_week_temp >= week_99th_max_temp , 1, 0),
    week_99th_ind_temp_min = ifelse(min_week_temp >=  week_99th_min_temp, 1, 0),
    week_95th_ind_precip = ifelse(sum_week_precip >= week_95th_precip , 1, 0),
    week_95th_ind_temp_max = ifelse(max_week_temp >= week_95th_max_temp , 1, 0),
    week_95th_ind_temp_min = ifelse(min_week_temp >=  week_95th_min_temp, 1, 0))%>%
  select(-date, -precip_building,  -tmax_building, -tmin_building) %>% ### remove sub-weekly information
  unique()

# Write out data
write_fst(nycha_daymet_2021_2023_indicators, "daymet-nycha-indicators-POST.fst")


# nycha_daymet_2021_2023_indicators %>%
#    ungroup() %>%
#    select(tds_nmb, bldng_n) %>%
#    n_distinct()
