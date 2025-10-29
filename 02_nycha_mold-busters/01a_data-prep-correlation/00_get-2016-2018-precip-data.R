### The objective of this code is to create extreme precipitation and heat 
### indicators for weeks/buildings that are above the 95th percentile.

### To do this, we are using historical daymet data from 1980-2010 to get a weekly
### distribution from which we can identify the 95th percentile. Then, we are using the
### daymet data from 2016-2018 to identify building-weeks that are above the 
### 95th percentile.

# for simplicity we just use 2.6 inches since that was the lower bound of 95th 
# percentile in the otehr analysis.

# Load required libraries
library(data.table)
library(fst)
library(lubridate)
library(tidyverse)
library(dplyr)


# Read in 2021-2023 data
setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/daymet")
nycha_daymet_2016_2018 <- read.fst("daymet-nycha-bin-PRE.fst") 

# Create building weekly variable
nycha_daymet_2016_2018 <- nycha_daymet_2016_2018 %>%
  group_by(tds_nmb, bldng_n, MMWRweek, MMWRyear, year_week_date) %>% ### first calculate the building-week aggregate values
  mutate(
    sum_week_precip = sum(precip_building)) %>%
  ungroup()


# Join indicators to data
nycha_daymet_2016_2018_indicators <- nycha_daymet_2016_2018 %>%
  group_by(tds_nmb, bldng_n, MMWRweek, MMWRyear) %>%
  mutate(
    week_95th_ind_precip = ifelse(sum_week_precip >= 66.04 , 1, 0))%>%
  select(-date, -precip_building) %>% ### remove sub-weekly information
  unique()

# Write out data
write_fst(nycha_daymet_2016_2018_indicators, "daymet-nycha-indicators-PRE.fst")


