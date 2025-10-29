# the purpose of this script is to join the building characteristics
# and mold work orders to the
# dataset 02_sw_indicator.R and to produce a clean
# dataset for analysis.

#load packages
library(fst)
library(lubridate)
library(tidyverse)
library(dplyr)
library(zoo)


setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/DAYMET-2016-2018")
nycha_daymet_indicators <- read.fst("daymet-nycha-indicators-POST.fst") %>%
  mutate(building_number = as.character(bldng_n),
         tds_number = tds_nmb) %>%
  select(-c(bldng_n, tds_nmb)) %>%
  mutate(building_number = if_else(building_number == "4e" | building_number == "4E", "4.3", building_number),
         building_number = if_else(building_number == "4w" | building_number == "4W", "4.7", building_number)) %>%
  mutate(building_number = as.numeric(building_number))

nycha_daymet_indicators %>%
    select(tds_number, building_number) %>%
    n_distinct()

# development and building characteristics dataset:
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
building_char <- read.fst("building_characteristics.fst") %>%
  mutate(tds_number = as.numeric(tds_number),
         building_number = as.character(building_number))%>%
  mutate(MMWRyear = year) %>%
  select(-c(n)) %>%
  mutate(building_number = if_else(building_number == "4e" | building_number == "4E", "4.3", building_number),
         building_number = if_else(building_number == "4w" | building_number == "4W", "4.7", building_number)) %>%
  mutate(building_number = as.numeric(building_number))


building_char  %>%
  select(tds_number, building_number) %>%
  n_distinct()

# mold work orders dataset:
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
work_orders <- read.fst("work-order-processed-aim-1_week.fst") %>%
  mutate(
    tds_number = as.numeric(tds_number),
    building_number = as.character(building_number)) %>%
  mutate(building_number = if_else(building_number == "4e" | building_number == "4E", "4.3", building_number),
         building_number = if_else(building_number == "4w" | building_number == "4W", "4.7", building_number)) %>%
  mutate(building_number = as.numeric(building_number))


work_orders  %>%
  select(tds_number, building_number) %>%
  n_distinct()

str(work_orders)
str(nycha_daymet_indicators)
# meteorological covariates
setwd("~/Desktop/projects/Mattlab/F31/data/nldas-data-processed")

nldas_post <- read.fst("nldas_post.fst") %>%
  mutate(
    tds_number = as.numeric(tds_nmb),
    building_number = as.character(bldng_n)) %>%
  filter(MMWRyear < 2024) %>%
  select(-c(tds_nmb, bldng_n)) %>%
  mutate(building_number = if_else(building_number == "4e" | building_number == "4E", "4.3", building_number),
         building_number = if_else(building_number == "4w" | building_number == "4W", "4.7", building_number)) %>%
  mutate(building_number = as.numeric(building_number))

nldas_post %>% select(tds_number, building_number) %>% n_distinct()

# holidays
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
h_week <- read.fst("holiday_weeks.fst")

# time since intervention 
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
intervention <- read.fst("time_since_intervention.fst") %>%
  mutate(tds_number = as.numeric(TDS)) %>%
  dplyr::select(tds_number, days_until_2021, intervention_date) %>%
  mutate( year_month_MB = as.yearmon(as.Date(intervention_date)),
          year_month_MB_label = format(year_month_MB, "%b %Y"), # Format as "Jul 2018", "Apr 2019"
          year_month_MB_label = if_else(is.na(year_month_MB_label), "Unknown/privately managed", year_month_MB_label),
          year_month_MB_factor = factor(year_month_MB_label, levels = unique(year_month_MB_label))) %>% # Convert to factor
  select(-c(year_month_MB, year_month_MB_label))

# first join mold work orders and meterological data - when there's na for
# the work orders, it's because there was no work order for that week
# and building. We will fill in the na with 0.

mold_met_data <- nycha_daymet_indicators %>%
  left_join(work_orders) %>%
  mutate(n = ifelse(is.na(n), 0, n),
         n_founded = ifelse(is.na(n_founded), 0, n_founded),
         n_repeat = ifelse(is.na(n_repeat_reports), 0, n_repeat_reports),
         n_severe = ifelse(is.na(n_severe), 0, n_severe),
         n_bathroom = ifelse(is.na(n_bathroom), 0, n_bathroom),
         n_non_bathroom = ifelse(is.na(n_non_bathroom), 0, n_non_bathroom),
         n_multiple_room = ifelse(is.na(n_multiple_room), 0, n_multiple_room),
                  n_founded_no_severe = ifelse(is.na(n_founded_no_severe), 0, n_founded_no_severe)) %>%
  left_join(building_char, by = c("tds_number", "building_number", "MMWRyear")) %>%
  left_join(nldas_post,
            by = c("tds_number","building_number",
              "year_week_date", "MMWRyear", "MMWRweek")) %>%
  left_join(h_week, by = "year_week_date") %>%
  left_join(intervention, by= c("tds_number"))

mold_met_data <- mold_met_data %>%
  filter((MMWRyear == 2021 |
            MMWRyear == 2022) | 
           (MMWRyear == 2020 & MMWRweek %in% 47:53)|
           MMWRyear == 2023) %>%
  drop_na(total_households)

names(mold_met_data)

mold_met_data %>%
  select(tds_number, building_number) %>%
  n_distinct()

sum(mold_met_data$n_founded)

# write out the dataset:
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
write.fst(mold_met_data, "mold-met-data-post-mb.fst")
