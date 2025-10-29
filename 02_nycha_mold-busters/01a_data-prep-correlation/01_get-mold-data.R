

# load packages -----------------------------------------------------------
require(tidyverse)
require(lubridate)
require(dplyr)
require(fst)


# read in the precipitation data ------------------------------------------

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/daymet")

# Read and prepare the data
df <- read.fst("daymet-nycha-indicators-PRE.fst") %>%
  rename(tds_number = tds_nmb,
         building_number = bldng_n) %>%
  select(tds_number, building_number, year_week_date, week_95th_ind_precip) %>%
  mutate(tds_number = as.numeric(tds_number),
         building_number = as.numeric(building_number))


# Filter and expand building_number 4 to 4, 4.3, and 4.7
nycha_daymet_2016_2018_indicators <- df %>%
  filter(tds_number == 25, building_number == 4) %>%
  bind_rows(
    mutate(., building_number = 4.3),
    mutate(., building_number = 4.7)
  ) %>%
  bind_rows(df %>% filter(! (tds_number == 25 & building_number == 4))) %>%
  arrange(building_number)
nycha_daymet_2016_2018_indicators <- 
nycha_daymet_2016_2018_indicators %>%
  filter(week_95th_ind_precip == 1) %>%
  na.omit() %>%
  rename(report_date_nyc = year_week_date) %>%
  mutate(tds_number = as.numeric(tds_number),
         building_number = as.numeric(building_number))


# read in the data --------------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
dta <- read.fst("mold-through-2023.fst") %>%
  janitor::clean_names() %>%
  filter(failure_code == "Mold Condition") %>%
  filter(zz_is_apt == 1) %>%
  filter(category == "Pre-Mold Busters") %>%
  mutate(
    datetime_nyc = as.POSIXct(zzcreate_date, format = "%m/%d/%y %H:%M", tz = "America/New_York"),
    report_date_nyc = as.Date(datetime_nyc, format = "%m/%d/%y"),
    report_date_year_nyc = year(report_date_nyc)
  ) %>%
  arrange(report_date_nyc) %>%
  filter(report_date_year_nyc %in% c(2016:2018)) %>%
  mutate(
    building_number = if_else(building_number == "4e", "4.3", building_number),
    building_number = if_else(building_number == "4w", "4.7", building_number)) %>%
  mutate(tds_number = as.numeric(tds_number),
         building_number = as.numeric(building_number)) 

dta %>% select(tds_number) %>% n_distinct() ### 2025

# overall reports ---------------------------------------------------------
# identify repeat reports for each building-year
data_reports <- dta %>%
  group_by(tds_number, building_number, wo_location, report_date_nyc) %>%
  slice(1) %>%  # Remove duplicates within the same wo_location-date
  ungroup() %>%
  group_by(tds_number, building_number, wo_location) %>%
  arrange(report_date_nyc) %>%
  mutate(
    repeat_flag = if_else(
      lag(report_date_nyc, 1) %within% interval(report_date_nyc - dyears(1), report_date_nyc - 1),
      1,
      0
    ),
    repeat_flag_6 = if_else(
      lag(report_date_nyc, 1) %within% interval(report_date_nyc - dmonths(6), report_date_nyc - 1),
      1,
      0)
  ) %>%
  ungroup() 

data_reports_sw <- data_reports %>%
  full_join(nycha_daymet_2016_2018_indicators) %>%
  group_by(tds_number, building_number) %>%
  arrange(report_date_nyc, .by_group = TRUE) %>%
  mutate(last_sw_date = if_else(week_95th_ind_precip == 1, report_date_nyc, as.Date(NA)),
         last_sw_date = zoo::na.locf(last_sw_date, na.rm = FALSE)) %>%
  fill(last_sw_date, .direction = "down") %>%
  mutate(days_since_last_sw = as.numeric(report_date_nyc - last_sw_date)) %>%
  ungroup() %>%
  mutate(recent_sw = if_else(days_since_last_sw <= 42,1,0)) %>%
  drop_na(report_date_year_nyc)


data_reports_sum <- data_reports_sw %>%
  group_by(tds_number, building_number, report_date_year_nyc) %>%
  summarize(
    total_reports = n(),
    total_severe_weather = sum(recent_sw, na.rm = TRUE),
    total_repeats = sum(repeat_flag, na.rm = TRUE),
    total_repeats_6 = sum(repeat_flag_6, na.rm = TRUE),
    unique_units_with_reports = n_distinct(wo_location),
    units_with_repeats = n_distinct(wo_location[repeat_flag == 1])
  ) %>%
  ungroup() %>%
  unique()


setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data-processed-correlations")
write.fst(data_reports_sum, "mold-yearly.fst")

n_distinct(data_reports_sum$tds_number)
sum(data_reports_sum$total_reports)
