# The purpose here is to identify quarterly, development level repeat 
# founded mold work orders for use in the lcmm analysis. 

# load packages -----------------------------------------------------------
require(tidyverse)
require(lubridate)
require(dplyr)
require(fst)



# read in the data --------------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
dta <- read.fst("mold-through-2023.fst") %>%
  janitor::clean_names() %>%
  filter(failure_code == "Mold Condition") %>%
  filter(zz_is_apt == 1) %>%
  mutate(
    datetime_nyc = as.POSIXct(zzcreate_date, format = "%m/%d/%y %H:%M", tz = "America/New_York"),
    report_date_nyc = as.Date(datetime_nyc, format = "%m/%d/%y"),
    qtr = quarter(report_date_nyc ),
    report_date_year_nyc = year(report_date_nyc)
  ) %>%
  arrange(report_date_nyc) %>%
 # filter(report_date_year_nyc > 2020, report_date_year_nyc < 2024) %>%
  filter(category == "Mold Busters")




sum(dta$founded_flag) # 30682, now 35260 with updated data

#names(dta)

dta %>% select(tds_number, building_number) %>% n_distinct() ### 1952, now 1961 with updated data


### FOCUS ON founded reports here - dont end up using the others for this analysis
# overall reports ---------------------------------------------------------
# identify repeat reports for each tds-quarter
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
    )
  ) %>%
  ungroup() %>%
  group_by(tds_number, report_date_year_nyc, qtr) %>%
  summarize(
    total_reports = n(),
    total_repeats = sum(repeat_flag, na.rm = TRUE),
    unique_units_with_reports = n_distinct(wo_location),
    units_with_repeats = n_distinct(wo_location[repeat_flag == 1])
  ) %>%
  ungroup()

# founded reports ---------------------------------------------------------
# filter for founded reports and repeat the process
data_founded_reports <- dta %>%
  filter(founded_flag == 1) %>%
  group_by(tds_number, building_number, wo_location, report_date_nyc) %>%
  slice(1) %>%
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
      lag(report_date_nyc, 1) %within% interval(report_date_nyc - dmonths(6), report_date_nyc - 1), ### checks if the lag report is within 6 months of this report
      1,
      0
    )
  ) %>%
  ungroup() %>%
  group_by(tds_number, report_date_year_nyc, qtr) %>%
  summarize(
    total_founded_reports = n(),
    total_founded_repeats = sum(repeat_flag, na.rm = TRUE),
    total_founded_repeats_6 = sum(repeat_flag_6, na.rm = TRUE),
    unique_units_with_founded_reports = n_distinct(wo_location),
    units_with_founded_repeats = n_distinct(wo_location[repeat_flag == 1])
  ) %>%
  ungroup()

# severe reports ---------------------------------------------------------
# filter for severe founded reports and repeat the process
data_severe_reports <- dta %>%
  filter(founded_flag == 1, zz_sq_feet >= 20) %>%
  group_by(tds_number, building_number, wo_location, report_date_nyc) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(tds_number, building_number, wo_location) %>%
  arrange(report_date_nyc) %>%
  mutate(
    repeat_flag = if_else(
      lag(report_date_nyc, 1) %within% interval(report_date_nyc - dyears(1), report_date_nyc - 1),
      1,
      0
    )
  ) %>%
  ungroup() %>%
  group_by(tds_number, report_date_year_nyc, qtr) %>%
  summarize(
    total_severe_reports = n(),
    total_severe_repeats = sum(repeat_flag, na.rm = TRUE),
    unique_units_with_severe_reports = n_distinct(wo_location),
    units_with_severe_repeats = n_distinct(wo_location[repeat_flag == 1])
  ) %>%
  ungroup()

# join all data in final outcome dataset ----------------------------------


outcome_data <- data_reports %>%
  full_join(data_founded_reports, by = c("tds_number", "report_date_year_nyc", "qtr")) %>%
  full_join(data_severe_reports, by = c("tds_number", "report_date_year_nyc", "qtr")) 



# write out the dataset ---------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")
write.fst(outcome_data, "mold_data_quarterly.fst")


