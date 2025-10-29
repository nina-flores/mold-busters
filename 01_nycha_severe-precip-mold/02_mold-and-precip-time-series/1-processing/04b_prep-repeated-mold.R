# load required packages
require(tidyverse)
require(lubridate)
require(dplyr)
require(fst)
require(tis)
require(MMWRweek)

# Get a vector of holiday dates

# Set working directory and read data
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
dta <- read.fst("mold-through-2023.fst") %>%
  janitor::clean_names() %>%
  filter(failure_code == "Mold Condition") %>%
  filter(zz_is_apt == 1) %>%
  mutate(
    severe_flag = if_else(zz_sq_feet >= 20, 1, 0),
    datetime_nyc = as.POSIXct(zzcreate_date, format = "%m/%d/%y %H:%M", tz = "America/New_York"),
    MMWRweek(datetime_nyc), 
    year_week_date = MMWRweek2Date(MMWRyear, MMWRweek, 1)
  ) %>%
  arrange(datetime_nyc) %>%
  filter(datetime_nyc > "2020-01-01") %>%
  filter(category == "Mold Busters") %>%
  group_by(year_week_date, tds_number, building_number, wo_location, address) %>%
  mutate(n = n(),
         n_max = max(n)) %>%
  group_by(year_week_date, tds_number, building_number, wo_location, address, n_max) %>%
  slice(1) %>%  # Ensure unique reports within the same `wo_location`-week
  ungroup()

# 74954 - 65658


# Step 1: Aggregate to `wo_location`-week level
wo_location_week <- dta %>%
  group_by(wo_location, year_week_date) %>%
  summarize(
    any_founded = if_else(sum(founded_flag, na.rm = TRUE) > 0, 1, 0),
    .groups = "drop"
  ) %>%
  filter(any_founded ==1)

# Step 2: Create a repeat flag for founded reports across weeks
wo_location_week <- wo_location_week %>%
  arrange(wo_location, year_week_date) %>%
  group_by(wo_location) %>%
  mutate(
    repeat_flag = if_else(
      any_founded == 1 & lag(any_founded == 1) & 
        year_week_date - lag(year_week_date) <= dmonths(12),1, 0)) %>%
  ungroup() %>% 
  dplyr::select(wo_location, year_week_date, repeat_flag)

# Step 3: Merge repeat flags back to the original data
dta <- dta %>%
  left_join(
    wo_location_week,
    by = c("wo_location", "year_week_date")
  )

# Step 4: Aggregate to building level with weekly summaries
dta <- dta %>%
  group_by(year_week_date, tds_number, building_number, address) %>%
  summarize(
    n = n(),
    n_founded = sum(founded_flag, na.rm = TRUE),
    n_repeat_reports = sum(repeat_flag, na.rm = TRUE),
    n_severe = sum(severe_flag, na.rm = TRUE),
    n_founded_no_severe = n_founded - n_severe,
    n_multiple_room = sum(founded_flag == 1 & n_max > 1, na.rm = TRUE),
    n_bathroom = sum(founded_flag == 1 & wo_location_type == "BATHROOM" & n_max == 1, na.rm = TRUE),
    n_non_bathroom = sum(founded_flag == 1 & wo_location_type != "BATHROOM" & n_max == 1, na.rm = TRUE),
    .groups = "drop"
  )


# Step 5: Select the relevant columns for final output
final_data <- dta %>%
  dplyr::select(
    year_week_date,
    tds_number,
    building_number,
    address,
    n,
    n_founded,
    n_repeat_reports,
    n_severe,
    n_founded_no_severe,
    n_multiple_room,
    n_bathroom,
    n_non_bathroom)

sum(final_data$n_founded)
sum(final_data$n_repeat_reports)
sum(final_data$n_bathroom)
sum(final_data$n_non_bathroom)
sum(final_data$n_multiple_room)


# Save the data -----------------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
write_fst(final_data, "work-order-processed-aim-1_week.fst")
