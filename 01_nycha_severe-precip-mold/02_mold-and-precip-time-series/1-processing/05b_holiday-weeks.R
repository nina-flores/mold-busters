library(dplyr)
library(lubridate)
library(MMWRweek)
library(tis) # for generating holiday dates

# Step 1: Generate a sequence of dates for the desired years
# Ensure start_year and end_year are properly defined
start_year <- 2020
end_year <- 2023

# Generate a sequence of all dates
all_dates <- seq(from = as.Date(paste0(start_year, "-01-01")),
                 to = as.Date(paste0(end_year, "-12-31")),
                 by = "day")

# Step 2: Identify holiday dates for the specified years
holiday_dates <- tis::holidays(start_year:end_year)
holiday_dates_as_date <- as.Date(as.character(holiday_dates), format = "%Y%m%d")

# Step 3: Create a dataset with all dates and mark holidays
dates_with_holidays <- tibble(
  date = all_dates,
  is_holiday = date %in% holiday_dates_as_date 
)

# Step 5: Aggregate to find epiweeks with holidays
holiday_weeks <- dates_with_holidays %>%
  mutate(
    MMWRweek(date),
    year_week_date = MMWRweek2Date(MMWRyear, MMWRweek, 1) # Get the first day of each week
  ) %>%
  group_by(year_week_date) %>%
  summarize(is_holiday = sum(is_holiday))

setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
write.fst(holiday_weeks, "holiday_weeks.fst")