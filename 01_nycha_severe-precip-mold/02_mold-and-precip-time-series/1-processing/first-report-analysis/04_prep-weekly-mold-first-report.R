# the objective of this script is to aggregate mold reports to weekly sums
# for the time series analysis

# load required packages
require(tidyverse)
require(lubridate)
require(dplyr)
require(fst)
require(tis)
require(MMWRweek)


# read in and clean the data ----------------------------------------------

setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
dta <- read.fst("mold-through-2023.fst") %>%
  janitor::clean_names() %>%
  filter(failure_code == "Mold Condition") %>%
  filter(zz_is_apt == 1) %>%
  filter(category == "Mold Busters" | category == "Pre-Mold Busters") %>%
  mutate(
    severe_flag = if_else(zz_sq_feet >= 20, 1, 0),
    datetime_nyc = as.POSIXct(zzcreate_date, format = "%m/%d/%y %H:%M", tz = "America/New_York"),
    MMWRweek(datetime_nyc), 
    year_week_date = MMWRweek2Date(MMWRyear, MMWRweek, 1)  ) %>%
  arrange(datetime_nyc) %>%
  group_by(wo_location) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  filter(datetime_nyc > "2020-01-01") %>%
  group_by(year_week_date, tds_number, building_number, address) %>%
  mutate(
    n = n(),
    n_founded = sum(founded_flag, na.rm = T),
    n_severe = sum(severe_flag, na.rm = T),
    n_founded_no_severe = n_founded - n_severe
  ) %>%
  dplyr::select(year_week_date,
         tds_number,
         building_number,
         address,
         n,
         n_founded,
         n_severe,
         n_founded_no_severe) %>%
  slice(1) %>%
  ungroup() 



# save the data -----------------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
write_fst(dta, "work-order-processed-aim-1_week-first-only.fst")
