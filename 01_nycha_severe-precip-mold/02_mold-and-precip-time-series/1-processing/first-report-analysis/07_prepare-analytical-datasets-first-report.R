# The objective of this script is to create the glm dataset and the
# spatiotemporal dataset needed to plug into the mold x extreme
# precipitation models.

# What needs to occur in this dataset includes:
# (a) fixing week 53 and updating variables accordingly
# (b) create offset as log of total households
# (c) prep date variable to be used in spline
# (d) create 0-8 lag variables for exposure

# space specific:
# (e) create spatiotemporal object
# (f) run spatial variogram to define distance threshold
# (g) finalize spatiotemporal objects
# (h) save glm and spatiotemporal objects


# load packages -----------------------------------------------------------

require(sf)
require(dplyr)
require(tidyverse)
require(sp)
require(spacetime)
require(gstat)
require(ggplot2)
require(spdep)
require(tis)
require(fst)

# set working directory and read in data ----------------------------------

# anlaytical data
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
analytical_dat <- read.fst("mold-met-data-post-mb-first-only.fst")


# spatial data
setwd("~/Desktop/projects/Mattlab/F31/data/NYCHA_buildings_shapefile")
nycha_building_shape <- st_read("nycha_buildings_with_bins2.shp")

nycha_building_shape_unique <- nycha_building_shape %>%
  group_by(  tds_nmb,  bldng_n) %>%
  dplyr::slice(1) %>%
  st_transform(crs = 32618) %>% ### UTM for NYC
  mutate(geometry = st_centroid(geometry),
         tds_number = as.numeric(tds_nmb),
         building_number = as.character(bldng_n)) %>%
  mutate(building_number = if_else(building_number == "4e" | building_number == "4E", "4.3", building_number),
         building_number = if_else(building_number == "4w" | building_number == "4W", "4.7", building_number)) %>%
  mutate(building_number = as.numeric(building_number))

setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/time_series")
saveRDS(nycha_building_shape_unique, file = "nycha_building_shape_unique.rds")


# make glm dataset --------------------------------------------------------

glm_analytical_dat <- analytical_dat %>%
  rename(borough = geographical_borough) %>%
  dplyr::select(
    consolidated_tds_number,
    year_week_date, 
    MMWRyear, 
    MMWRweek,
    tds_number,
    building_number,
    n,
    n_founded,
    n_severe,
    n_founded_no_severe,
    week_95th_ind_precip,
    total_households,
    temp_weekly_mean,
    windspeed_weekly_mean,
    relative_humidity_weekly_mean,
    absolute_humidity_weekly_mean,
    is_holiday,
    sum_week_precip,
    intervention_date,
    year_month_MB_factor,
    borough)  %>%
  group_by(year_week_date, tds_number, building_number, MMWRyear, MMWRweek) %>%
  mutate(
    temp_weekly_mean = mean(temp_weekly_mean, na.rm = TRUE),
    windspeed_weekly_mean = mean(windspeed_weekly_mean, na.rm = TRUE),
    relative_humidity_weekly_mean = mean(relative_humidity_weekly_mean, na.rm = TRUE),
    n = sum(n, na.rm = TRUE),
    n_founded = sum(n_founded, na.rm = TRUE),
    n_severe = sum(n_severe, na.rm = TRUE),
    n_founded_no_severe = sum(n_founded_no_severe, na.rm = TRUE),
    n_founded_no_severe = if_else(n_founded_no_severe < 0, 0 ,n_founded_no_severe),
    total_households = total_households,
    week_95th_ind_precip = sum(week_95th_ind_precip, na.rm = TRUE),
    week_95th_ind_precip = if_else(week_95th_ind_precip > 1, 1, week_95th_ind_precip)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(weeks_since_intervention = as.numeric(difftime(year_week_date, intervention_date, units = "weeks"))) %>%
  mutate(month = month(year_week_date),
         season = case_when(
           month %in% c(12, 1, 2) ~ "Winter",
           month %in% 3:5 ~ "Spring",
           month %in% 6:8 ~ "Summer",
           month %in% 9:11 ~ "Fall")) %>%
  group_by(tds_number, building_number) %>%
  arrange(MMWRyear, 
          MMWRweek) %>%
  mutate(timepoint = 1,
         timepoint = cumsum(timepoint),
         period = 52) %>%
  mutate(
    sw_ind_0 = week_95th_ind_precip,
    sw_ind_1 = lag(week_95th_ind_precip, 1),
    sw_ind_2 = lag(week_95th_ind_precip, 2),
    sw_ind_3 = lag(week_95th_ind_precip, 3),
    sw_ind_4 = lag(week_95th_ind_precip, 4),
    sw_ind_5 = lag(week_95th_ind_precip, 5),
    sw_ind_6 = lag(week_95th_ind_precip, 6),
    sw_ind_7 = lag(week_95th_ind_precip, 7),
    sw_ind_8 = lag(week_95th_ind_precip, 8),
    sum_week_precip_0 = sum_week_precip,
    sum_week_precip_1 = lag(sum_week_precip, 1),
    sum_week_precip_2 = lag(sum_week_precip, 2),
    sum_week_precip_3 = lag(sum_week_precip, 3),
    sum_week_precip_4 = lag(sum_week_precip, 4),
    sum_week_precip_5 = lag(sum_week_precip, 5),
    sum_week_precip_6 = lag(sum_week_precip, 6)) %>%
  ungroup() %>%
  group_by(tds_number, building_number, MMWRyear, 
           MMWRweek) %>%
  mutate(multiple_6 = if_else(sw_ind_0 +
                                sw_ind_1 +
                                sw_ind_2 +
                                sw_ind_3 +
                                sw_ind_4 +
                                sw_ind_5 +
                                sw_ind_6 > 1, 1, 0))%>%
  mutate(multiple_8 = if_else(sw_ind_0 +
                                sw_ind_1 +
                                sw_ind_2 +
                                sw_ind_3 +
                                sw_ind_4 +
                                sw_ind_5 +
                                sw_ind_6 +
                                sw_ind_7+ 
                                sw_ind_8 > 1, 1,0 )) %>%
  ungroup()%>%
  mutate(
    cos_annual =cos(2 * 180 * timepoint / period),
    sin_annual =sin(2 * 180 * timepoint / period),
    timepoint_sq = timepoint ^ 2,
    timepoint_ctr_sd = scale(timepoint),
    timepoint_sq_ctr_sd = scale(timepoint_sq),
    founded_mold = n_founded,
    mold_report = n,
    severe_mold = n_severe,
    household_offset = log(total_households),
    year = as.character(MMWRyear),
    week = MMWRweek,
    consolidated_tds_number = as.factor(consolidated_tds_number),
    development_factor = as.factor(tds_number),
    id = paste0(tds_number,"_",building_number))  %>%
  arrange(year_week_date,
          tds_number,
          building_number) %>%
  ungroup() %>%
  drop_na(temp_weekly_mean)%>%
  drop_na(n_founded) %>%
  drop_na(household_offset) %>%
  drop_na(windspeed_weekly_mean) %>%
  drop_na(relative_humidity_weekly_mean)%>%
  drop_na(sw_ind_6)%>%
  unique() %>%
  filter(year_month_MB_factor != "Unknown/privately managed") %>%
  mutate(any_founded = if_else(founded_mold >=1,1,0),
         any_report = if_else(mold_report >=1,1,0),
         any_severe = if_else(severe_mold >=1,1,0),
         any_non_severe = if_else(n_founded_no_severe>=1,1,0),
         multiple_founded = if_else(founded_mold >1,1,0) ,
         year_month_MB_group = if_else(year_month_MB_factor == "Jul 2018" | year_month_MB_factor == "Apr 2019", "early", "late"))



# make spatiotemporal object ----------------------------------------------

spatial_analytical_dat <-
  left_join(nycha_building_shape_unique, glm_analytical_dat) %>%
  arrange(year_week_date, tds_number, building_number) 

coordinates <- st_coordinates(spatial_analytical_dat)

# Add the coordinates to the data frame
spatial_analytical_dat$x <- coordinates[, 1]
spatial_analytical_dat$y <- coordinates[, 2]


spatial_analytical_dat_frame <- spatial_analytical_dat %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  arrange(year_week_date, tds_number, building_number) %>%
  drop_na(year_month_MB_factor)


# write out analytical objects --------------------------------------------

setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/time_series")
saveRDS(glm_analytical_dat, file = "glm_analytical_dat-first-only.rds")
saveRDS(spatial_analytical_dat_frame, file = "spatial_analytical_dat_frame-first-only.rds")

sum(analytical_dat$n_founded)

