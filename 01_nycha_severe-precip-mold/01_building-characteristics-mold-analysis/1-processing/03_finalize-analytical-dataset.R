# Here we clean up for lcmm and future tables.

# load packages -----------------------------------------------------------
require(sf)
require(dplyr)
require(tigris)
require(fst)
require(tidyverse)
require(zoo)
require(classInt)
require(readxl)

# Read in data
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")
analytical_data <- read.fst("analytical_data_quarterly.fst") #%>%
 # filter(year %in% c(2021, 2022, 2023))   

analytical_data_id <- analytical_data %>%
  distinct(tds_number)

#sum(analytical_data$building_total)/12



# add EOP sites

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/data/Mold Research Columbia ")
eop <- read_excel("EOP site list as of 10-23-23.xlsx", skip = 2) %>%
  janitor::clean_names() %>%
  mutate(consolidated_tds_number = as.numeric(cons_tds_number)) %>%
  mutate(eop = round) %>%
  select(consolidated_tds_number, eop) %>%
  mutate(eop = 1) %>%
  unique()

analytical_data <- analytical_data %>%
  left_join(eop)



analytical_data <- analytical_data %>% # fix missing building median income
  group_by(year, archetype, total_population) %>%
  mutate(median_household_income_rep = mean(median_household_income, na.rm = T)) %>%
  ungroup() %>%
  mutate(median_household_income = if_else(is.na(median_household_income), median_household_income_rep, median_household_income))

# Clean and process data
analytical_dat <- analytical_data %>%
  mutate(archetype = if_else(str_detect(development, "FHA"), "FHA home", archetype)) %>%
  mutate(
    archetype = as.factor(case_when(
      archetype == "1 - High-rise in the park" ~ "high_rise_in_park",
      archetype == "2 - Mid-rise in the park" ~ "mid_rise_in_park",
      archetype == "3 - Low-rise in the park" ~ "low_rise_in_park",
      archetype %in% c("4 - Context Towers", "5 - Context Mid-rises") ~ "context",
      archetype == "6 - Walkups & Brownstones" ~ "walkup_and_brownstones",
      archetype == "FHA home" ~ "FHA_home")),
    year = factor(year, levels = c(2019, 2020, 2021, 2022, 2023)),
    archetype = relevel(archetype, ref = "walkup_and_brownstones"),
    building_year_group = factor(building_year_group, levels = c(
      "1936-1956", "1956-1967", "1967-1978", "1978-1994")),
    senior_development_flag = as.factor(case_when(
      senior_development_flag == "EXCLUSIVELY" ~ "exclusively",
      senior_development_flag == "PARTIALLY (1 BUILDING)" ~ "partially",
      senior_development_flag == "PARTIALLY (2 BUILDINGS)" ~ "partially",
      senior_development_flag == "-" ~ "no",
      senior_development_flag == "PARTIALLY (1 STAIRHALL)" ~ "no",
      senior_development_flag == "NA" ~ "no",
      is.na(senior_development_flag) ~ "no")),
    building_age = age_of_the_building_as_of_10_1_2023,
    building_sq_footage = building_coverage_square_footage_calculated_in_gis / 1000,
    #residential_buildings = total_residential_buildings_as_of_10_1_2023,
  #  building_sq_footage_by_building = building_coverage_square_footage_calculated_in_gis / 
  #    (total_residential_buildings_as_of_10_1_2023 + total_non_residential_buildings_as_of_10_1_2023),
  #  building_floors = str_extract_all(number_of_stories, "\\d+") %>% 
  #    map_dbl(~ mean(as.numeric(.x))), 
    svi = svi * 100,
    sheetrock_status_flag = as.factor(if_else(sheetrock_status_flag == "Yes", 1, 0)),
    sandy_development_flag = as.factor(if_else(sandy_development_flag == "Yes", 1, 0)),
    mechanical_ventilation = as.factor(mechanical_ventilation),
    pct_female = (female_population / total_population) * 100,
    pct_female_scaled = scale(pct_female),
    pct_pop_under_4 = ((population_4) / total_population) * 100,
    pct_pop_under_18 = ((population_4 + population_5_17) / total_population) * 100, # wouldnt use this actually bc its missing 4 year olds
    pct_pop_under_18_scaled = scale(pct_pop_under_18),
    pct_pop_18_39 = (population_18_39 / total_population) * 100,
    pct_pop_18_39_scaled = scale(pct_pop_18_39),
    pct_pop_40_plus = (population_40_and_over / total_population) * 100,
    pct_pop_62_plus = (population_62_and_over / total_population) * 100,
    pct_pop_40_plus_scaled = scale(pct_pop_40_plus),
    pct_pop_nhwhite = (white_population / total_population) * 100,
    pct_pop_nhwhite_scaled = scale(pct_pop_nhwhite),
    pct_pop_nhblack = (black_population / total_population) * 100,
    pct_pop_nhblack_scaled = scale(pct_pop_nhblack),
    pct_pop_hispanic = (hispanic_population / total_population) * 100,
    pct_pop_hispanic_scaled = scale(pct_pop_hispanic),
    pct_pop_asian = (asian_population / total_population) * 100,
    pct_pop_asian_scaled = scale(pct_pop_asian),
    pct_pop_hispanic_or_black = ((hispanic_population + black_population) / total_population) * 100,
    pct_pop_hispanic_or_black_scaled = scale(pct_pop_hispanic_or_black),
    building_median_income_scaled = scale(median_household_income),
    building_median_income = median_household_income,
    svi_scaled = scale(svi),
    scattered_site = as.factor(scattered_site_flag),
    ct_median_income_scaled = scale(ct_median_income),
    ct_percent_nhb_scaled = scale(ct_percent_nhb),
    ct_percent_h_scaled = scale(ct_percent_h),
    ct_percent_nhw_scaled = scale(ct_percent_nhw),
    ct_percent_pov_scaled = scale(ct_percent_pov),
    ct_percent_h_or_nhb = ct_percent_h + ct_percent_nhb,
    ct_percent_h_or_nhb_scaled = scale(ct_percent_h_or_nhb),
    ndvi_scaled = scale(ndvi),
    heat_scaled = scale(f_deviation_smooth),
    heat_quartile = as.factor(heat_quartile),
    ndvi_quartile = as.factor(ndvi_quartile)) %>%
  unique() %>%
  group_by(tds_number) %>%
  mutate(ndvi = mean(ndvi, na.rm = T),
         f_deviation_smooth = mean (f_deviation_smooth)) %>%
  ungroup() %>%
  arrange(year, qtr, tds_number) %>%
  mutate(
    senior_development_flag = factor(senior_development_flag, levels = c("no", "partially", "exclusively")),
    year_month_MB_factor = factor(year_month_MB_factor, levels = c("Jul 2018", "Apr 2019", "Jul 2019", "Aug 2019",
                                                                   "Sep 2019", "Oct 2019", "Nov 2019")),
  ) 



# spatial data
setwd("~/Desktop/projects/Mattlab/F31/data/NYCHA_buildings_shapefile")
nycha <- st_read("nycha_buildings_with_bins2.shp") %>%
  st_transform(2263) %>%
  mutate(tds_number = as.numeric(tds_nmb)) %>%
  group_by(tds_number) %>%
  summarize(geometry = st_union(geometry), .groups = "drop") %>%
  mutate(geometry = st_centroid(geometry))%>%
  filter(tds_number %in% analytical_data_id$tds_number) 


# Merge analytical data with spatial data
spatial_analytical_dat <- full_join(nycha, analytical_dat) %>%
  arrange(tds_number) 

# Extract coordinates and add to the dataset
coordinates <- st_coordinates(spatial_analytical_dat)
# Add the coordinates to the data frame
analytical_dat$x <- coordinates[, 1]
analytical_dat$y <- coordinates[, 2]


# 
# 
# 
# ### add preMB reports
# 
# # read in the dta --------------------------------------------------------
# setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
# dta_pre <- read.fst("mold-through-2023.fst") %>%
#   janitor::clean_names() %>%
#   filter(failure_code == "Mold Condition") %>%
#   filter(category == "Pre-Mold Busters") %>%
#   filter(zz_is_apt == 1) %>%
#   mutate(
#     datetime_nyc = as.POSIXct(zzcreate_date, format = "%m/%d/%y %H:%M", tz = "America/New_York"),
#     report_date_nyc = as.Date(datetime_nyc, format = "%m/%d/%y"),
#     qtr = quarter(report_date_nyc ),
#     report_date_year_nyc = year(report_date_nyc),
#   ) %>%
#   arrange(report_date_nyc) %>%
#   filter( report_date_year_nyc < 2019) 
# 
# # overall reports ---------------------------------------------------------
# # identify  reports for each tds-quarter
# dta_reports_pre <- dta_pre %>%
#   group_by(tds_number, building_number, wo_location, report_date_nyc) %>%
#   slice(1) %>%  # Remove duplicates within the same wo_location-date
#   ungroup() %>%
#   group_by(tds_number, building_number, wo_location) %>%
#   arrange(report_date_nyc) %>%
#   mutate(
#     n_any = n() ) %>%
#   ungroup() %>%
#   group_by(tds_number, report_date_year_nyc, qtr) %>%
#   summarize(
#     pre_reports = sum(n_any, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   mutate(year = report_date_year_nyc) %>%
#   mutate(tds_number = as.numeric(tds_number),
#          year = as.numeric(year),
#          qtr = as.numeric(qtr)) %>%
#   group_by(tds_number) %>%
#   mutate(average_pre_rep = mean(pre_reports, na.rm = T)) %>%
#   ungroup() %>%
#   dplyr::select(tds_number, average_pre_rep ) %>% 
#   unique()
# 
# 
# analytical_dat <- analytical_data %>%
#   left_join(dta_reports_pre)



# Save processed datasets
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")
write.fst(analytical_dat, "analytical_data_clean.fst")
