# The purpose here is to join all the mold outcome and building information
# for the lcmm analysis.

require(sf)
require(dplyr)
require(tigris)
require(fst)
require(tidyverse)
require(zoo)

# outcome data ------------------------------------------------------------

setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")
outcome_data <- read.fst("mold_data_quarterly.fst") %>%
  mutate(
    tds_number = as.numeric(tds_number),
    year = report_date_year_nyc
  ) 

# development and building characteristics -------------------------------------
# aggregate to development level

setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
char <- read.fst("development_characteristics.fst") %>%
  mutate(tds_number = as.numeric(tds_number)) 

# Create expanded building-year combinations
expanded_combinations <- expand_grid(
  tds_number = unique(char$tds_number),
  year = c(2019, 2020, 2021, 2022, 2023),
  qtr = c(1:4)) 

# this will give us a more accurate count of other residential buildings
# as this has already dropped buildings that drop out of the dataset 

setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
char_build <- read.fst("building_characteristics.fst") %>%
  filter(year == 2021) %>%
  mutate(tds_number = as.numeric(tds_number)) %>% 
  group_by(tds_number) %>%
  mutate(building_total = n()) %>%
  select(tds_number, building_total) %>%
  unique()

# Expand outcome_data to ensure all tds-year combinations ------------


outcome_data <- expanded_combinations %>%
  left_join(outcome_data, by = c("tds_number", "year", "qtr")) %>%
  replace_na(list(
    total_reports = 0,
    total_repeats = 0,
    unique_units_with_reports = 0,
    units_with_repeats = 0,
    total_founded_reports = 0,
    total_founded_repeats = 0,
    total_founded_repeats_6 = 0,
    unique_units_with_founded_reports = 0,
    units_with_founded_repeats = 0,
    total_severe_reports = 0,
    total_severe_repeats = 0,
    unique_units_with_severe_reports = 0,
    units_with_severe_repeats = 0
  )) %>%
  left_join(char_build)

# time since intervention -------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
intervention <- read.fst("time_since_intervention.fst") %>%
  mutate(tds_number = as.numeric(TDS)) %>%
  dplyr::select(tds_number, days_until_2021, intervention_date) %>%
  mutate( year_month_MB = as.yearmon(as.Date(intervention_date)),
          year_month_MB_label = format(year_month_MB, "%b %Y"), # Format as "Jul2018", "Apr2019"
          year_month_MB_label = if_else(is.na(year_month_MB_label), "Unknown/privately managed", year_month_MB_label),
          year_month_MB_factor = factor(year_month_MB_label, levels = unique(year_month_MB_label))) # Convert to ordered factor


# census tract level data -------------------------------------------------
state_code <- "NY"
county_codes <- c("005", "047", "061", "081", "085")

nyc_ct <- tracts(state = state_code, county = county_codes) %>%
  st_transform(2263) %>% 
  dplyr::select(GEOID)

setwd("~/Desktop/projects/Mattlab/F31/data/NYCHA_buildings_shapefile")
nycha <- st_read("nycha_buildings_with_bins2.shp") %>%
  st_transform(2263) %>%
  mutate(tds_number = as.numeric(tds_nmb)) %>%
  group_by(tds_number) %>%
  summarize(geometry = st_union(geometry), .groups = "drop") %>%
  mutate(geometry = st_centroid(geometry))

nycha_ct <- st_join(nyc_ct, nycha) %>%
  na.omit() %>% 
  as.data.frame() %>%
  dplyr::select(GEOID, tds_number) %>%
  unique()

# census, flooding, heat, svi, and ndvi data ------------------------------
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")

census_data <- read.fst("census_data_ct.fst") %>%
  dplyr::select(GEOID, ct_median_income, ct_percent_nhb, ct_percent_h, ct_percent_nhw, ct_percent_pov, ct_pct_pov_quart) 
nycha_ct <- nycha_ct %>% left_join(census_data)

flooding_data <- read.fst("flooding_nycha.fst") %>%
  dplyr::select(
    tds_nmb, bldng_n, intersects_coastal_flood, 
    intersects_nuisance_flood, intersects_deep_flood
  ) %>%
  mutate(
    tds_number = as.numeric(tds_nmb),
    building_number = as.character(bldng_n)
  ) %>%
  group_by(tds_number) %>%
  summarize(intersects_coastal_flood_max = max(intersects_coastal_flood),
            intersects_nuisance_flood_max = max(intersects_nuisance_flood),
            intersects_deep_flood_max = max(intersects_deep_flood),
            intersects_coastal_flood_n = sum(intersects_coastal_flood),
            intersects_nuisance_flood_n = sum(intersects_nuisance_flood),
            intersects_deep_flood_n = sum(intersects_deep_flood)) %>%
  ungroup() %>%
  distinct()

heat_data <- read.fst("heat_ct.fst")
nycha_ct <- nycha_ct %>% left_join(heat_data)

svi_data <- read.fst("svi_dat_ct.fst") %>%
  rename(GEOID = FIPS) %>%
  mutate(GEOID = as.character(GEOID))

nycha_ct <- nycha_ct %>% left_join(svi_data)

ndvi_data <- read.fst("ndvi_max.fst") %>%
  mutate(GEOID = as.character(geoid))

# Merge data to create prelim_data ----------------------------------------
prelim_data <- outcome_data %>%
  left_join(char, by = c("tds_number", "year")) %>%
  left_join(intervention, by = "tds_number") %>%
  left_join(flooding_data, by = c("tds_number")) %>%
  left_join(nycha_ct, by = c("tds_number")) %>%
  left_join(ndvi_data, by = c("year", "GEOID"))


# Deal with rad pact to get final analytical dataset -------------------------
#also want to allow there to be NA's where there should truly be
# NA's. This will be once a building is converted to PACT.

analytical_data <- prelim_data %>%
  filter(year_month_MB_factor != "Unknown/privately managed")

# Write out dataset -------------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")
write.fst(analytical_data, "analytical_data_quarterly.fst")

