require(sf)
require(dplyr)
require(tigris)
require(fst)
require(tidyverse)
require(zoo)
options(scipen = 999) 
# outcome data ------------------------------------------------------------

# development and building characteristics -------------------------------------

setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
char <- read.fst("building_characteristics.fst") %>%
  mutate(tds_number = as.numeric(tds_number)) %>%
  mutate(
    building_number = if_else(building_number == "4E", "4.3", building_number),
    building_number = if_else(building_number == "4W", "4.7", building_number)
  ) %>%
  mutate(
    building_number = as.numeric(building_number),
    id = paste0(tds_number, "_", building_number)  # Create unique ID for each building
  )


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
  mutate(
    geometry = st_centroid(geometry),
    tds_number = as.numeric(tds_nmb),
    building_number = as.character(bldng_n)
  ) %>%
  mutate(
    building_number = if_else(building_number == "4e", "4.3", building_number),
    building_number = if_else(building_number == "4E", "4.3", building_number),
    building_number = if_else(building_number == "4w", "4.7", building_number),
    building_number = if_else(building_number == "4W", "4.7", building_number)
  )%>%
  mutate(building_number = as.numeric(building_number))

nycha_ct <- st_join(nyc_ct, nycha) %>%
  na.omit() %>% 
  as.data.frame() %>%
  dplyr::select(GEOID, tds_number, building_number) %>%
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
  group_by(tds_number, building_number) %>%
  summarize(intersects_coastal_flood = max(intersects_coastal_flood),
            intersects_nuisance_flood = max(intersects_nuisance_flood),
            intersects_deep_flood = max(intersects_deep_flood)) %>%
  ungroup() %>%
  mutate(
    building_number = if_else(building_number == "4e", "4.3", building_number),
    building_number = if_else(building_number == "4E", "4.3", building_number),
    building_number = if_else(building_number == "4w", "4.7", building_number),
    building_number = if_else(building_number == "4W", "4.7", building_number)
  ) %>%
  mutate(building_number = as.numeric(building_number)) %>%
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
prelim_data <- char %>%
  left_join(intervention, by = "tds_number") %>%
  left_join(flooding_data, by = c("tds_number", "building_number")) %>%
  left_join(nycha_ct, by = c("tds_number", "building_number")) %>%
  left_join(ndvi_data, by = c("year", "GEOID"))


# Deal with rad pact to get final analytical dataset -------------------------
#also want to allow there to be NA's where there should truly be
# NA's. This will be once a building is converted to PACT.

analytical_data <- prelim_data %>%
  filter(year_month_MB_factor != "Unknown/privately managed") %>%
  filter(year != 2020)

# Write out dataset -------------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")
write.fst(analytical_data, "analytical_data_table_1.fst")

