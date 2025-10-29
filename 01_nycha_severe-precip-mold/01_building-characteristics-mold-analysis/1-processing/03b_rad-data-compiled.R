require(sf)
require(dplyr)
require(tigris)
require(fst)
require(tidyverse)
require(zoo)
require(classInt)
require(readxl)



# get data for the rad pact coverted buildings

# want to read in and add the characteristics of the rad pact buildings so they
# can be seen in the data

rad_tds <- c( 3,
              147,
              125,
              160,
              344,
              224,
              342,
              190,
              334,
              305,
              356,
              353,
              57,
              312,
              351,
              366,
              368,
              313,
              365,
              339,
              345,
              194,
              95,
              46,
              2)



# Set working directory
setwd("~/Desktop/projects/Mattlab/F31/data/Tenant data updated files")
dev_char_2021 <- read_excel("NYCHA data for Mold Research Columbia University Jan 2021 additional data.xlsx", sheet = 1) %>%
  janitor::clean_names() %>%
  mutate(tds_number = as.numeric(tds_number),
         year = 2021) %>%
  filter(tds_number %in% rad_tds)

dev_build_2021 <- read_excel("NYCHA data for Mold Research Columbia University Jan 2021 additional data.xlsx", sheet = 2) %>%
  janitor::clean_names() %>%
  mutate(tds_number = as.numeric(tds_number)) %>%
  filter(tds_number %in% rad_tds) %>%
  group_by(tds_number) %>%
  summarize(building_total = n())

dev_char_2021 <- dev_char_2021 %>% full_join(dev_build_2021) %>%
  mutate()

names(dev_char_2021)


# add development characteristics -----------------------------------------

setwd("~/Desktop/projects/Mattlab/F31/data/NYCHA Data_1_8-29-2024")
# add development level characteristics
dev_char <- read_excel("Schedule -Ventilation-Dev Characteristics  10.24.2023 Rev 08.01.2024.xlsx", 
                       sheet = 4,
                       skip = 2) %>%
  janitor::clean_names()   %>%
  mutate(building_year = year(construction_date)) %>%
  janitor::clean_names()   %>%
  mutate(building_year = year(construction_date),
         building_sq_footage_by_building = building_coverage_square_footage_calculated_in_gis/(total_residential_buildings_as_of_10_1_2023 + total_non_residential_buildings_as_of_10_1_2023),
         building_floors = str_extract_all(number_of_stories, "\\d+") %>% 
           map_dbl(~ mean(as.numeric(.x))))


setwd("~/Desktop/projects/Mattlab/F31/data/NYCHA Data_1_8-29-2024")

# Read in sheet 3 of the development characteristics file
dev_char_s3 <- read_excel("Schedule -Ventilation-Dev Characteristics  10.24.2023 Rev 08.01.2024.xlsx", 
                          sheet = 3, skip = 1) %>%
  janitor::clean_names() %>%
  mutate(tds_number  = as.numeric(tds)) %>%
  mutate(mechanical_ventilation = 1)

dta <- dev_char_2021 %>% 
  left_join(dev_char) %>%
  left_join(dev_char_s3, by = "tds_number")


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
nycha <- st_read("nycha_buildings_with_bins.shp") %>%
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
prelim_data <- dta %>%
  left_join(intervention, by = "tds_number") %>%
  left_join(flooding_data, by = c("tds_number")) %>%
  left_join(nycha_ct, by = c("tds_number")) %>%
  left_join(ndvi_data, by = c("year", "GEOID"))


# Deal with rad pact to get final analytical dataset -------------------------
#also want to allow there to be NA's where there should truly be
# NA's. This will be once a building is converted to PACT.

analytical_data <- prelim_data %>%
  filter(year_month_MB_factor != "Unknown/privately managed")




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
    year = factor(year, levels = c(2021, 2022, 2023)),
    archetype = relevel(archetype, ref = "walkup_and_brownstones"),
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
    building_sq_footage_by_building = building_coverage_square_footage_calculated_in_gis / 
      (total_residential_buildings_as_of_10_1_2023 + total_non_residential_buildings_as_of_10_1_2023),
    building_floors = str_extract_all(number_of_stories, "\\d+") %>% 
      map_dbl(~ mean(as.numeric(.x))), 
    svi = svi * 100,
    sheetrock_status_flag = as.factor(if_else(sheetrock_status_flag == "Yes", 1, 0)),
    sandy_development_flag = as.factor(if_else(sandy_development_flag == "Yes", 1, 0)),
    mechanical_ventilation = as.factor(mechanical_ventilation),
    pct_female = (female_population / total_population) * 100,
    pct_female_scaled = scale(pct_female),
    pct_pop_under_4 = ((population_4) / total_population) * 100,
    pct_pop_under_18 = ((population_4 + population_5_17) / total_population) * 100,
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
  mutate(ndvi = mean(ndvi),
         f_deviation_smooth = mean (f_deviation_smooth)) %>%
  ungroup() %>%
  arrange(year, tds_number) %>%
  mutate(
    senior_development_flag = factor(senior_development_flag, levels = c("no", "partially", "exclusively")),
    year_month_MB_factor = factor(year_month_MB_factor, levels = c("Jul 2018", "Apr 2019", "Jul 2019", "Aug 2019",
                                                                   "Sep 2019", "Oct 2019", "Nov 2019")),
  )%>%
  mutate(mechanical_ventilation = if_else(mechanical_ventilation == 1, "Yes", "No"),
         sheetrock_status_flag = if_else(sheetrock_status_flag  == 1, "Yes", "No"),
         senior_development_flag = if_else(senior_development_flag == "exclusively", "Yes", "No"),
         buildings = building_total,
         sandy_development_flag = if_else(sandy_development_flag  == 1, "Yes", "No"),
  ) %>%
  mutate(class = "rad") %>%
  select(class,  buildings, archetype, building_age, building_sq_footage, 
         building_floors, sheetrock_status_flag, mechanical_ventilation,
         scattered_site_flag, total_households,pct_female, pct_pop_under_4, 
         senior_development_flag, building_median_income, pct_pop_nhblack, pct_pop_nhwhite, eop,
         pct_pop_hispanic, svi,
         ct_percent_pov,ct_percent_nhb, ct_percent_nhw, ct_percent_h,
         sandy_development_flag, intersects_coastal_flood_max, intersects_nuisance_flood_max,year_month_MB_factor,
         intersects_deep_flood_max, ndvi_quartile, heat_quartile, f_deviation_smooth, ndvi, geographical_borough, tds_number) 

setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")

write.csv(analytical_dat, "rad_data_table.csv", row.names = FALSE)
