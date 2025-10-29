# the goal of this script is to compile the non-NYCHA control census tracts of
# similar demographic, income, and baseline mold characteristics.

require(tidyverse)
require(lubridate)
require(dplyr)
require(sf)
require(fst)
require(tigris)
require(tidycensus)
require(readxl)
require(zoo)


# identify non-NYCHA block groups -----------------------------------------
# We need to be sure to ensure to exclude block groups with current 
# and previous NYCHA buildings pre rad conversions


setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/bins")
NYCHA_bin <- read.fst("NYCHA_bin.fst") %>%
  mutate(converted_date = as.Date(converted_date, format = "%m/%d/%Y"))

setwd("~/Desktop/projects/Mattlab/F31/data/NYC Building Footprints")
nyc_shapes <- st_read("geo_export_de70d948-26b6-4497-9cef-dc2bad06d1c1.shp") %>%
  select(bin, cnstrct_yr, heightroof)

NYCHA_shapes <- NYCHA_bin %>%
  left_join(nyc_shapes, by = "bin") %>%
  st_as_sf() %>%
  st_transform(crs = 2263)

# figure out which ones could not be matched by bin 
empty_geometries <- st_is_empty(NYCHA_shapes)

# add this info to the original dataset
NYCHA_shapes$empty_geometries <- empty_geometries # 40 total

dta_bin_shape <- NYCHA_shapes %>%
  group_by(tds_number) %>%
  arrange(building_number) %>%
  mutate(
    building_number_old = building_number,
    nearest_row = if_else(empty_geometries, map2_int(empty_geometries, building_number, ~ {
      if (.x) {
        non_empty_indices <- which(!empty_geometries)
        non_empty_indices[which.min(abs(building_number[non_empty_indices] - .y))]
      } else {
        NA_integer_
      }
    }), row_number())
  ) %>%
  ungroup() %>%
  group_by(tds_number) %>%
  mutate(
    geometry = if_else(empty_geometries, geometry[nearest_row], geometry),
    building_number_match = if_else(empty_geometries, building_number[nearest_row], building_number)
  ) %>%
  ungroup()

NYCHA_shapes <- dta_bin_shape %>%
  select(-nearest_row, -building_number_old, -building_number_match) 

rm(nyc_shapes)

# link to block group shapes. Using 2020 shape file.

# # Read in block group shapefile for a specific state and county # census was down one day and this stopped working; pulled actual files
# tracts <- tigris::tracts(state = "NY", 
#                              county = c("005", "047", "061", "085", "081"),
#                              year = 2020) %>%
#   select(GEOID) %>%
#   st_transform(crs = 2263)


setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/tl_2020_36_tract")
tracts <- st_read("tl_2020_36_tract.shp") %>%
  filter(COUNTYFP %in% c("005", "047", "061", "085", "081")) %>%
  select(GEOID) %>%
  st_transform(crs = 2263)

# perform intersection 
intersects <- st_intersects(tracts, NYCHA_shapes)

# Get indices of tracts that don't intersect with any NYCHA shape
non_intersect_indices <- which(lengths(intersects) == 0)

# Subset the tracts
non_nycha_ct <- tracts[non_intersect_indices, ]

# now only have block groups without past or present NYCHA buildings. Want to
# refine this possible control group by linking demographic characteristics.


# also get the NYCHA block groups for later
nycha_ct <- st_join(NYCHA_shapes, tracts) %>%
  as.data.frame() %>%
  select(bin, GEOID) %>%
  unique()

# link demographic data ---------------------------------------------------

# Get median income from ACS
# --------------------------------------------------------------
acs_data <- get_acs(
  geography = "tract",
  variables = "B19013_001",  # Median household income
  state = "NY",
  county = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
  year = 2020,  
  survey = "acs5",
)

# Process the ACS income data
acs_processed <- acs_data %>%
  select(GEOID, estimate) %>%
  rename(median_income = estimate)

non_nycha_census <- non_nycha_ct %>%
  left_join(acs_processed) %>% 
  filter(!is.na(median_income))

# read in NYCHA building data to compare to -------------------------------
# but ensure that this is reflective of the NYCHA buildings that 
# will be in the analytical sample
# need to remove (a) pre-2019 rad/pact conversions
# (b) third party managed buildings and
# (c) the 5% of pilot buildings that we are removing from analysis

setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
intervention <- read.fst("time_since_intervention.fst") %>%
  mutate(tds_number = as.numeric(TDS)) %>%
  dplyr::select(tds_number, intervention_date) %>%
  mutate( year_MB = year(as.Date(intervention_date)),
          intervention_date = as.character(intervention_date),
          intervention_date = if_else(is.na(intervention_date), "Unknown/privately managed", intervention_date))


setwd("~/Desktop/projects/Mattlab/F31/data/Tenant data updated files")

# Define a function to read and process the CSV file
read_and_process <- function(year) {
  file_name <- paste0("NYCHA data for Mold Research Columbia University Jan ", year, " additional data.xlsx")
  
  read_excel(file_name, sheet = 2) %>%
    janitor::clean_names() %>%  # Clean column names
     mutate(year = year,
            tds_number = as.numeric(tds_number)) %>%
     select(tds_number,
            building_number,
            year,
            population_4,
            population_5_17,
            population_18_39,
            population_40_and_over,
            median_household_income,
            total_population,
            female_population,
            total_households)
}

# Loop through 2016-2023 and combine all into one dataframe
years <- 2016:2023
building_data <- map_dfr(years, read_and_process) %>%
  mutate(building_number = if_else(building_number == "4E", "4.3", building_number),
         building_number = if_else(building_number == "4W", "4.7", building_number),
         building_number = as.numeric(building_number)) %>%
  left_join(NYCHA_shapes) %>%
  select(-address, -cnstrct_yr) %>%
  unique() %>%
  filter(is.na(converted_date) | converted_date > as.Date("1/1/2020", format = "%m/%d/%Y")) %>%
  left_join(intervention) %>%
  filter(intervention_date != "Unknown/privately managed") %>% # removing privately managed because we cannot confirm that they received intervention
  filter(year_MB != 2018) %>% # removing pilot buildings bc their experience with intervention was different
  group_by(bin) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(!(n < 4 )) %>% # making sure there is data at least through 2019 on a building
  filter(!(n > 8 )) %>% # removing the few bins that serve multiple buildings as we will not know which building to attribute to, n = 5
  drop_na(bin) %>%
  mutate(GEOID = NA) %>%
  unique() 

a <- building_data %>%
  select(building_number, tds_number, converted_date) %>%
  unique() %>%
  filter(converted_date < as.Date("1/1/2020", format = "%m/%d/%Y"))  %>%
  left_join(intervention)

a %>%
  select(tds_number) %>%
  n_distinct()
  

# Create a complete dataset of all bin-year combinations
complete_df <- expand.grid(bin = unique(building_data$bin), year = c(2016:2024)) %>%
  left_join(building_data) %>%
  arrange(bin, year)

# Fill missing values by rolling forward
complete_df <- complete_df %>%
  group_by(bin) %>%
  fill(everything(), .direction = "down") %>%
  unique() %>%
  select(bin, year, tds_number, building_number, population_4, population_5_17, 
         population_18_39, population_40_and_over, female_population, total_population,
         median_household_income, total_households) %>%
  ungroup()


# get development level population for the buildings that are present for analysis
pop_data <- complete_df %>%
  select(year, tds_number, building_number,population_4, population_5_17, 
         population_18_39, population_40_and_over, female_population, 
         median_household_income, total_households, total_population) %>%
  unique() %>% # remove duplicates from multiple bins in some buildings
  group_by(tds_number, building_number, year) %>%
  summarize(population_4 = population_4,
            population_5_17 = population_5_17,
            population_18_39 = population_18_39,
            population_40_and_over = population_40_and_over,
            female_population = female_population,
            total_population = total_population,
            analyticial_total_population = sum(population_5_17, population_18_39, population_40_and_over),
            median_household_income = mean(median_household_income),
            total_households = total_households) %>%
  ungroup() 

# Extract IQR information from 2016 for now - will update to 2019
iqr_info <- complete_df %>%
  select(year, tds_number, building_number, median_household_income, total_households) %>%
  unique() %>%
  filter(year %in% 2016:2020) %>%
  group_by(tds_number) %>%
  summarize(median_household_income = weighted.mean(median_household_income, total_households, na.rm = T)) %>% # aggregating the building level info to the development
  ungroup() %>%
  summarise(
    lower_income = quantile(median_household_income, 0, na.rm = TRUE),
    upper_income = quantile(median_household_income, 1, na.rm = TRUE)
  ) %>%
  ungroup()


# Filter dataset based on IQR conditions for income
controls <- non_nycha_census %>%
  filter( median_income <= iqr_info$upper_income &
          median_income >= iqr_info$lower_income ) 

controls_export <- controls %>% as.data.frame() %>%
  select(GEOID)

#setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/prepped-for-laptop")
#write.csv(controls_export, "exclusive_controlsv3.csv", row.names = F)

# and add census tract
NYCHA <- complete_df %>% 
  select(bin, year, tds_number, building_number) %>%
  left_join(pop_data) %>%
  mutate(group = "nycha") %>%
  left_join(nycha_ct) %>%
  rename(bin_or_ct = bin) %>%
  select(-population_4)

complete_controls <- expand.grid(GEOID = unique(controls$GEOID), year = c(2016:2024)) 

control_g <- complete_controls %>%
  select(GEOID, year) %>%
  mutate(tds_number = NA,
         building_number = NA, 
         population_5_17 = NA,
         population_18_39 = NA,
         population_40_and_over = NA,
         total_population = NA,
         analyticial_total_population = NA,
         female_population = NA,
         median_household_income = NA,
         total_households = NA, 
         group = "controls") %>%
  mutate(bin_or_ct = as.numeric(as.character(GEOID))) %>%
  select(bin_or_ct, year, tds_number, median_household_income, building_number, population_5_17, population_18_39,             
         population_40_and_over, female_population, total_population, total_households,
         analyticial_total_population, group, GEOID) 


control_g$GEOID
# Create a complete dataset of all geoid-year combinations

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/prepped-for-doh")
nycha_pop_data <- rbind(NYCHA, control_g) %>%
  unique()

data <- nycha_pop_data %>%
  select(bin_or_ct, year, tds_number, building_number, group) %>%
  unique()

write.fst(nycha_pop_data, "nycha_pop_data.fst")
write.fst(data, "for_doh.fst")

b <- data %>%
  select(tds_number) %>%
  unique() 

nycha_pop_data %>%
  select(tds_number, building_number) %>%
  n_distinct()


# need to save out lat and long -------------------------------------------


NYCHA_shp <- NYCHA %>%
  select(bin_or_ct, tds_number, building_number) %>%
  left_join(NYCHA_shapes) %>% 
  group_by(tds_number, building_number) %>%
  slice(1) %>%
  select(geometry, tds_number, building_number) %>%
  st_as_sf() %>%
  mutate(geoid = NA, 
         group = "nycha")

non_NYCHA_shp <- controls %>%
  janitor::clean_names() %>%
  select(geoid, geometry) %>% 
  mutate(tds_number = NA,
         building_number = NA,
         group = "controls")

shp <- rbind(NYCHA_shp, non_NYCHA_shp) %>%
  st_transform(crs = 2263) %>%
  st_centroid() 

# Extract lon/lat from centroid geometry
coords <- st_coordinates(shp)
shp$lon <- coords[, 1]
shp$lat <- coords[, 2]

shp <- shp %>%
  as.data.frame() %>%
  select(-geometry)

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/lat-long")
write.fst(shp, "lat-long.fst")
  