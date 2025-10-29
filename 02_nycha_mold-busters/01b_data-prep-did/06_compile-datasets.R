
# the purpose of this script is to combine all covariate, denominator, and 
# strata data so that they can be combined with the outcome data on the 
# encrypted computer to create the analytical dataset.

require(tidyverse)
require(lubridate)
require(dplyr)
require(sf)
require(fst)
require(janitor)

setwd("~/Desktop/projects/Mattlab/F31/data/NYC Building Footprints")
nyc_shapes <- st_read("geo_export_de70d948-26b6-4497-9cef-dc2bad06d1c1.shp") %>%
  as.data.frame() %>%
  select(bin, heightroof)

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/prepped-for-doh")
pop_data <- read.fst("nycha_pop_data.fst") %>%
  mutate(tds_number = as.numeric(tds_number)) %>%
  rename(bin = bin_or_ct) %>%
  left_join(nyc_shapes) %>%
  group_by(tds_number, building_number) %>%
  mutate(heightroof_nycha = mean(heightroof, na.rm = T)) %>%
  select(-bin, -heightroof) %>%
  ungroup()


setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
nycha_ct <- read.csv("nycha-ct.csv") %>%
  mutate(geoid_clean = as.character(geoid_clean))

pop_data <- pop_data %>%
  left_join(nycha_ct, by = c("tds_number", "building_number")) %>%
  mutate(geoid_clean = if_else(group != "nycha", GEOID, geoid_clean)) %>%
  select(-GEOID) %>%
  rename(geoid = geoid_clean) %>%
  unique() # make sure we remove any duplicates 
  

# now we can add other data by geoid's

# adding the census tract population data ---------------------------------
setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/census-data")
ct_pop <- read.fst("census_data.fst") %>%
  select(-tr2020ge, -NAME) %>%
  rename(geoid = GEOID)


pop_data <- pop_data %>% left_join(ct_pop)


# now add prism data ------------------------------------------------------

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/prism-monthly")
prism <- read.csv("prism_by_tract_2016_2024.csv") %>%
  mutate(geoid = as.character(geoid))

pop_data <- pop_data %>% left_join(prism)

# now add air pollution data ---------------------------------------------

# Set working directory
setwd("~/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/processed_air_pollution")

# List of files and short names
file_info <- list(
  pm25 = "nyccas_pm25_by_tract_2016_2023.csv",
  o3   = "nyccas_o3_by_tract_2016_2023.csv",
  bc  = "nyccas_bc_by_tract_2016_2023.csv",
  no2  = "nyccas_no2_by_tract_2016_2023.csv"
  
)

# Function to read and process each file
process_file <- function(file) {
  read.csv(file) %>%
    dplyr::select(-ID) %>%
    mutate(geoid = as.character(GEOID)) %>%
    dplyr::select(-GEOID)
}

# Apply to all files
pollution_data <- map(file_info, process_file)

# Access each one individually if needed
pm <- pollution_data$pm25
o3   <- pollution_data$o3
no2  <- pollution_data$no2
bc  <- pollution_data$bc

pop_data <- pop_data %>% 
  left_join(pm) %>% 
  left_join(o3) %>% 
  left_join(no2) %>% 
  left_join(bc)


# add traffic data 
setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/traffic-data")
traffic <- read.fst("processed_traffic_2016_2023.fst") %>%
  mutate(year = as.integer(year),
         geoid = as.character(GEOID))

pop_data <- pop_data %>% 
  left_join(traffic) 

# now add ndvi data -------------------------------------------------------

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/ndvi")

# List of CSV file names
files <- c("monthly_ndvi_tracts-2016.csv",
           "monthly_ndvi_tracts-2017.csv",
           "monthly_ndvi_tracts-2018.csv",
           "monthly_ndvi_tracts-2019.csv",
           "monthly_ndvi_tracts-2020.csv",
           "monthly_ndvi_tracts-2021.csv",
           "monthly_ndvi_tracts-2022.csv",
           "monthly_ndvi_tracts-2023.csv",
           "monthly_ndvi_tracts-2024.csv")

# Function to read and clean a single file
read_and_process_ndvi <- function(file) {
  read.csv(file) %>%
    clean_names() %>%
    mutate(geoid = as.character(geoid)) %>%
    group_by(geoid) %>%
    summarize(ndvi = max(ndvi, na.rm = T), .groups = "drop") %>%
    mutate(year = as.numeric(str_extract(file, "\\d{4}"))) %>% # Extract year from filename
    ungroup()
}

# Apply the function to each file and combine
ndvi_all_years <- map_df(files, read_and_process_ndvi)

pop_data <- pop_data %>% left_join(ndvi_all_years)


# add development data like intervention groups and LCGA groups -----------

# lcga groups
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")
lcga_classes <- read.fst("classes_lcga2021.fst") %>%
  mutate(tds_number = as.numeric(as.character(tds_number))) %>%
  select(class5, tds_number) %>%
  unique() %>%
  mutate(class_letter = case_when(
    class5 == 1 ~ "D",
    class5 == 2 ~ "E",
    class5 == 3 ~ "A",
    class5 == 4 ~ "C",
    class5 == 5 ~ "B",)) 

# development characteristics
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
char <- read.fst("dev_characteristics_MB.fst") %>%
  mutate(tds_number = as.numeric(tds_number)) %>%
  select(construction_date, geographical_borough, tds_number, senior_development_flag,
         consolidated_tds_number, archetype, age_of_the_building_as_of_10_1_2023,
         building_floors, building_sq_footage_by_building, residential_buildings) %>%
  unique() %>%
  mutate(age_of_the_building_as_of_10_1_2018 = age_of_the_building_as_of_10_1_2023 -5)

# intervention dates
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
intervention <- read.fst("time_since_intervention.fst") %>%
  mutate(tds_number = as.numeric(TDS)) %>%
  dplyr::select(tds_number, intervention_date) 


pop_data <- pop_data %>% 
  left_join(lcga_classes) %>%
  left_join(char) %>%
  left_join(intervention) %>%
  unique() 

a <- pop_data %>%
  select(tds_number, class_letter) %>%
  unique()

# get year constructed by control geoid's 

setwd("~/Desktop/projects/Mattlab/F31/data/NYC Building Footprints")
nyc_building_points <- st_read("geo_export_de70d948-26b6-4497-9cef-dc2bad06d1c1.shp") %>%
  select(cnstrct_yr, bin, heightroof) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  st_centroid()  %>%
  st_transform(crs = 2263)

control_geoid <- pop_data %>%
  filter(group == "controls") %>%
  select(geoid) %>%
  unique()

# Read in block group shapefile for a specific state and county
setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/tl_2020_36_tract")
tracts <- st_read("tl_2020_36_tract.shp") %>%
  filter(COUNTYFP %in% c("005", "047", "061", "085", "081"))%>%
  select(GEOID) %>%
  st_transform(crs = 2263) 

tract_constr <- st_join(tracts, nyc_building_points, left = TRUE) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(GEOID) %>%
  summarize(cnstrct_yr = round(mean(cnstrct_yr, na.rm = T),0),
            heightroof_control = mean(heightroof, na.rm = T)) %>%
  ungroup()


pop_data <- pop_data %>% 
  left_join(tract_constr) %>%
  mutate(
    heightroof = if_else(group == "nycha", heightroof_nycha, heightroof_control),
    heightroof = coalesce(heightroof, heightroof_control)
  )


# add an indicator for nycha neighbor

setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
nb_1000 <- read.csv ("nycha-neighbors-1000.csv") 
nb_500 <- read.csv ("nycha-neighbors-500.csv") 
nb_250 <- read.csv ("nycha-neighbors-250.csv") 
nb_100 <- read.csv ("nycha-neighbors-100.csv") 



pop_data <- pop_data %>%
  mutate(nycha_neighb_1000 = if_else(geoid %in% nb_1000$geoid, 1,0),
         nycha_neighb_500 = if_else(geoid %in% nb_500$geoid, 1,0),
         nycha_neighb_250 = if_else(geoid %in% nb_250$geoid, 1,0),
         nycha_neighb_100 = if_else(geoid %in% nb_100$geoid, 1,0))


# add covid data 

covid <- read.csv("tract_level_deathrates.csv") %>%
   mutate(GEOID = as.character(GEOID)) 


pop_data <- pop_data %>%
  left_join(covid)

a <- pop_data %>%
  select(GEOID, group, deathrate, year)



# add lat and long --------------------------------------------------------

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/lat-long")
shp <- read.fst( "lat-long.fst")

shp2 <- shp %>%
  select(-geoid)

pop_data_nycha <- pop_data %>%
  filter(group == "nycha") %>%
  left_join(shp2, by = c("tds_number", "building_number", "group"))


pop_data_con <- pop_data %>%
  filter(group == "controls") %>%
  left_join(shp, by = c("geoid", "tds_number", "building_number", "group"))

pop_data <- bind_rows(pop_data_nycha, pop_data_con)

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/prepped-for-laptop")
write.fst(pop_data, "covariate_data.fst")


setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data-processed-correlations")
mold_data <- read.fst("mold-yearly.fst") %>%
  rename(year = report_date_year_nyc)

mold_set <- pop_data %>%
  filter(group=="nycha") %>%
  filter(year %in% c(2016:2018)) %>%
  unique() %>%
  left_join(mold_data) %>%
  mutate(across(c(total_reports, total_severe_weather, total_repeats_6), ~ifelse(is.na(.), 0, .)))

names(mold_set)

sum(mold_data$total_reports)


setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/prepped-for-laptop")
write.fst(mold_set, "covariate_mold_data.fst")

mold_set %>%
  select(tds_number, building_number) %>%
  n_distinct()


pop_data %>%
  filter(group != "nycha") %>%
  select(geoid) %>%
  n_distinct()

mold_set %>%
  select(tds_number, building_number) %>%
  n_distinct()

a <- pop_data%>%
  select(tds_number, class_letter) %>%
  unique()
  
