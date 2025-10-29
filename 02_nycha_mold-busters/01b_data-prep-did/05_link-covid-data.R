require(sf)
library(tidyr)
library(dplyr)
library(lubridate)
library(fst)


setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/covid")

death_data <- read.csv("deathrate-by-modzcta.csv") 

mod_zcta <- st_read("Modified Zip Code Tabulation Areas (MODZCTA)_20250605/geo_export_96174bf8-b8bb-49fa-92c3-5123d62f9f77.shp") %>%
  select(modzcta, pop_est)



long_df <- death_data %>%
  mutate(across(starts_with("DEATHRATE_"), ~ replace_na(.x, 0))) %>%
  pivot_longer(
    cols = starts_with("DEATHRATE_"),
    names_to = "group",
    names_prefix = "DEATHRATE_",
    values_to = "deathrate"
  ) %>%
  filter(!group %in% c("Bronx",
                        "Queens",
                        "Manhattan",
                        "Staten_Island",
                        "Brooklyn",
                        "Citywide")) %>%
  mutate(year = year(mdy(paste0("1/", date)))) %>%
  group_by(year, group) %>%
  summarize(deathrate = mean(deathrate, na.rm = TRUE), .groups = "drop") %>%
  rename(modzcta = group)

# Step 2: Create a complete grid of years and groups
all_years <- 2016:max(long_df$year)
all_modzcta<- unique(long_df$modzcta)

complete_df <- expand.grid(
  year = all_years,
  modzcta = all_modzcta,
  stringsAsFactors = FALSE
)

# Step 3: Join and fill missing values with 0
final_df <- complete_df %>%
  left_join(long_df, by = c("year", "modzcta")) %>%
  mutate(deathrate = replace_na(deathrate, 0))
  

covid_mod <- full_join(mod_zcta, final_df) %>%
  na.omit() %>%
  st_transform(crs = 32618) %>%
  filter(year != 2025) %>%
  mutate(death_number = deathrate/100000*pop_est)

# now link this to tracts
setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/tl_2020_36_tract")
all_ct <-  st_read("tl_2020_36_tract.shp") %>%
  filter(COUNTYFP %in% c("005", "047", "061", "085", "081")) %>%
  dplyr::select(GEOID) %>%
  st_transform(crs = 32618) 

# adding the census tract population data ---------------------------------
setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/census-data")
ct_pop <- read.fst("census_data.fst") %>%
  select(-tr2020ge, -NAME) %>%
  select(GEOID, total_population_ct_2023)

all_ct <- full_join(all_ct, ct_pop)


# Intersect MODZCTAs with tracts
zcta_tract_intersect <- st_intersection(covid_mod, all_ct)

# Calculate full tract area (for % overlap calc)
tract_area <- all_ct %>%
  select(GEOID) %>%
  mutate(full_area = st_area(.)) %>%
  st_drop_geometry()

# Join full tract area into the intersected data
zcta_tract_intersect <- zcta_tract_intersect %>%
  left_join(tract_area, by = "GEOID") %>%
  mutate(
    intersect_area = st_area(.),
    pct_area = as.numeric(intersect_area / full_area),
    est_pop_in_piece = pct_area * total_population_ct_2023,
    est_deaths_in_piece = (death_number * est_pop_in_piece) / pop_est
  )

# Aggregate estimated deaths and pop to the tract level
tract_deaths <- zcta_tract_intersect %>%
  group_by(GEOID, year) %>%
  summarize(
    total_deaths = sum(est_deaths_in_piece, na.rm = TRUE),
    est_pop = sum(est_pop_in_piece, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    deathrate = (total_deaths / est_pop) * 100000
  )

# Export
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
tract_deaths <- tract_deaths %>%
  as.data.frame() %>%
  select(GEOID, year, deathrate) 

write.csv(tract_deaths, "tract_level_deathrates.csv", row.names = FALSE)




