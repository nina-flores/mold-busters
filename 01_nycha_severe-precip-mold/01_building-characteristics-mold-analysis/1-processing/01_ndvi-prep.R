require(dplyr)
require(fst)
require(lubridate)
require(tidyverse)
require(tigris)
require(sf)
require(janitor)

# setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/ndvi")
# 
# ndvi_2021 <- read.csv("monthly_ndvi_tracts_2021_2.csv") %>%
#   mutate(year = 2021,
#          month_name = month(month, label = TRUE),  # Convert numbers to month names 
#          date_month = as.Date(paste(year, month, 01, sep = "-"))) %>%
#   select(GEOID, year, date_month, NDVI)
# 
# ndvi_2022 <- read.csv("monthly_ndvi_tracts_2022_2.csv") %>%
#   mutate(year = 2022,
#          month_name = month(month, label = TRUE),  # Convert numbers to month names 
#          date_month = as.Date(paste(year, month, 01, sep = "-"))) %>%
#   select(GEOID, year, date_month, NDVI)
# 
# ndvi_2023 <- read.csv("monthly_ndvi_tracts_2023_2.csv") %>%
#   mutate(year = 2023,
#          month_name = month(month, label = TRUE),  # Convert numbers to month names 
#          date_month = as.Date(paste(year, month, 01, sep = "-"))) %>%
#   select(GEOID, year, date_month, NDVI)
# 
# ndvi <- rbind(ndvi_2021, ndvi_2022, ndvi_2023)
# 
# ndvi_dates <- ndvi
# 
# ndvi_max <- ndvi %>%
#   group_by(GEOID, year) %>%
#   summarize(ndvi_max_year = max(NDVI, na.rm = TRUE), .groups = "drop") %>%
#   ungroup() %>%
#   mutate(GEOID = as.character(GEOID))
# 
# nyc_tracts <- tracts(state = "NY", cb = TRUE, year = 2020) 
# 
# ndvi_max <- nyc_tracts %>% full_join(ndvi_max) %>%
#   filter(COUNTYFP %in% c("005", "047", "061", 
#                          "081", "085"))
# 
# 
# ndvi_max <- ndvi_max %>%
#   group_by(year) %>%
#   mutate(GEOID = as.character(GEOID),
#          ndvi_quartile = cut(ndvi_max_year, 
#                              breaks = quantile(ndvi_max_year,  probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), 
#                              include.lowest = TRUE, 
#                              labels = FALSE)) %>%
#   ungroup()
# 
# 
# ggplot(data = ndvi_max ) +
#   geom_sf(aes(fill = ndvi_quartile), color = "black", size = 0.1) +
#   labs(title = "New York City Census Tracts") +
#   theme_minimal()
# 
# ndvi_max <- ndvi_max %>%
#   as.data.frame() %>%
#   select(-geometry)


# using alternative satellite data to be consistent with other paper


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
    mutate(GEOID = as.character(geoid)) %>%
    group_by(geoid, GEOID) %>%
    summarize(ndvi = max(ndvi, na.rm = T), .groups = "drop") %>%
    mutate(year = as.numeric(str_extract(file, "\\d{4}"))) # Extract year from filename
}

# Apply the function to each file and combine
nyc_county_prefixes <- c("36005", "36047", "36061", "36081", "36085")

ndvi_all_years <- map_df(files, read_and_process_ndvi) %>%
  filter(year %in% 2021:2023) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  filter(substr(GEOID, 1, 5) %in% nyc_county_prefixes) %>%
  group_by(year) %>%
  mutate(ndvi_quartile = cut(ndvi, 
                             breaks = quantile(ndvi, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), 
                             include.lowest = TRUE, 
                             labels = FALSE)) %>%
  ungroup()



# write out dataset -------------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")
#write.fst(ndvi_dates, "ndvi_dates.fst")

write.fst(ndvi_all_years, "ndvi_max.fst")
