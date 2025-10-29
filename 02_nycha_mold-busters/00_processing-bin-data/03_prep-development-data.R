# the objective of the file is to have the development characteristic
# data for the NYCHA buildings in the mold busters analysis

# Load libraries
library(tidyverse)
library(readxl)
require(fst)
require(classInt)
require(gtsummary)

# read in and bind building data ------------------------------------------

# Set working directory
setwd("~/Desktop/projects/Mattlab/F31/data/Tenant data updated files")


# read in development characteristics --------------------------------------

setwd("~/Desktop/projects/Mattlab/F31/data/NYCHA Data_1_8-29-2024")

dev_char <- read_excel("Schedule -Ventilation-Dev Characteristics  10.24.2023 Rev 08.01.2024.xlsx", 
                       sheet = 4,
                       skip = 2) %>%
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
  mutate(mechanical_ventilation = 1) %>%
  select(-development_name)

# Join these data

dev_dta <- dev_char %>% 
  left_join(dev_char_s3, by = "tds_number") %>%
  mutate(mechanical_ventilation = ifelse(is.na(mechanical_ventilation), 0, mechanical_ventilation)) %>%
  mutate(mechanical_ventilation = ifelse(is.na(geographical_borough), NA_real_, mechanical_ventilation)) 


# add rad/pact  -----------------------------------------------------------

### read in the rad/pact data 
setwd("~/Desktop/projects/Mattlab/F31/data")
rad_data <- read.csv("rad_pact_clean.csv") %>%
  select(tds_number, conversion_date) %>%
  mutate(tds_number = as.numeric(tds_number)) %>%
  unique() %>%
  select(tds_number, conversion_date)

dta <- dev_dta %>% 
  left_join(rad_data) %>%
  mutate(conversion_date = as.Date(conversion_date, format = "%Y-%m-%d")) %>%
  filter(conversion_date > as.Date("2020-01-01", format = "%Y-%m-%d") | is.na(conversion_date)) %>%
  group_by(tds_number) %>%
  ungroup() %>%
  filter(private_management_flag == "No")

# Function to apply Jenks natural breaks and return labeled categories
apply_jenks_labels_descriptive <- function(variable, n_groups, type = "size") {
  breaks_c <- classInt::classIntervals(variable, n_groups, style = "jenks")$brks
  breaks <- round(breaks_c)  # Round break values for cleaner labels
  
  # Assign labels based on type
  if (type == "size") {
    labels <- c(
      paste0( breaks[1], "-", breaks[2]),
      paste0( breaks[2], "-", breaks[3]),
      paste0( breaks[3], "-", breaks[4]),
      paste0( breaks[4], "-", breaks[5])
      
    )
  } else if (type == "age") {
    labels <- c(
      paste0(breaks[1], "-", breaks[2]),
      paste0(breaks[2], "-", breaks[3]),
      paste0(breaks[3], "-", breaks[4]),
      paste0(breaks[4], "-", breaks[5])
      
    )
  } else if (type == "floor") {
    labels <- c(
      paste0(breaks[1], "-", breaks[2]),
      paste0(breaks[2], "-", breaks[3]),
      paste0(breaks[3], "-", breaks[4]),
      paste0(breaks[4], "-", breaks[5])
      
    )
  }
  
  # Apply labels
  cut(variable, breaks_c, include.lowest = TRUE, labels = labels)
}

# Apply the updated Jenks function with descriptive labels
jenks_data <- dta %>%
  select(tds_number, building_year, building_floors, building_sq_footage_by_building,total_residential_buildings_as_of_10_1_2023 ) %>%
  distinct() %>%
  mutate(
    residential_buildings = apply_jenks_labels_descriptive(total_residential_buildings_as_of_10_1_2023, 4, type = "size"),
    building_year_group = apply_jenks_labels_descriptive(building_year, 4, type = "age"),
    building_sq_footage_group = apply_jenks_labels_descriptive(building_sq_footage_by_building, 4, type = "size"),
    building_floor_group = apply_jenks_labels_descriptive(building_floors, 4, type = "floor")) %>%
  select(tds_number, building_year_group, building_floor_group, building_sq_footage_group, residential_buildings)

dta <- dta %>% left_join(jenks_data)


# write out ---------------------------------------------------------------

# Write out data
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
write.fst(dta, "dev_characteristics_MB.fst")

dta %>%
  select(tds_number) %>%
  n_distinct() 

dta_dev <- dta %>% select(names(dev_char), names(dev_char_s3), residential_buildings,
                          building_year_group, building_sq_footage_group , building_floor_group)

