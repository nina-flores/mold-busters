### the objective of the file is to figure out which buildings NYCHA is following
### over the years. We will want to use this to determine which buildings should 
### be included in analysis. We also clean up the building-level data.

# Load libraries
library(tidyverse)
library(readxl)
require(fst)
require(classInt)
require(gtsummary)

# read in and bind building data ------------------------------------------

# Set working directory
setwd("~/Desktop/projects/Mattlab/F31/data/Tenant data updated files")

build_char_2021 <- read_excel("NYCHA data for Mold Research Columbia University Jan 2021 additional data.xlsx", sheet = 2) %>%
  mutate(year = 2021)

build_char_2021 %>%
  select(`TDS#`) %>%
  n_distinct() # seems we were given development characteristics as of 2021,
# as we have 285 tds in the development file. 
# may need to ask if we want earlier year files. 

build_char_2022 <- read_excel("NYCHA data for Mold Research Columbia University Jan 2022 additional data.xlsx", sheet = 2) %>%
  mutate(year = 2022) %>%
  select(-`X Population`)

build_char_2023 <- read_excel("NYCHA data for Mold Research Columbia University Jan 2023 additional data.xlsx", sheet = 2) %>%
  mutate(year = 2023)%>%
  select(-`X Population`)


build_char_2020 <- read_excel("NYCHA data for Mold Research Columbia University Jan 2020 additional data.xlsx", sheet = 2) %>%
  mutate(year = 2020)


# bind all together
build_char <- rbind(build_char_2020,
                    build_char_2021,
                    build_char_2022,
                    build_char_2023
                    ) %>%
  janitor::clean_names() %>%
  mutate(tds_number = as.numeric(tds_number))


dta <- build_char %>%
  group_by(tds_number, building_number) %>%
  mutate(n = n()) %>%
  ungroup()

tds_in_2023 <- build_char_2023 %>%
  janitor::clean_names() %>%
  select(tds_number) %>%
  unique()

# question about should this consider those in the file all 9 years versus
# just the 3 of analysis. For this analysis, just using the complete case for 
# the 3 years needed. Only lost 85 buildings from start to end of that period.


# add development characteristics -----------------------------------------

setwd("~/Desktop/projects/Mattlab/F31/data/NYCHA Data_1_8-29-2024")
# add development level characteristics
dev_char <- read_excel("Schedule -Ventilation-Dev Characteristics  10.24.2023 Rev 08.01.2024.xlsx", 
                       sheet = 4,
                       skip = 2) %>%
  janitor::clean_names()   %>%
  mutate(building_year = year(construction_date),
         building_sq_footage_by_building = building_coverage_square_footage_calculated_in_gis/(total_residential_buildings_as_of_10_1_2023 + total_non_residential_buildings_as_of_10_1_2023),
         building_floors = str_extract_all(number_of_stories, "\\d+") %>% 
           map_dbl(~ mean(as.numeric(.x))))



dta <- dta %>% 
  left_join(dev_char)


setwd("~/Desktop/projects/Mattlab/F31/data/NYCHA Data_1_8-29-2024")

# Read in sheet 3 of the development characteristics file
dev_char_s3 <- read_excel("Schedule -Ventilation-Dev Characteristics  10.24.2023 Rev 08.01.2024.xlsx", 
                          sheet = 3, skip = 1) %>%
  janitor::clean_names() %>%
  mutate(tds_number  = as.numeric(tds)) %>%
  mutate(mechanical_ventilation = 1) %>%
  select(-development_name)

# Join these data

dta <- dta %>% 
  left_join(dev_char_s3, by = "tds_number") %>%
  mutate(mechanical_ventilation = ifelse(is.na(mechanical_ventilation), 0, mechanical_ventilation)) %>%
  mutate(mechanical_ventilation = ifelse(is.na(geographical_borough), NA_real_, mechanical_ventilation)) 


dta %>%
  filter(year == 2021) %>%
  summarize(sum(total_population),
            sum(total_households))


# add rad/pact  -----------------------------------------------------------

### read in the rad/pact data 
setwd("~/Desktop/projects/Mattlab/F31/data")
rad_data <- read.csv("rad_pact_clean.csv") %>%
  select(tds_number, conversion_date) %>%
  mutate(tds_number = as.numeric(tds_number)) %>%
  unique() %>%
  select(tds_number, conversion_date)

a <- dta %>% 
  left_join(rad_data) %>%
  mutate(conversion_date = as.Date(conversion_date, format = "%Y-%m-%d")) %>%
  filter(conversion_date > as.Date("2019-12-31") & conversion_date < as.Date("2023-12-31") )

c <- a %>% select(tds_number, conversion_date) %>%
  unique()

dta <- dta %>% 
  left_join(rad_data) %>%
  mutate(conversion_date = as.Date(conversion_date, format = "%Y-%m-%d")) %>%
  filter(conversion_date > as.Date("2023-12-31") | is.na(conversion_date)) %>% # all conversion dates are not 2024 and beyond
  mutate(year_conversion = year(conversion_date)) %>%
  mutate(converted = if_else(year >= year_conversion,1,0)) %>%
  mutate(converted = if_else(is.na(converted),0, converted))  %>%
  group_by(tds_number, building_number) %>%
  mutate(n = n()) %>%
  filter(n == 4) %>% # dont want those that drop out before 2023
  ungroup() %>%
  filter(private_management_flag == "No")
  
#sum(dta_2023$total_households)
#sum(dta_2023$total_population)



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
  filter(year == 2021) %>%
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
write.fst(dta, "building_characteristics.fst")

dta %>%
  select(tds_number, building_number) %>%
  n_distinct() 

dta_dev <- dta %>% select(names(dev_char), names(dev_char_s3), residential_buildings,
                          building_year_group, building_sq_footage_group , building_floor_group)


# table of differences from 2021 to 2024 ----------------------------------

# Create a summary table
summary_table <- dta %>%
  filter(year == 2023) %>%
  mutate(pct_black = black_population/total_population,
         pct_white = white_population/total_population,
         pct_hispanic = hispanic_population/total_population) %>%
  dplyr::select(converted, 
                total_households, 
         total_population, 
         pct_white,                                             
         pct_black,                                             
         pct_hispanic,
         median_household_income,
         mechanical_ventilation,
         geographical_borough) %>%  
  tbl_summary(
    by = converted,  # Grouping by the dropped flag
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"), # Mean & SD for continuous, count & % for categorical
    missing = "no"  # Exclude missing data from calculations
  ) %>%
  modify_header(label = "**Variable**") %>%
  bold_labels()

# Print the table
summary_table

