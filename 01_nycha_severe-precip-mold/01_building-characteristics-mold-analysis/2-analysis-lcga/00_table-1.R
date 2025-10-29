# load packages -----------------------------------------------------------


require(dplyr)
require(ggplot2)
require(purrr)
require(RColorBrewer)
require(gridExtra)
require(ggcorrplot)
require(ppsr)
require(fst)
require(lme4)
require(glmmTMB)
require(DHARMa)
require(broom.mixed)
require(MuMIn)
require(gt)
require(splines2)
require(gtsummary)
require(gtExtras)
require(forestploter)
require(tidyverse)


# read in the data --------------------------------------------------------
set.seed(444)

setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")
analytical_data <- read.fst("analytical_data_table_1.fst") %>%
  filter(year == 2021) %>%
  mutate(sandy_development_flag = as.numeric(if_else(sandy_development_flag =="Yes",1,0)))%>%
  mutate(sheetrock_status_flag = as.numeric(if_else(sheetrock_status_flag == "Yes",1,0)))%>%
  mutate(mechanical_ventilation = as.numeric(if_else(mechanical_ventilation ==1,1,0)))%>%
  mutate(senior_development_flag = as.numeric(if_else(senior_development_flag =="exclusively",1,0))) %>%
  mutate(pop_over_62 = (population_62_and_over / total_population *100),
         pct_female = (female_population / total_population) * 100,
         pct_pop_under_18 = ((population_4 + population_4_5  + population_6_9 + population_10_13 + population_14_17) / total_population) * 100,
         pct_pop_18_39 = (population_18_39 / total_population) * 100,
         pct_pop_40_plus = (population_40_and_over / total_population) * 100,
         scattered_site = as.factor(scattered_site_flag),
         sheetrock_status_flag = as.factor(if_else(sheetrock_status_flag == "Yes", 1, 0)),
         pct_pop_hispanic = (hispanic_population / total_population) * 100,
         pct_pop_nhblack = (black_population / total_population) * 100,
         building_median_income = median_household_income,
         pct_pop_nhwhite = (white_population / total_population) * 100)


names(analytical_data)


analytical_data <- analytical_data %>% # fix missing building median income
  group_by(year, archetype, total_population) %>%
  mutate(median_household_income_rep = mean(median_household_income, na.rm = T)) %>%
  ungroup() %>%
  mutate(median_household_income = if_else(is.na(median_household_income), median_household_income_rep, median_household_income))


summary(analytical_data)

# Define the variables to include in the table
variables_to_summarize <- c(
  "archetype", "building_year_group", "building_sq_footage_group", "building_floor_group",
  "sheetrock_status_flag", "mechanical_ventilation", "scattered_site", "pct_female", 
  "pct_pop_under_18", "pct_pop_18_39", "pct_pop_40_plus",  "pop_over_62",
  "senior_development_flag", "building_median_income", "pct_pop_hispanic",
  "pct_pop_nhblack","pct_pop_nhwhite", "total_households", "total_population",
  "svi", "ct_percent_pov", "ct_percent_nhb", 
   "ct_percent_h", "ct_percent_nhw", "intersects_deep_flood", 
  "intersects_coastal_flood", "intersects_nuisance_flood", "sandy_development_flag", "heat_quartile", 
  "ndvi_quartile",  "geographical_borough", "year_month_MB_factor"
)

# Define clean labels for variables
variable_labels <- list(
  archetype ~ "Building Archetype",
  building_year_group ~ "Building Year Group",
 # building_sq_footage_group ~ "Building Square Footage (per 1,000)",
  sheetrock_status_flag ~ "Sheetrock Status",
  mechanical_ventilation ~ "Mechanical Ventilation",
  scattered_site ~ "Scattered Site Indicator",
  pct_female ~ "Percent Female",
  pct_pop_under_18 ~ "Percent Population Under 18",
  pct_pop_18_39 ~ "Percent Population 18-39",
  pct_pop_40_plus ~ "Percent Population 40+",
  senior_development_flag ~ "Senior Development Indicator",
  building_median_income ~ "Median Building Income",
  pct_pop_hispanic ~ "Percent Hispanic",
  pct_pop_nhblack ~ "Percent Non-Hispanic Black",
  pct_pop_nhwhite ~ "Percent Non-Hispanic White",
  svi ~ "Census Tract Social Vulnerability Index (SVI)",
 # ct_median_income ~ "Median Census Tract Income",
  ct_percent_pov ~ "Census Tract Poverty Rate (%)",
 # ct_percent_h_or_nhb ~ "Census Tract % Hispanic or Non-Hispanic Black",
  ct_percent_nhb ~ "Census Tract % Non-Hispanic Black",
  ct_percent_nhw ~ "Census Tract % Non-Hispanic White",
  ct_percent_h ~ "Census Tract % Hispanic",
  intersects_deep_flood ~ "Intersects Deep Flood Zone",
  intersects_coastal_flood ~ "Intersects Coastal Flood Zone",
  intersects_nuisance_flood ~ "Intersects Nuisance Flood Zone",
  heat_quartile ~ "Heat Quartile",
  ndvi_quartile ~ "NDVI Quartile",
  sandy_development_flag ~ "Development damaged by Sandy",
  geographical_borough ~ "Borough",
  year_month_MB_factor ~ "Mold Busters date"
  
)

library(gtsummary)
library(gt)
library(dplyr)

# Define variable groupings
variable_groups <- list(
  "Building Variables" = c("archetype", "building_year_group", "building_sq_footage_group",
                           "sheetrock_status_flag", "mechanical_ventilation", "scattered_site"),
  
  "Building Demographics" = c("pct_female", "pct_pop_under_18", "pct_pop_18_39", "pct_pop_40_plus", "pct_pop_hispanic",
                              "pct_pop_nhblack","pct_pop_nhwhite",
                              "senior_development_flag", "building_median_income"),
  
  "Neighborhood Demographics" = c("svi", "ct_percent_pov", "ct_percent_nhb", "ct_percent_h", "ct_percent_nhw"),
  
  "Other Exposure Characteristics" = c("intersects_deep_flood", "intersects_coastal_flood",
                                       "intersects_nuisance_flood", "sandy_development_flag",
                                       "heat_quartile", "ndvi_quartile", "geographical_borough",
                                       "year_month_MB_factor")
)

# Convert variable groupings to a lookup table
variable_groups_df <- enframe(variable_groups, name = "group", value = "variable") %>%
  unnest(variable)

# Create the summary table
summary_table <- tbl_summary(
  data = analytical_data,
  include = all_of(variables_to_summarize),
  label = variable_labels
) %>%
  modify_table_body(
    ~ .x %>%
      left_join(variable_groups_df, by = "variable") %>%  # Add category column
      relocate(group, .before = label)  # Move it to the left
  ) %>%
  modify_header(group ~ "**Category**", label ~ "**Variable**")  # Rename headers

# Convert to gt and adjust font size
summary_table_gt <- summary_table %>%
  as_gt() %>%
  gt::tab_options(
    table.font.size = px(12),  # Compact font
  )

# Print the table
summary_table_gt

