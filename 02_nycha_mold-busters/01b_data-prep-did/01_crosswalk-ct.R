# the goal of this script is to crosswalk the 2018 data I need to the 
# 2020 boundaries for consistency later

require(tidyverse)
require(lubridate)
require(dplyr)
require(sf)
require(fst)
require(tigris)
require(tidycensus)
require(readxl)
require(zoo)

# read in the 2020-2010 block group crosswalk file from ipums
setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/nhgis_bg2010_tr2020_36")
cw <- read.csv("nhgis_bg2010_tr2020_36.csv") %>%
  select(bg2010ge, tr2020ge, wt_pop, wt_hh, wt_hu) %>%
  mutate(GEOID = as.character(bg2010ge)) %>%
  group_by(bg2010ge)

 a <- load_variables("acs5", year =2018)
# b <- load_variables("acs5", year =2023)
# --------------------------------------------------------------
# Named vector for the variables we need
# Race variables from B03002
vars_race <- c(
  "ct_total_population" = "B03002_001",
  "nhw_ct" = "B03002_003",
  "nhb_ct" = "B03002_004",
  "hispanic_ct" = "B03002_012",
  "poverty_count" = "B17001_002",
  "poverty_total" = "B17001_001"
)

# Age by sex variables from B01001
vars_age_sex <- c(
  "male_5_9" = "B01001_004", "male_10_14" = "B01001_005", "male_15_17" = "B01001_006",
  "male_18_19" = "B01001_007", "male_20" = "B01001_008", "male_21" = "B01001_009", 
  "male_22_24" = "B01001_010", "male_25_29" = "B01001_011", "male_30_34" = "B01001_012", 
  "male_35_39" = "B01001_013", "male_40_44" = "B01001_014", "male_45_49" = "B01001_015", 
  "male_50_54" = "B01001_016", "male_55_59" = "B01001_017", "male_60_61" = "B01001_018", 
  "male_62_64" = "B01001_019", "male_65_66" = "B01001_020", "male_67_69" = "B01001_021", 
  "male_70_74" = "B01001_022", "male_75_79" = "B01001_023", "male_80_84" = "B01001_024", 
  "male_85_plus" = "B01001_025",
  "total_female" = "B01001_026", 
  "female_5_9" = "B01001_028", "female_10_14" = "B01001_029", "female_15_17" = "B01001_030",
  "female_18_19" = "B01001_031", "female_20" = "B01001_032", "female_21" = "B01001_033", 
  "female_22_24" = "B01001_034", "female_25_29" = "B01001_035", "female_30_34" = "B01001_036", 
  "female_35_39" = "B01001_037", "female_40_44" = "B01001_038", "female_45_49" = "B01001_039", 
  "female_50_54" = "B01001_040", "female_55_59" = "B01001_041", "female_60_61" = "B01001_042", 
  "female_62_64" = "B01001_043", "female_65_66" = "B01001_044", "female_67_69" = "B01001_045", 
  "female_70_74" = "B01001_046", "female_75_79" = "B01001_047", "female_80_84" = "B01001_048", 
  "female_85_plus" = "B01001_049",
  "median_income" = "B19013_001", 
  "median_building_age" = "B25037_001",
  "total_households" = "B11001_001",
  "total_housing_units" = "B25001_001"
)


# health insurance info is by age and sex so this is kind of a pain to pull:

# Load the ACS variable list again if needed
a <- load_variables("acs5", year = 2018)

# Identify uninsured variables by label text
no_insurance_vars <- a %>%
  filter(str_detect(name, "^B27010_"),
         str_detect(label, "No health insurance coverage$")) %>%
  pull(name)

public_only_vars <- a %>%
  filter(str_detect(name, "^B27010_"),
         str_detect(label, "With Medicaid")) %>%
  pull(name)

insurance_relevant_pop <- a %>%
  filter(str_detect(name, "^B27010_"),
         str_detect(label, "Estimate!!Total!!Under 19 years$") |
           str_detect(label, "Estimate!!Total!!19 to 34 years$") |
           str_detect(label, "Estimate!!Total!!35 to 64 years$")) %>%
  pull(name)


# Combine all variables
vars_acs <- c(vars_age_sex, vars_race,
              set_names(no_insurance_vars, paste0("no_insurance_", no_insurance_vars)),
              set_names(public_only_vars, paste0("public_only_", public_only_vars)),
              set_names(insurance_relevant_pop, paste0("relevant_pop_", insurance_relevant_pop)))



acs_2023 <-   get_acs(
    geography = "tract",
    variables = vars_acs,
    state = "NY",
    county = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
    year = 2023,
    survey = "acs5",
    geometry = FALSE
  )

acs_2018 <-   get_acs(
  geography = "block group",
  variables = vars_acs,
  state = "NY",
  county = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
  year = 2018,
  survey = "acs5",
  geometry = FALSE
)



# # Clean and combine the data
# # --------------------------------------------------------------

# Define additive and non-additive variables
additive_vars <- c(
  "total_population_ct_2018", "total_analytical_pop_2018",
  "hispanic_ct_2018", "nhb_ct_2018", "nhw_ct_2018",
  "male_5_17_2018", "male_18_39_2018", "male_40_plus_2018", "total_female_2018",
  "female_5_17_2018", "female_18_39_2018", "female_40_plus_2018",
  "population_5_17_2018", "population_18_39_2018", "population_40_and_over_2018",
  "total_no_insurance_2018","insurance_pop_2018", "total_public_only_2018", "poverty_count_2018",
  "poverty_total_2018"
)

non_additive_vars <- c("median_income_2018", "median_building_age_2018")


acs_2018_processed <- acs_2018 %>%
  select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  select(GEOID, NAME, ct_total_population, hispanic_ct, nhb_ct, nhw_ct, median_income,
         male_5_9, male_10_14, male_15_17,
         male_18_19, male_20, male_21, male_22_24, male_25_29, male_30_34, male_35_39,
         male_40_44, male_45_49, male_50_54, male_55_59, male_60_61, male_62_64 ,
         male_65_66, male_67_69, male_70_74, male_75_79, male_80_84, male_85_plus,
         total_female, female_5_9, female_10_14, female_15_17,
         female_18_19,  female_20, female_21, female_22_24, female_25_29, female_30_34, female_35_39,
         female_40_44, female_45_49, female_50_54, female_55_59, female_60_61, female_62_64 ,
         female_65_66, female_67_69, female_70_74, female_75_79, female_80_84, female_85_plus,
         median_building_age, total_households, total_housing_units,poverty_count, poverty_total,
         starts_with("no_insurance_"), starts_with("relevant_pop_"), starts_with("public_only_")) %>%
  # Calculate age groups by sex
  mutate(
    # Male
    male_5_17_2018 = male_5_9 + male_10_14 + male_15_17,
    male_18_39_2018 = male_18_19 + male_20 + male_21 + male_22_24+ male_25_29 + male_30_34 + male_35_39,
    male_40_plus_2018 = male_40_44 + male_45_49 + male_50_54 + male_55_59 + male_60_61 + male_62_64  + male_65_66 + male_67_69+
      male_70_74 + male_75_79 + male_80_84 + male_85_plus,
    # Female
    female_5_17_2018 = female_5_9 + female_10_14 + female_15_17,
    female_18_39_2018 = female_18_19 + female_20 + female_21 + female_22_24+ female_25_29 + female_30_34 + female_35_39,
    female_40_plus_2018 = female_40_44 + female_45_49 + female_50_54 + female_55_59 + female_60_61 + female_62_64  + female_65_66 + female_67_69+
      female_70_74 + female_75_79 + female_80_84 + female_85_plus,
    
    population_5_17_2018 = male_5_17_2018 + female_5_17_2018,
    population_18_39_2018 = male_18_39_2018 + female_18_39_2018,
    population_40_and_over_2018 = male_40_plus_2018 + female_40_plus_2018,
    total_population_ct_2018 = ct_total_population,
    hispanic_ct_2018 = hispanic_ct,
    nhb_ct_2018 = nhb_ct,
    nhw_ct_2018 = nhw_ct,
    median_income_2018 = median_income,
    total_analytical_pop_2018 = population_5_17_2018 +  population_18_39_2018 +
      population_40_and_over_2018,
    total_female_2018 = total_female,
    median_building_age_2018 = median_building_age,
    total_households_2018 = total_households,
    total_housing_units_2018 = total_housing_units,
    total_no_insurance_2018 = rowSums(select(.,  starts_with("no_insurance_")), na.rm = TRUE),
    total_public_only_2018 = rowSums(select(., starts_with("public_only_")), na.rm = TRUE),
    insurance_pop_2018 = rowSums(select(., starts_with("relevant_pop_")), na.rm = TRUE),
    poverty_total_2018 = poverty_total,
    poverty_count_2018 = poverty_count
    
    ) %>%
  select(GEOID, NAME, total_population_ct_2018, hispanic_ct_2018, nhb_ct_2018, 
         total_analytical_pop_2018, total_female_2018, nhw_ct_2018, median_income_2018,
         male_5_17_2018, male_18_39_2018, male_40_plus_2018,
         female_5_17_2018, female_18_39_2018, female_40_plus_2018,
         population_5_17_2018, population_18_39_2018, population_40_and_over_2018,
         median_building_age_2018, total_households_2018, total_housing_units_2018,
         total_no_insurance_2018, total_public_only_2018, insurance_pop_2018,
         poverty_count_2018, poverty_total_2018) %>%
  left_join(cw) %>%
  mutate(across(all_of(additive_vars),
                ~ .x * wt_pop)) %>%
  mutate(total_households_2018 = total_households_2018 * wt_hh,
         total_housing_units_2018 = total_housing_units_2018 * wt_hu
         ) %>%
  
  mutate(median_building_age_2018 = if_else(median_building_age_2018 == 0 | 
                                              median_building_age_2018 == 18 , NA, median_building_age_2018),
         median_income_2018 = if_else(median_income_2018 == 0, NA, median_income_2018),
         total_households_2018_calc = if_else(median_income_2018 == 0 |
                                                is.na(median_income_2018), NA, total_households_2018),
         total_units_2018_calc = if_else(median_building_age_2018 == 0 |
                                                is.na(median_building_age_2018), NA, total_housing_units_2018)) %>%
  group_by(tr2020ge) %>%
  summarize(
    median_income_2018 = sum(median_income_2018 * total_households_2018_calc, na.rm = TRUE)/sum(total_households_2018_calc, na.rm = TRUE),
    median_building_age_2018 = sum(median_building_age_2018 * total_units_2018_calc, na.rm = TRUE)/sum(total_units_2018_calc, na.rm = TRUE),
    across(all_of(additive_vars), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(GEOID = as.character(tr2020ge)) %>%
  filter(GEOID %in% acs_2023$GEOID) 



acs_2023_processed <- acs_2023 %>%
  select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  select(GEOID, NAME, ct_total_population, hispanic_ct, nhb_ct, nhw_ct, median_income,
         male_5_9, male_10_14, male_15_17, median_building_age, total_households, total_housing_units,
         male_18_19, male_20, male_21, male_22_24, male_25_29, male_30_34, male_35_39,
         male_40_44, male_45_49, male_50_54, male_55_59, male_60_61, male_62_64 ,
         male_65_66, male_67_69, male_70_74, male_75_79, male_80_84, male_85_plus,
         total_female, female_5_9, female_10_14, female_15_17,
         female_18_19,  female_20, female_21, female_22_24, female_25_29, female_30_34, female_35_39,
         female_40_44, female_45_49, female_50_54, female_55_59, female_60_61, female_62_64 ,
         female_65_66, female_67_69, female_70_74, female_75_79, female_80_84, female_85_plus,
         poverty_count, poverty_total,
         starts_with("no_insurance_"), starts_with("relevant_pop_"), starts_with("public_only_")) %>%
  # Calculate age groups by sex
  mutate(
    # Male
    male_5_17_2023 = male_5_9 + male_10_14 + male_15_17,
    male_18_39_2023 = male_18_19 + male_20 + male_21 + male_22_24+ male_25_29 + male_30_34 + male_35_39,
    male_40_plus_2023 = male_40_44 + male_45_49 + male_50_54 + male_55_59 + male_60_61 + male_62_64  + male_65_66 + male_67_69+ 
      male_70_74 + male_75_79 + male_80_84 + male_85_plus,
    # Female
    female_5_17_2023 = female_5_9 + female_10_14 + female_15_17,
    female_18_39_2023 = female_18_19 + female_20 + female_21 + female_22_24+ female_25_29 + female_30_34 + female_35_39,
    female_40_plus_2023 = female_40_44 + female_45_49 + female_50_54 + female_55_59 + female_60_61 + female_62_64  + female_65_66 + female_67_69+ 
      female_70_74 + female_75_79 + female_80_84 + female_85_plus,

    hispanic_ct_2023 = hispanic_ct,
    nhb_ct_2023 = nhb_ct,
    nhw_ct_2023 = nhw_ct,
    population_5_17_2023 = male_5_17_2023 + female_5_17_2023,
    population_18_39_2023 = male_18_39_2023 + female_18_39_2023,
    population_40_and_over_2023 = male_40_plus_2023 + female_40_plus_2023,
    total_population_ct_2023 = ct_total_population,
    total_analytical_pop_2023 = population_5_17_2023 +  population_18_39_2023 +
      population_40_and_over_2023, 
    median_income_2023 = median_income,
    total_female_2023 = total_female,
    median_building_age_2023 = median_building_age,
    total_households_2023 = total_households,
    total_housing_units_2023 = total_housing_units,
    total_no_insurance_2023 = rowSums(select(., starts_with("no_insurance_")), na.rm = TRUE),
    total_public_only_2023 = rowSums(select(., starts_with("public_only_")), na.rm = TRUE),
    insurance_pop_2023 = rowSums(select(., starts_with("relevant_pop_")), na.rm = TRUE),
    poverty_count_2023 = poverty_count,
    poverty_total_2023 = poverty_total
    
    ) %>%
  select(GEOID, NAME, total_population_ct_2023, total_analytical_pop_2023, 
         male_5_17_2023, male_18_39_2023, male_40_plus_2023, median_income_2023,
         total_female_2023, female_5_17_2023, female_18_39_2023, female_40_plus_2023,
         population_5_17_2023, population_18_39_2023, population_40_and_over_2023, 
         hispanic_ct_2023, nhb_ct_2023, nhw_ct_2023, median_building_age_2023,
         total_households_2023, total_housing_units_2023,total_no_insurance_2023,
          total_public_only_2023, insurance_pop_2023, poverty_count_2023, poverty_total_2023) 

data <- full_join(acs_2018_processed, acs_2023_processed) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
  mutate(median_income_2018 = if_else(median_income_2018 == 0, NA, median_income_2018)) %>%
  mutate(prop_female_2023 = total_female_2023/total_population_ct_2023,
         pop_young_old_2023 = 1-(population_18_39_2023/total_population_ct_2023)) %>%
  mutate(prop_female_2018 = total_female_2018/total_population_ct_2018,
         pop_young_old_2018 = 1-(population_18_39_2018/total_population_ct_2018)) %>%
  mutate(
    # 2018
    pct_uninsured_2018 = total_no_insurance_2018 / insurance_pop_2018,
    pct_public_only_2018 = total_public_only_2018 / insurance_pop_2018,
    pct_uninsured_or_public_2018 = (total_no_insurance_2018 + total_public_only_2018) / insurance_pop_2018,
    pct_poverty_2018 = (poverty_count_2018/ poverty_total_2018),
    # 2023
    pct_uninsured_2023 = total_no_insurance_2023 / insurance_pop_2023,
    pct_public_only_2023 = total_public_only_2023 / insurance_pop_2023,
    pct_uninsured_or_public_2023 = (total_no_insurance_2023 + total_public_only_2023) / insurance_pop_2023,
    pct_poverty_2023 = (poverty_count_2023/ poverty_total_2023),
    
    
  )

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/census-data")
write.fst(data, "census_data.fst")



a <- data %>%
  select(GEOID, median_income_2018, median_income_2023) %>%
  mutate(sum_diff = median_income_2023 - median_income_2018)


b <- data %>%
  select(GEOID, median_building_age_2018, median_building_age_2023) %>%
  mutate(sum_diff = median_building_age_2023 - median_building_age_2018)

