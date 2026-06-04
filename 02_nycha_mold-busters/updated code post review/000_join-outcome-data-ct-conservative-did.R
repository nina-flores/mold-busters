library(ggplot2)
library(patchwork)
library(dplyr)
library(paletteer)
library(here)
library(fst)
library(tidyverse)
library(igraph)

setwd(here("data"))

sparcs_analytical <- read.fst("processed_data.fst") 

s <- sparcs_analytical %>%
  filter(nycha == 1) %>%
  filter(analytical_age_group == "all") %>%
  select(tds_or_ct, building_number) %>%
  unique()
# 1968 buildings

sum(s$total_ed)


covariate_mold <- read.fst("covariate_mold_data.fst") %>%
  mutate(building_age = age_of_the_building_as_of_10_1_2023 - (2023 - year)) %>%
  group_by(tds_number, building_number) %>%
  mutate(median_household_income = if_else(is.na(median_household_income), 
                                           mean(median_household_income, na.rm = T), 
                                           median_household_income)) %>%
  ungroup() %>%
  group_by(tds_number) %>%
  mutate(median_household_income = if_else(is.na(median_household_income), 
                                           mean(median_household_income, na.rm = T), 
                                           median_household_income)) %>%
  ungroup()

sum(covariate_mold$total_reports)
#87146


covariate <- read.fst("covariate_data.fst") %>%
  mutate(building_age = if_else(group == "nycha", 
                                age_of_the_building_as_of_10_1_2023 - (2023 - year),
                                year - cnstrct_yr)) %>%
  mutate(senior = if_else(senior_development_flag == "EXCLUSIVELY",1,0),
         senior = if_else(is.na(senior),0,senior))

names(covariate)

covariate_total_pop <- covariate %>%
  filter(group == "nycha") %>%
  filter(year == "2019") %>%
  summarize(tot = sum(analyticial_total_population),
            )


# checks out, 333409
a <- covariate %>%
  filter(group != "nycha") %>%
  select(geoid, nycha_neighb_250,nycha_neighb_500, nycha_neighb_100 ) %>%
  unique()
sum(a$nycha_neighb_250)
sum(a$nycha_neighb_500)
sum(a$nycha_neighb_100)

b <- covariate %>%
  filter(group == "nycha") %>%
  select(tds_number,class_letter ) %>%
  unique()



# for mold analyses -------------------------------------------------------
sparcs_mold <- sparcs_analytical %>%
  filter(nycha == 1) %>%
  rename(tds_number = tds_or_ct)

dat_mold <- covariate_mold %>%
  left_join(sparcs_mold) %>%
  unique() %>%
  drop_na(analytical_age_group) %>%
  mutate(prop_female = female_population/total_population,
         pop_young_old = 1-(population_18_39/total_population)) %>%
  mutate(denominator_age = case_when(analytical_age_group == "all" ~ analyticial_total_population,
                                     analytical_age_group == "5-17" ~ population_5_17,
                                     analytical_age_group == "18-39" ~ population_18_39,
                                     analytical_age_group == "40+" ~ population_40_and_over),
         
         denominator_sex = case_when(gender == "F" ~ prop_female*analyticial_total_population,
                                     gender == "M" ~ (1-prop_female)*analyticial_total_population),
         
         denominator_age_sex =  case_when(analytical_age_group == "all" & gender == "F" ~ prop_female*analyticial_total_population,
                                          analytical_age_group == "5-17" & gender == "F" ~ prop_female*population_5_17,
                                          analytical_age_group == "18-39" & gender == "F" ~ prop_female*population_18_39,
                                          analytical_age_group == "40+" & gender == "F" ~ prop_female*population_40_and_over,
                                          analytical_age_group == "all" & gender == "M" ~ (1-prop_female)*analyticial_total_population,
                                          analytical_age_group == "5-17" & gender == "M" ~ (1-prop_female)*population_5_17,
                                          analytical_age_group == "18-39" & gender == "M" ~ (1-prop_female)*population_18_39,
                                          analytical_age_group == "40+" & gender == "M" ~ (1-prop_female)*population_40_and_over)) %>%
  mutate(denominator_sex = round(denominator_sex, 0),
         denominator_age_sex = round(denominator_age_sex))

setwd(here("data", "mold-analytical-datasets"))
# age
summary_age <- dat_mold %>%
  group_by(tds_number, building_number, consolidated_tds_number, year, building_age,
           denominator_age, median_household_income, prop_female, pop_young_old,
           total_households, ppt, tdmean, annual_pm, ndvi, age_of_the_building_as_of_10_1_2023,
           building_sq_footage_by_building, residential_buildings, geographical_borough,
           total_reports, total_severe_weather, total_repeats_6,
           analytical_age_group, lon, lat) %>%
  summarize(total_ed = sum(total_ed),
            total_inpat = sum(total_inpat) ) %>%
  mutate(id = paste0(tds_number, "_", building_number)) %>%
  group_by(consolidated_tds_number) %>%
  mutate(con_lon = first(lon),
         con_lat = first(lat)) %>%
  ungroup()

write.csv(summary_age, paste0("data_agegroup.csv"), row.names= FALSE)
    

# sex
summary_sex <- dat_mold %>%
  group_by(tds_number, building_number, consolidated_tds_number, year, 
           denominator_sex, median_household_income, prop_female, pop_young_old, building_age,
           total_households, ppt, tdmean, annual_pm, ndvi, age_of_the_building_as_of_10_1_2023,
           building_sq_footage_by_building, residential_buildings, geographical_borough,
           total_reports, total_severe_weather, total_repeats_6,
           gender, lon, lat) %>%
  summarize(total_ed = sum(total_ed),
            total_inpat = sum(total_inpat))  %>%
  filter(gender != "U") %>%
  group_by(consolidated_tds_number) %>%
  mutate(id = paste0(tds_number, "_", building_number)) %>%
  mutate(con_lon = first(lon),
         con_lat = first(lat)) %>%
  ungroup()


write.csv(summary_sex, paste0("data_sex.csv"), row.names= FALSE)

# age and sex
summary_agesex <- dat_mold %>%
  group_by(tds_number, building_number, consolidated_tds_number, year, 
           denominator_age_sex, median_household_income, prop_female, pop_young_old, building_age,
           total_households, ppt, tdmean, annual_pm, ndvi, age_of_the_building_as_of_10_1_2023,
           building_sq_footage_by_building, residential_buildings, geographical_borough,
           total_reports, total_severe_weather, total_repeats_6,
           gender, analytical_age_group, lon, lat) %>%
  summarize(total_ed = sum(total_ed),
            total_inpat = sum(total_inpat)
            ) %>%
  filter(gender != "U") %>%
  mutate(id = paste0(tds_number, "_", building_number)) %>%
  group_by(consolidated_tds_number) %>%
  mutate(con_lon = first(lon),
         con_lat = first(lat)) %>%
  ungroup()

write.csv(summary_agesex, paste0("data_agesex.csv"), row.names= FALSE)



# for did analyses -------------------------------------------------------

sparcs_nycha <- sparcs_analytical %>%
  filter(nycha == 1) %>%
  rename(tds_number = tds_or_ct)

nycha_ids_cts <- covariate %>%
  left_join(sparcs_nycha) %>%
  filter(nycha == 1) %>%
  select(consolidated_tds_number, geoid) %>%
  unique() %>%
  graph_from_data_frame(directed = FALSE) %>%
  components()

nycha_ids_cts <- nycha_ids_cts$membership %>%
  stack() %>%
  transmute(consolidated_tds_number = as.character(ind), geo_group = values) %>%
  filter(nchar(consolidated_tds_number) <11) %>%
  mutate(consolidated_tds_number = as.numeric(consolidated_tds_number))


# set up nycha data first 
sparcs_nycha_cov <- covariate %>%
  left_join(sparcs_nycha) %>%
  left_join(nycha_ids_cts) %>%
  filter(group != "controls") %>%
  unique() %>%
  ungroup() %>%
  rename("analytical_total_population" = "analyticial_total_population") %>%
  drop_na(analytical_age_group) %>%
  mutate(prop_female = female_population/total_population,
         pop_young_old = 1-(population_18_39 /total_population),
         prop_public_insurance = if_else(year <= 2018, pct_public_only_2018, pct_public_only_2023),
         
         pop_young_old2 = if_else(year <= 2018, pop_young_old_2018, pop_young_old_2023),
         
         prop_uninsured = if_else(year <= 2018, pct_uninsured_2018, pct_uninsured_2023),
         prop_underinsured = if_else(year <= 2018, pct_uninsured_or_public_2018, pct_uninsured_or_public_2023),
         prop_poverty = if_else(year <= 2018, pct_poverty_2018, pct_poverty_2023)) %>%

  mutate(denominator_age = case_when(analytical_age_group == "all" ~ analytical_total_population,
                                     analytical_age_group == "5-17" ~ population_5_17,
                                     analytical_age_group == "18-39" ~ population_18_39,
                                     analytical_age_group == "40+" ~ population_40_and_over),
         
         denominator_sex = case_when(gender == "F" ~ prop_female*analytical_total_population,
                                     gender == "M" ~ (1-prop_female)*analytical_total_population),
         
         denominator_age_sex =  case_when(analytical_age_group == "all" & gender == "F" ~ prop_female*analytical_total_population,
                                          analytical_age_group == "5-17" & gender == "F" ~ prop_female*population_5_17,
                                          analytical_age_group == "18-39" & gender == "F" ~ prop_female*population_18_39,
                                          analytical_age_group == "40+" & gender == "F" ~ prop_female*population_40_and_over,
                                          analytical_age_group == "all" & gender == "M" ~ (1-prop_female)*analytical_total_population,
                                          analytical_age_group == "5-17" & gender == "M" ~ (1-prop_female)*population_5_17,
                                          analytical_age_group == "18-39" & gender == "M" ~ (1-prop_female)*population_18_39,
                                          analytical_age_group == "40+" & gender == "M" ~ (1-prop_female)*population_40_and_over)) %>%
  mutate(denominator_sex = round(denominator_sex, 0),
         denominator_age_sex = round(denominator_age_sex))  %>%
  unique()


# age
summary_age_nycha <- sparcs_nycha_cov %>%
  group_by(geo_group, year, analytical_age_group) %>%
  summarize(total_ed = sum(total_ed),
            total_inpat = sum(total_inpat),
            total_ed_non_asthma = sum(total_ed_non_asthma),
            total_inpat_non_asthma = sum(total_inpat_non_asthma),
            summer_ed = sum(summer_ed),
            winter_ed = sum(winter_ed),
            spring_ed = sum(spring_ed),
            senior = min(senior),
            fall_ed = sum(fall_ed),
            summer_ed2 = sum(summer_ed2),
            winter_ed2 = sum(winter_ed2),
            spring_ed2 = sum(spring_ed2),
            fall_ed2 = sum(fall_ed2),
            deathrate = mean(deathrate, na.rm = T),
            heightroof = mean(heightroof_control, na.rm = T),
            denominator_age = sum(denominator_age_sex, na.rm = T),
            median_household_income = mean(median_household_income, na.rm = T),
            building_age = mean(building_age, na.rm = T),
            prop_female = mean(prop_female),
            pop_young_old = mean(pop_young_old),
            pop_young_old2 = mean(pop_young_old2),
            prop_public_insurance = mean(prop_public_insurance),
            prop_uninsured = mean(prop_uninsured),
            prop_underinsured = mean(prop_underinsured),
            prop_poverty = mean(prop_poverty),
            ppt = mean(ppt),
            tdmean = mean(tdmean),
            tmax_max = mean(tmax_max),
            tmin_min = mean(tmin_min),
            annual_pm = mean(annual_pm),
            annual_o3 = mean(annual_o3),
            annual_bc = mean(annual_bc),
            annual_no2 = mean(annual_no2),
            mean_aadt = mean(mean_aadt),
            ndvi = mean(ndvi), 
            lon = first(lon),
            lat = first(lat),
            nycha_neighb_1000 = max(nycha_neighb_1000),
            nycha_neighb_500 = max(nycha_neighb_500),
            nycha_neighb_250 = max(nycha_neighb_250),
            nycha_neighb_100 = max(nycha_neighb_100),
            geoid = as.character(first(geoid))) %>%
  mutate(group = "nycha") 


# sex and age # dont need to compile sex alone because it ends up in this 
# dataset under the all category
summary_agesex_nycha <- sparcs_nycha_cov %>%
  group_by(geo_group, year, 
           gender, analytical_age_group) %>%
  summarize(total_ed = sum(total_ed),
            senior = min(senior),
            total_inpat = sum(total_inpat),
            total_ed_non_asthma = sum(total_ed_non_asthma),
            total_inpat_non_asthma = sum(total_inpat_non_asthma),
            summer_ed = sum(summer_ed),
            winter_ed = sum(winter_ed),
            spring_ed = sum(spring_ed),
            fall_ed = sum(fall_ed),
            summer_ed2 = sum(summer_ed2),
            winter_ed2 = sum(winter_ed2),
            spring_ed2 = sum(spring_ed2),
            fall_ed2 = sum(fall_ed2),
            deathrate = mean(deathrate, na.rm = T),
            heightroof = mean(heightroof_control, na.rm = T),
            denominator_age_sex = sum(denominator_age_sex, na.rm = T),
            median_household_income = mean(median_household_income, na.rm = T),
            building_age = mean(building_age, na.rm = T),
            prop_female = mean(prop_female),
            pop_young_old = mean(pop_young_old),
            pop_young_old2 = mean(pop_young_old2),
            prop_public_insurance = mean(prop_public_insurance),
            prop_uninsured = mean(prop_uninsured),
            prop_underinsured = mean(prop_underinsured),
            prop_poverty = mean(prop_poverty),
            ppt = mean(ppt),
            tdmean = mean(tdmean),
            tmax_max = mean(tmax_max),
            tmin_min = mean(tmin_min),
            annual_pm = mean(annual_pm),
            annual_o3 = mean(annual_o3),
            annual_bc = mean(annual_bc),
            annual_no2 = mean(annual_no2),
            mean_aadt = mean(mean_aadt),
            ndvi = mean(ndvi),
            nycha_neighb_1000 = max(nycha_neighb_1000),
            nycha_neighb_500 = max(nycha_neighb_500),
            nycha_neighb_250 = max(nycha_neighb_250),
            nycha_neighb_100 = max(nycha_neighb_100),
            lon = first(lon),
            lat = first(lat),
            geoid = as.character(first(geoid))) %>%
  filter(gender != "U") %>%
  mutate(group = "nycha")

# lcga group
summary_lcga_nycha <- sparcs_nycha_cov %>%
  filter(analytical_age_group == "all") %>%
  group_by(geo_group, year, 
           class5, class_letter, analytical_age_group) %>%
  summarize(total_ed = sum(total_ed),
            total_inpat = sum(total_inpat),
            total_ed_non_asthma = sum(total_ed_non_asthma),
            total_inpat_non_asthma = sum(total_inpat_non_asthma),
            summer_ed = sum(summer_ed),
            winter_ed = sum(winter_ed),
            senior = min(senior),
            spring_ed = sum(spring_ed),
            fall_ed = sum(fall_ed),
            summer_ed2 = sum(summer_ed2),
            winter_ed2 = sum(winter_ed2),
            spring_ed2 = sum(spring_ed2),
            fall_ed2 = sum(fall_ed2),
            deathrate = mean(deathrate, na.rm = T),
            heightroof = mean(heightroof_control, na.rm = T),
            denominator = sum(denominator_age_sex, na.rm = T),
            median_household_income = mean(median_household_income, na.rm = T),
            building_age = mean(building_age, na.rm = T),
            prop_female = mean(prop_female),
            pop_young_old = mean(pop_young_old),
            pop_young_old2 = mean(pop_young_old2),
            prop_public_insurance = mean(prop_public_insurance),
            prop_uninsured = mean(prop_uninsured),
            prop_underinsured = mean(prop_underinsured),
            prop_poverty = mean(prop_poverty),
            ppt = mean(ppt),
            tdmean = mean(tdmean),
            tmax_max = mean(tmax_max),
            tmin_min = mean(tmin_min),
            annual_pm = mean(annual_pm),
            annual_o3 = mean(annual_o3),
            annual_bc = mean(annual_bc),
            annual_no2 = mean(annual_no2),
            mean_aadt = mean(mean_aadt),
            ndvi = mean(ndvi),
            nycha_neighb_1000 = max(nycha_neighb_1000),
            nycha_neighb_500 = max(nycha_neighb_500),
            nycha_neighb_250 = max(nycha_neighb_250),
            nycha_neighb_100 = max(nycha_neighb_100),
            lon = first(lon),
            lat = first(lat),
            geoid = as.character(first(geoid))) %>%
  mutate(group = "nycha") 


# now  set up the control information

sparcs_control <- sparcs_analytical %>%
  filter(nycha == 0) %>%
  rename(geoid = tds_or_ct) 

sparcs_control_cov <- covariate %>%
  mutate(geoid = as.numeric(geoid)) %>%
  left_join(sparcs_control) %>%
  rename("analytical_total_population" = "analyticial_total_population") %>%
  filter(group == "controls") %>%
  unique() %>%
  drop_na(analytical_age_group) %>%
  mutate(population_5_17 = if_else(year <= 2018, population_5_17_2018, population_5_17_2023),
         population_18_39 = if_else(year <= 2018, population_18_39_2018, population_18_39_2023),
         population_40_and_over = if_else(year <= 2018, population_40_and_over_2018, population_40_and_over_2023),
         female_population = if_else(year <= 2018, total_female_2018, total_female_2023),
         total_population = if_else(year <= 2018, total_population_ct_2018, total_population_ct_2023),
         analytical_total_population = if_else(year <= 2018, total_analytical_pop_2018, total_analytical_pop_2023),
         median_household_income = if_else(year <= 2018, median_income_2018, median_income_2023),
         prop_public_insurance = if_else(year <= 2018, pct_public_only_2018, pct_public_only_2023),
         prop_uninsured = if_else(year <= 2018, pct_uninsured_2018, pct_uninsured_2023),
         prop_underinsured = if_else(year <= 2018, pct_uninsured_or_public_2018, pct_uninsured_or_public_2023),
         prop_poverty = if_else(year <= 2018, pct_poverty_2018, pct_poverty_2023),
         median_household_income = if_else(is.na(median_household_income), mean(median_income_2018, na.rm = T), median_household_income),
         building_age = year - cnstrct_yr,
         geo_group = geoid,
         prop_female = if_else(year <= 2018, prop_female_2018, prop_female_2023),
         pop_young_old = if_else(year <= 2018, pop_young_old_2018, pop_young_old_2023),
         pop_young_old2 = if_else(year <= 2018, pop_young_old_2018, pop_young_old_2023)
         
         
         ) %>%
  mutate(denominator_age = case_when(analytical_age_group == "all" ~ analytical_total_population,
                                     analytical_age_group == "5-17" ~ population_5_17,
                                     analytical_age_group == "18-39" ~ population_18_39,
                                     analytical_age_group == "40+" ~ population_40_and_over),
         
         denominator_sex = case_when(gender == "F" & year <= 2018 ~ female_5_17_2018 + female_18_39_2018 + female_40_plus_2018,
                                     gender == "F" & year > 2018 ~ female_5_17_2023 + female_18_39_2023 + female_40_plus_2023,
                                     gender == "M" & year <= 2018 ~ male_5_17_2018 + male_18_39_2018 + male_40_plus_2018,
                                     gender == "M" & year > 2018 ~ male_5_17_2023 + male_18_39_2023 + male_40_plus_2023), 
                                     
         denominator_age_sex =  case_when(analytical_age_group == "all" & gender == "F" & year <= 2018 ~ female_5_17_2018 + female_18_39_2018 + female_40_plus_2018,
                                          analytical_age_group == "all" & gender == "F" & year > 2018 ~ female_5_17_2023 + female_18_39_2023 + female_40_plus_2023,
                                          analytical_age_group == "5-17" & gender == "F" & year <= 2018 ~ female_5_17_2018,
                                          analytical_age_group == "5-17" & gender == "F" & year > 2018 ~ female_5_17_2023,
                                          analytical_age_group == "18-39" & gender == "F" & year <= 2018 ~ female_18_39_2018,
                                          analytical_age_group == "18-39" & gender == "F" & year > 2018 ~ female_18_39_2023,
                                          analytical_age_group == "40+" & gender == "F" & year <= 2018 ~ female_40_plus_2018,
                                          analytical_age_group == "40+" & gender == "F" & year > 2018 ~ female_40_plus_2023,
                                          
                                          
                                          analytical_age_group == "all" & gender == "M" & year <= 2018 ~ male_5_17_2018 + male_18_39_2018 + male_40_plus_2018,
                                          analytical_age_group == "all" & gender == "M" & year > 2018 ~ male_5_17_2023 + male_18_39_2023 + male_40_plus_2023,
                                          analytical_age_group == "5-17" & gender == "M" & year <= 2018 ~ male_5_17_2018,
                                          analytical_age_group == "5-17" & gender == "M" & year > 2018 ~ male_5_17_2023,
                                          analytical_age_group == "18-39" & gender == "M" & year <= 2018 ~ male_18_39_2018,
                                          analytical_age_group == "18-39" & gender == "M" & year > 2018 ~ male_18_39_2023,
                                          analytical_age_group == "40+" & gender == "M" & year <= 2018 ~ male_40_plus_2018,
                                          analytical_age_group == "40+" & gender == "M" & year > 2018 ~ male_40_plus_2023)) %>%
  mutate(denominator_sex = round(denominator_sex, 0),
         denominator_age_sex = round(denominator_age_sex)) 

# before any aggregations - make sure we are aggregated to the geoid level to do
# this and account for the fact that there might be multiple geoids for certain 
# consolidations and only a few might match, we use igraph

sparcs_control_cov %>%
  select(consolidated_tds_number) %>%
  n_distinct()

sparcs_control_cov %>%
  select(consolidated_tds_number) %>%
  n_distinct()
        

# age
summary_age_control <- sparcs_control_cov %>%
  group_by(geo_group, year, 
           analytical_age_group) %>%
  summarize(total_ed = sum(total_ed),
            total_inpat = sum(total_inpat),
            total_ed_non_asthma = sum(total_ed_non_asthma),
            total_inpat_non_asthma = sum(total_inpat_non_asthma),
            summer_ed = sum(summer_ed),
            winter_ed = sum(winter_ed),
            spring_ed = sum(spring_ed),
            fall_ed = sum(fall_ed),
            summer_ed2 = sum(summer_ed2),
            winter_ed2 = sum(winter_ed2),
            spring_ed2 = sum(spring_ed2),
            fall_ed2 = sum(fall_ed2),
            deathrate = mean(deathrate, na.rm = T),
            heightroof = mean(heightroof_control, na.rm = T),
            denominator_age = sum(denominator_age_sex, na.rm = T),
            median_household_income = mean(median_household_income, na.rm = T),
            building_age = mean(building_age, na.rm = T),
            prop_female = mean(prop_female),
            pop_young_old = mean(pop_young_old),
            pop_young_old2 = mean(pop_young_old2),
            senior = min(senior),
            
            prop_public_insurance = mean(prop_public_insurance),
            prop_uninsured = mean(prop_uninsured),
            prop_underinsured = mean(prop_underinsured),
            prop_poverty = mean(prop_poverty),
            ppt = mean(ppt),
            tdmean = mean(tdmean),
            tmax_max = mean(tmax_max),
            tmin_min = mean(tmin_min),
            annual_pm = mean(annual_pm),
            annual_o3 = mean(annual_o3),
            annual_bc = mean(annual_bc),
            annual_no2 = mean(annual_no2),
            mean_aadt = mean(mean_aadt),
            ndvi = mean(ndvi),
            lon = first(lon),
            lat = first(lat),
            nycha_neighb_1000 = max(nycha_neighb_1000),
            nycha_neighb_500 = max(nycha_neighb_500),
            nycha_neighb_250 = max(nycha_neighb_250),
            nycha_neighb_100 = max(nycha_neighb_100),
            geoid = as.character(first(geoid))) %>%
  mutate(group = "control")


# sex and age # dont need to compile sex alone because it ends up in this 
# dataset under the all category
summary_agesex_control <- sparcs_control_cov %>%
  group_by(geo_group, year, 
           gender, analytical_age_group) %>%
  summarize(total_ed = sum(total_ed),
            total_inpat = sum(total_inpat),
            total_ed_non_asthma = sum(total_ed_non_asthma),
            total_inpat_non_asthma = sum(total_inpat_non_asthma),
            summer_ed = sum(summer_ed),
            winter_ed = sum(winter_ed),
            spring_ed = sum(spring_ed),
            fall_ed = sum(fall_ed),
            summer_ed2 = sum(summer_ed2),
            winter_ed2 = sum(winter_ed2),
            spring_ed2 = sum(spring_ed2),
            fall_ed2 = sum(fall_ed2),
            deathrate = mean(deathrate, na.rm = T),
            heightroof = mean(heightroof_control, na.rm = T),
            denominator_age_sex = sum(denominator_age_sex),
            median_household_income = mean(median_household_income, na.rm = T),
            building_age = mean(building_age, na.rm = T),
            prop_female = mean(prop_female),
            pop_young_old = mean(pop_young_old),
            pop_young_old2 = mean(pop_young_old2),
            senior = min(senior),
            
            prop_public_insurance = mean(prop_public_insurance),
            prop_uninsured = mean(prop_uninsured),
            prop_underinsured = mean(prop_underinsured),
            prop_poverty = mean(prop_poverty),
            ppt = mean(ppt),
            tdmean = mean(tdmean),
            tmax_max = mean(tmax_max),
            tmin_min = mean(tmin_min),
            annual_pm = mean(annual_pm),
            annual_o3 = mean(annual_o3),
            annual_bc = mean(annual_bc),
            annual_no2 = mean(annual_no2),
            mean_aadt = mean(mean_aadt),
            ndvi = mean(ndvi),
            nycha_neighb_1000 = max(nycha_neighb_1000),
            nycha_neighb_500 = max(nycha_neighb_500),
            nycha_neighb_250 = max(nycha_neighb_250),
            nycha_neighb_100 = max(nycha_neighb_100),
            lon = first(lon),
            lat = first(lat),
            geoid = as.character(first(geoid))) %>%
  filter(gender != "U") %>%
  mutate(group = "control")

# lcga group
summary_lcga_control <- sparcs_control_cov %>%
  group_by(geo_group, year, 
           class5, class_letter, analytical_age_group) %>%
  summarize(total_ed = sum(total_ed),
            total_inpat = sum(total_inpat),
            total_ed_non_asthma = sum(total_ed_non_asthma),
            total_inpat_non_asthma = sum(total_inpat_non_asthma),
            summer_ed = sum(summer_ed),
            winter_ed = sum(winter_ed),
            spring_ed = sum(spring_ed),
            fall_ed = sum(fall_ed),
            summer_ed2 = sum(summer_ed2),
            winter_ed2 = sum(winter_ed2),
            spring_ed2 = sum(spring_ed2),
            fall_ed2 = sum(fall_ed2),
            deathrate = mean(deathrate, na.rm = T),
            heightroof = mean(heightroof_control, na.rm = T),
            denominator = sum(denominator_age_sex, na.rm = T),
            median_household_income = mean(median_household_income, na.rm = T),
            building_age = mean(building_age, na.rm = T),
            prop_female = mean(prop_female),
            pop_young_old = mean(pop_young_old),
            pop_young_old2 = mean(pop_young_old2),
            senior = min(senior),
            
            prop_public_insurance = mean(prop_public_insurance),
            prop_uninsured = mean(prop_uninsured),
            prop_underinsured = mean(prop_underinsured),
            prop_poverty = mean(prop_poverty),
            ppt = mean(ppt),
            tdmean = mean(tdmean),
            tmax_max = mean(tmax_max),
            tmin_min = mean(tmin_min),
            annual_pm = mean(annual_pm),
            annual_o3 = mean(annual_o3),
            annual_bc = mean(annual_bc),
            annual_no2 = mean(annual_no2),
            mean_aadt = mean(mean_aadt),
            ndvi = mean(ndvi),
            nycha_neighb_1000 = max(nycha_neighb_1000),
            nycha_neighb_500 = max(nycha_neighb_500),
            nycha_neighb_250 = max(nycha_neighb_250),
            nycha_neighb_100 = max(nycha_neighb_100),
            lon = first(lon),
            lat = first(lat),
            geoid = as.character(first(geoid))) %>%
  filter(analytical_age_group == "all") %>%
  mutate(group = "control")


# bind nycha and control datasets before saving out -----------------------
setwd(here("data", "did-analytical-datasets"))

# create quartiles for variables that are harder to match to controls 
did_age <- rbind(summary_age_nycha, summary_age_control) %>%
  ungroup() %>%
  mutate(across(all_of(c("ndvi")),  #"prop_female", "median_household_income"
                ~ cut(.,
                      breaks = quantile(., probs = c(0,0.25, .5, 0.75, 1), na.rm = TRUE),
                      include.lowest = TRUE,
                      labels = c("q1", "q2", "q3", "q4")),
                .names = "{.col}_quartile")) %>%
  mutate(deathrate_quartile = as.character(ntile(deathrate,4)))


write.csv(did_age, paste0("data_agegroup_ct.csv"), row.names= FALSE)

did_agesex <- rbind(summary_agesex_nycha, summary_agesex_control)
write.csv(did_agesex, paste0("data_agesexgroup_ct.csv"), row.names= FALSE)

did_lcga <- rbind(summary_lcga_nycha, summary_lcga_control)
write.csv(did_lcga, paste0("data_lcgagroup_ct.csv"), row.names= FALSE)

#n_distinct(did_age$geo_group)


