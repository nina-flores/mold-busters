library(ggplot2)
library(patchwork)
library(dplyr)
library(paletteer)
library(here)
library(fst)
library(tidyverse)
library(DHARMa)
library(glmmTMB)
library(performance)
library(splines)

setwd(here("data", "mold-analytical-datasets"))

data <- read.csv("data_agegroup.csv") %>%
  mutate(tds_number = as.factor(tds_number),
         consolidated_tds_number = as.factor(consolidated_tds_number),
         year = as.factor(year),
         across(all_of(c("tdmean", "ppt", "annual_pm", "ndvi", 
                         "median_household_income",
                         "building_age", "building_sq_footage_by_building",
                         "pop_young_old", "prop_female")), scale)) %>%
  mutate(denominator = denominator_age) %>%
  mutate(total_reports = (total_reports / total_households)*100,
         total_repeats_6 = (total_repeats_6/ total_households) * 100,
         total_severe_weather = (total_severe_weather / total_households) * 100)

a <- data %>%
  filter(analytical_age_group == "all")
sum(a$total_reports) # same when you haven't already converted it to a rate


data_s <- read.csv("data_agesex.csv") %>%
  mutate(tds_number = as.factor(tds_number),
         consolidated_tds_number = as.factor(consolidated_tds_number),
         year = as.factor(year),
         across(all_of(c("tdmean", "ppt", "annual_pm", "ndvi", 
                         "median_household_income",
                         "building_age", "building_sq_footage_by_building",
                         "pop_young_old", "prop_female")), scale)) %>%
  mutate(denominator = denominator_age_sex) %>%
  mutate(total_reports = (total_reports / total_households)*100,
         total_repeats_6 = (total_repeats_6/ total_households) * 100,
         total_severe_weather = (total_severe_weather / total_households) * 100)



data_overall <- data %>%
  filter(analytical_age_group == "all") 


summary(data_overall$total_reports)

data_age <- data %>%
  filter(analytical_age_group != "all") 

data_sex <- data_s %>%
  filter(analytical_age_group == "all") 

data_agesex <- data_s %>%
  filter(analytical_age_group != "all") 

table(data_overall$total_visits)



compute_results_with_plot <- function(outcome, data, exposures, var, group_vars) {
  results <- data.frame(
    group = character(),
    exposure = character(),
    estimate = numeric(),
    conf.low = numeric(),
    conf.high = numeric(),
    stringsAsFactors = FALSE)
  
  # Create a group ID from grouping variables
  data$group_id <- interaction(data[, group_vars], drop = TRUE, sep = "_")
  
  # Split by group
  grouped_data <- split(data, data$group_id)
  
  for (group_name in names(grouped_data)) {
    group_df <- grouped_data[[group_name]] %>%
      filter(denominator != 0)
    
    for (exposure in exposures) {
      temp_df <- group_df
      
      # If the exposure is total_repeats_6, filter to 2017–2018 only
      if (exposure == "total_repeats_6") {
        temp_df <- temp_df %>% filter(year %in% c(2017, 2018))
      }
      
      formula <- as.formula(paste(
        outcome, "~", exposure, "+", paste(var, collapse = " + "),
        "+ ns(scale(lat), 12) + ns(scale(lon), 12)",
        "+ (1 | id) + (1 | tds_number) + (1 | consolidated_tds_number)"
      ))
      
      model <- glmmTMB(formula, data = temp_df, family = nbinom1(), offset = log(denominator/100))
      
      tidy_model <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
        filter(term == exposure) %>%
        mutate(group = group_name, exposure = exposure)
      
      results <- bind_rows(results, tidy_model)
    }
  }
  
  return(results)
}


results_overall <- compute_results_with_plot(outcome = "total_visits",
                                             data = data_overall,
                                             exposures = c("total_reports",
                                                           "total_repeats_6"),
                                             var = c("tdmean", 
                                                     "ppt",
                                                     "annual_pm",
                                                     "ndvi",
                                                     "median_household_income",
                                                     "building_age",
                                                     "building_sq_footage_by_building",
                                                     "residential_buildings",
                                                     "pop_young_old",
                                                     "prop_female"),
                                             group_vars = c("analytical_age_group"))

results_ages <- compute_results_with_plot(outcome = "total_visits",
                                             data = data_age,
                                             exposures = c("total_reports",
                                                           "total_repeats_6"),
                                             var = c("tdmean", 
                                                     "ppt",
                                                     "annual_pm",
                                                     "ndvi",
                                                     "median_household_income",
                                                     "building_age",
                                                     "building_sq_footage_by_building",
                                                     "residential_buildings",
                                                     "prop_female"),
                                             group_vars = c("analytical_age_group"))


results_sex <- compute_results_with_plot(outcome = "total_visits",
                                          data = data_sex,
                                          exposures = c("total_reports",
                                                        "total_repeats_6"),
                                          var = c("tdmean", 
                                                  "ppt",
                                                  "annual_pm",
                                                  "ndvi",
                                                  "median_household_income",
                                                  "building_age",
                                                  "building_sq_footage_by_building",
                                                  "residential_buildings",
                                                  "pop_young_old"),
                                          group_vars = c("gender"))

results_age_sex <- compute_results_with_plot(outcome = "total_visits",
                                             data = data_agesex,
                                             exposures = c("total_reports",
                                                           "total_repeats_6",
                                                           "total_severe_weather"),
                                             var = c("tdmean", 
                                                     "ppt",
                                                     "annual_pm",
                                                     "ndvi",
                                                     "median_household_income",
                                                     "building_age",
                                                     "building_sq_footage_by_building",
                                                     "residential_buildings"),
                                             group_vars = c("analytical_age_group", "gender"))



data_results <- rbind(results_overall,
                      results_ages,
                      results_sex,
                      results_age_sex)
                      
setwd(here("data", "results-datasets-mold"))

write.csv(data_results, "results.csv", row.names = FALSE)


