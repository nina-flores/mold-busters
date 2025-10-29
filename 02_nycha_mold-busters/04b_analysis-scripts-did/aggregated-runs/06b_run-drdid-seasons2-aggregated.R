library(ggplot2)
library(patchwork)
library(dplyr)
library(paletteer)
library(here)
library(fst)
library(tidyverse)
library(DRDID)
library(did)


setwd(here("data", "did-analytical-datasets"))

data <- read.csv("data_agegroup.csv") %>%
  mutate(denominator = denominator_age) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population = 1000 *(total_ed / denominator),
         spring_ed2 = 1000 *(spring_ed2 / denominator),
         summer_ed2 = 1000 *(summer_ed2 / denominator),
         fall_ed2 = 1000 *(fall_ed2 / denominator),
         winter_ed2 = 1000 *(winter_ed2 / denominator),
         visits_non_asthma_per_pop = 1000 *(total_ed_non_asthma / denominator)) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0))%>%
  filter(!year %in%   c(2019, 2020)) %>%
  filter(  nycha_neighb_250 == 1) %>%
  mutate(borough = substr(geoid, 1, 5),
         borough = if_else(borough == "36085", "36047", borough))

data_overall <- data %>%
  filter(analytical_age_group == "all")%>%
  mutate(year = if_else(year <2019, 2018, 2021)) %>%
  group_by(consolidated_tds_number,year) %>%
  summarize(visits_per_population = mean(visits_per_population),
            spring_ed2 = mean(spring_ed2),
            summer_ed2 = mean(summer_ed2),
            fall_ed2 = mean(fall_ed2),
            winter_ed2 = mean(winter_ed2),
            building_age = mean(building_age),
            ppt =mean(ppt), 
            pop_young_old=mean(pop_young_old),
            tdmean = mean(tdmean),
            annual_pm = mean(annual_pm),
            ndvi = mean(ndvi),
            borough = first(borough),
            denominator = mean(denominator),
            analytical_age_group = first(analytical_age_group),
            first.treat = first(first.treat),
            deathrate_quartile = mean(deathrate_quartile),
            nycha_neighb_100 =first(nycha_neighb_100),
            group = first(group), 
            prop_uninsured = mean(prop_uninsured),
            mean_aadt = mean(mean_aadt),
            geoid = first(geoid)) %>%
  ungroup()

data_age <- data %>%
  filter(analytical_age_group != "all")%>%
  mutate(year = if_else(year <2019, 2018, 2021)) %>%
  group_by(consolidated_tds_number,year, analytical_age_group) %>%
  summarize(visits_per_population = mean(visits_per_population),
            spring_ed2 = mean(spring_ed2),
            summer_ed2 = mean(summer_ed2),
            fall_ed2 = mean(fall_ed2),
            winter_ed2 = mean(winter_ed2),
            building_age = mean(building_age),
            ppt =mean(ppt), 
            pop_young_old=mean(pop_young_old),
            tdmean = mean(tdmean),
            annual_pm = mean(annual_pm),
            ndvi = mean(ndvi),
            borough = first(borough),
            denominator = mean(denominator),
            analytical_age_group = first(analytical_age_group),
            first.treat = first(first.treat),
            deathrate_quartile = mean(deathrate_quartile),
            nycha_neighb_100 =first(nycha_neighb_100),
            group = first(group), 
            prop_uninsured = mean(prop_uninsured),
            mean_aadt = mean(mean_aadt),
            geoid = first(geoid)) %>%
  ungroup()


compute_attgt_by_group <- function(data, group_vars, xvars, outcome_var) {
  
  results <- data.frame(
    group = character(),
    estimate = numeric(),
    conf.low = numeric(),
    conf.high = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Create group_id column for splitting
  data <- data %>%
    mutate(group_id = interaction(across(all_of(group_vars)), drop = TRUE, sep = "_"))
  
  grouped_data <- split(data, data$group_id)
  
  for (group_name in names(grouped_data)) {
    df <- grouped_data[[group_name]]
    
    df <- df %>%
      filter(denominator != 0)
    
    # Fit att_gt
     fit_mod <-  att_gt(
        yname = outcome_var,
        tname = "year",
        idname = "consolidated_tds_number",
        gname = "first.treat",
        xformla = as.formula(paste("~", paste(xvars, collapse = " + "))),
        data = df,
        panel = TRUE
      )
     
     print(group_name)
     print(fit_mod)

    tidy_model <- tidy(fit_mod) %>%
      mutate(group = group_name)
    
    # Append results
    results <- bind_rows(results, tidy_model)

  }
  
  return(results)
  
  
}



results_spring <- compute_attgt_by_group(data_overall,
                                         outcome_var = "spring_ed2",
                                          group_vars = "analytical_age_group",
                                          xvars = c("building_age", "ppt", "pop_young_old",
                                                    "tdmean", "ndvi", "borough", 
                                                    "annual_pm")) %>% 
  mutate(group = paste0(group, "_spring"))

results_summer <- compute_attgt_by_group(data_overall,
                                         outcome_var = "summer_ed2",
                                         group_vars = "analytical_age_group",
                                         xvars = c("building_age", "ppt", "pop_young_old",
                                                   "tdmean", "ndvi", "borough",
                                                   "annual_pm")) %>% 
  mutate(group = paste0(group, "_summer"))

results_fall <- compute_attgt_by_group(data_overall,
                                         outcome_var = "fall_ed2",
                                         group_vars = "analytical_age_group",
                                         xvars = c("building_age", "ppt", "pop_young_old",
                                                   "tdmean", "ndvi","borough",
                                                   "annual_pm")) %>% 
  mutate(group = paste0(group, "_fall"))
results_winter <-  compute_attgt_by_group(data_overall,
                                         outcome_var = "winter_ed2",
                                         group_vars = "analytical_age_group",
                                         xvars = c("building_age", "ppt", "pop_young_old",
                                                   "tdmean", "ndvi","borough",
                                                   "annual_pm")) %>% 
  mutate(group =paste0(group, "_winter"))




results_spring_age <- compute_attgt_by_group(data_age,
                                         outcome_var = "spring_ed2",
                                         group_vars = "analytical_age_group",
                                         xvars = c("building_age", "ppt", "pop_young_old",
                                                   "tdmean", "ndvi", "borough",
                                                   "annual_pm"))%>% 
  mutate(group = paste0(group, "_spring"))


results_summer_age <- compute_attgt_by_group(data_age,
                                         outcome_var = "summer_ed2",
                                         group_vars = "analytical_age_group",
                                         xvars = c("building_age", "ppt", "pop_young_old",
                                                   "tdmean", "ndvi", "borough",
                                                   "annual_pm")) %>% 
  mutate(group = paste0(group, "_summer"))

results_fall_age <- compute_attgt_by_group(data_age,
                                       outcome_var = "fall_ed2",
                                       group_vars = "analytical_age_group",
                                       xvars = c("building_age", "ppt", "pop_young_old",
                                                 "tdmean", "ndvi","borough",
                                                 "annual_pm")) %>% 
  mutate(group = paste0(group, "_fall"))

results_winter_age <- compute_attgt_by_group(data_age,
                                         outcome_var = "winter_ed2",
                                         group_vars = "analytical_age_group",
                                         xvars = c("building_age", "ppt", "pop_young_old",
                                                   "tdmean", "ndvi",
                                                   "annual_pm")) %>% 
  mutate(group = paste0(group, "_winter"))


seasons_results <- rbind(results_spring, 
                         results_summer,
                         results_fall,
                         results_winter,
                         results_spring_age,
                         results_summer_age,
                         results_fall_age,
                         results_winter_age)


setwd(here("data", "results-datasets-did-aggregated"))

write.csv(seasons_results, "season-results.csv", row.names = FALSE)
