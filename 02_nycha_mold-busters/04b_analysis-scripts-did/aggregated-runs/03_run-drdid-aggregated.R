library(ggplot2)
library(patchwork)
library(dplyr)
library(paletteer)
library(here)
library(fst)
library(tidyverse)
library(DRDID)
library(did)

setwd(here("data"))
controls_exc <- read.csv("exclusive_controlsv3.csv")

# bind nycha and control datasets before saving out -----------------------
setwd(here("data", "did-analytical-datasets"))


data <- read.csv("data_agegroup.csv") %>%
  mutate(denominator = denominator_age) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population = 1000 *(total_ed / denominator),
         visits_non_asthma_per_pop = 1000 *(total_ed_non_asthma / denominator),
         
         denominator10 = if_else(group == "nycha", denominator*1.1, denominator),
         visits_per_population10 = 1000 *(total_ed / denominator10),
         
         denominator40 = if_else(group == "nycha", denominator*1.4, denominator),
         visits_per_population40 = 1000 *(total_ed / denominator40),
         
         ) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in%   c(2019, 2020)) %>%
  filter(  nycha_neighb_250 == 1) %>%
  mutate(borough = substr(geoid, 1, 5),
         borough = if_else(borough == "36085", "36047", borough)) 


# geoid <- data %>%
#   select(MB, geoid) %>%
#   unique()
# 
# geoid_1 <- geoid %>% filter(MB ==1)
# geoid_0 <- geoid %>% filter(MB ==0)
# 
# geoid_1_filt <- geoid_1 %>%
#   filter(geoid %in% geoid_0$geoid)
# 
# geoid_0_filt <- geoid_0 %>%
#   filter(geoid %in% geoid_1$geoid)


# Extract 2016 baseline values
baseline_2016 <- data %>%
  filter(year == 2016) %>%
  select(consolidated_tds_number, analytical_age_group, baseline_2016 = visits_non_asthma_per_pop)

# Join back and calculate percent difference
data <- data %>%
  left_join(baseline_2016, by = c("consolidated_tds_number", "analytical_age_group")) %>%
  mutate(per_diff = 100 * (visits_non_asthma_per_pop - baseline_2016) / baseline_2016)

data_overall <- data %>%
  filter(analytical_age_group == "all")%>%
  mutate(year = if_else(year <2019, 2018, 2021)) %>%
  group_by(consolidated_tds_number,year, analytical_age_group) %>%
  summarize(visits_per_population = mean(visits_per_population),
            visits_per_population10 = mean(visits_per_population10),
            visits_per_population40 = mean(visits_per_population40),
            building_age = mean(building_age),
            ppt =mean(ppt), 
            pop_young_old=mean(pop_young_old),
            tdmean = mean(tdmean),
            annual_pm = mean(annual_pm),
            ndvi = mean(ndvi),
            per_diff = mean(per_diff), 
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


data_low_income <- data_overall %>%
  filter(group == "nycha" | group == "control" & geoid %in% controls_exc$GEOID) 


data_proximate <- data_overall %>%
  filter(nycha_neighb_100 == 1) 

data_age <- data %>%
  filter(analytical_age_group != "all") %>%
  mutate(year = if_else(year <2019, 2018, 2021)) %>%
  group_by(consolidated_tds_number,year, analytical_age_group) %>%
  summarize(visits_per_population = mean(visits_per_population),
            building_age = mean(building_age),
            ppt =mean(ppt), 
            pop_young_old=mean(pop_young_old),
            tdmean = mean(tdmean),
            annual_pm = mean(annual_pm),
            ndvi = mean(ndvi),
            per_diff = mean(per_diff), 
            borough = first(borough),
            denominator = mean(denominator),
            analytical_age_group = first(analytical_age_group),
            first.treat = first(first.treat))%>%
  ungroup()

data_s <- read.csv("data_agesexgroup.csv") %>%
  mutate(denominator = denominator_age_sex) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population = 1000 * (total_ed / denominator)) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in% c(2019, 2020)) %>%
  mutate(borough = substr(geoid, 1, 5),
         borough = if_else(borough == "36085", "36047", borough)) %>%
  filter( nycha_neighb_250 == 1) 

data_overall_sex <- data_s %>%
  filter(analytical_age_group == "all") %>%
  mutate(year = if_else(year <2019, 2018, 2021)) %>%
  group_by(consolidated_tds_number,year, analytical_age_group, gender) %>%
  summarize(visits_per_population = mean(visits_per_population),
            building_age = mean(building_age),
            ppt =mean(ppt), 
            gender = first(gender), 
            pop_young_old=mean(pop_young_old),
            tdmean = mean(tdmean),
            annual_pm = mean(annual_pm),
            ndvi = mean(ndvi),
            borough = first(borough),
            denominator = mean(denominator),
            analytical_age_group = first(analytical_age_group),
            first.treat = first(first.treat)) %>%
  ungroup()

data_age_sex <- data_s %>%
  filter(analytical_age_group != "all") %>%
  mutate(year = if_else(year <2019, 2018, 2021)) %>%
  group_by(consolidated_tds_number,year, analytical_age_group, gender) %>%
  summarize(visits_per_population = mean(visits_per_population),
            building_age = mean(building_age),
            ppt =mean(ppt), 
            gender = first(gender), 
            pop_young_old=mean(pop_young_old),
            tdmean = mean(tdmean),
            annual_pm = mean(annual_pm),
            ndvi = mean(ndvi),
            borough = first(borough),
            denominator = mean(denominator),
            analytical_age_group = first(analytical_age_group),
            first.treat = first(first.treat))%>%
  ungroup()
  


data_m <- read.csv(here("data", "did-analytical-datasets","data_lcgagroup.csv")) %>%
  mutate(denominator = denominator) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population =  1000 * (total_ed/denominator)) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in% c(2019, 2020)) %>%
  mutate(class_letter = if_else(is.na(class_letter) & MB == 1, "No class", class_letter)) %>%
  filter(analytical_age_group == "all") %>%
  mutate(borough = substr(geoid, 1, 5),
         borough = if_else(borough == "36085", "36047", borough)) %>%
  filter( nycha_neighb_250 == 1) %>%
  mutate(year = if_else(year <2019, 2018, 2021)) %>%
  group_by(consolidated_tds_number,year, analytical_age_group, class_letter) %>%
  summarize(visits_per_population = mean(visits_per_population),
            building_age = mean(building_age),
            ppt =mean(ppt), 
            pop_young_old=mean(pop_young_old),
            pop_young_old2 = mean(pop_young_old2),
            tdmean = mean(tdmean),
            annual_pm = mean(annual_pm),
            ndvi = mean(ndvi),
            borough = first(borough),
            denominator = mean(denominator),
            analytical_age_group = first(analytical_age_group),
            first.treat = first(first.treat),
            class_letter = first(class_letter),
            MB = first (MB))%>%
  ungroup()

data_sens <- read.csv(here("data", "did-analytical-datasets","data_lcgagroup.csv")) %>%
  mutate(denominator = denominator) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population =  1000 * (total_ed/denominator)) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in% c(2019, 2020)) %>%
  mutate(class_letter = if_else(is.na(class_letter) & MB == 1, "No class", class_letter)) %>%
  filter(!(MB == 1 & class_letter == "No class")) %>%
  filter(analytical_age_group == "all") %>%
  mutate(borough = substr(geoid, 1, 5),
         borough = if_else(borough == "36085", "36047", borough))  %>%
  filter( nycha_neighb_250 == 1) %>%
  mutate(year = if_else(year <2019, 2018, 2021)) %>%
  group_by(consolidated_tds_number,year) %>%
  summarize(visits_per_population = mean(visits_per_population),
            building_age = mean(building_age),
            ppt =mean(ppt), 
            pop_young_old=mean(pop_young_old),
            tdmean = mean(tdmean),
            annual_pm = mean(annual_pm),
            ndvi = mean(ndvi),
            borough = first(borough),
            denominator = mean(denominator),
            analytical_age_group = first(analytical_age_group),
            first.treat = first(first.treat))%>%
  ungroup()



compute_attgt_by_group <- function(data, group_vars, xvars, outcome_var = "visits_per_population") {
  
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


main_vars = c("building_age", "ppt", "pop_young_old",
              "tdmean", "annual_pm", "ndvi", "borough")


results_overall <- compute_attgt_by_group(data_overall, 
                                          group_vars = "analytical_age_group",
                                          xvars = main_vars
)

results_proximate <- compute_attgt_by_group(data_proximate, 
                                          group_vars = "analytical_age_group",
                                          xvars = main_vars
) %>% mutate(group = "Restricted control proximity")

results_low_income <- compute_attgt_by_group(data_low_income, 
                                            group_vars = "analytical_age_group",
                                            xvars = main_vars
)%>% mutate(group = "Restricted control income")

results_sens <- compute_attgt_by_group(data_sens, 
                                       group_vars = "analytical_age_group",
                                       xvars = main_vars) %>%
  mutate(group = "without_RAD")

results_sens2 <- compute_attgt_by_group(data_overall, 
                                        group_vars = "analytical_age_group",
                                        xvars = c(main_vars,"per_diff")) %>%
  mutate(group = "with_non_asthma")


results_sens3 <- compute_attgt_by_group(data_overall, 
                                        group_vars = "analytical_age_group",
                                        xvars = c("deathrate_quartile")) %>%
  mutate(group = "with_deathrate")

results_sens4 <- compute_attgt_by_group(data_overall, 
                                        group_vars = "analytical_age_group",
                                        xvars = c(main_vars, "prop_uninsured")) %>%
  mutate(group = "with_uninsured")


results_sens5 <- compute_attgt_by_group(data_overall, 
                                        group_vars = "analytical_age_group",
                                        xvars = c(main_vars, "mean_aadt")) %>%
  mutate(group = "with_traffic")


results_sens6 <- compute_attgt_by_group(data_overall, 
                                        outcome_var = "visits_per_population10",
                                        group_vars = "analytical_age_group",
                                        xvars = c(main_vars)) %>%
  mutate(group = "10_denominator")

results_sens7 <- compute_attgt_by_group(data_overall,
                                        outcome_var = "visits_per_population40",
                                        group_vars = "analytical_age_group",
                                        xvars = c(main_vars)) %>%
  mutate(group = "40_denominator")



results_age <- compute_attgt_by_group(data_age, 
                                      group_vars = "analytical_age_group",
                                      xvars = main_vars
)


results_overall_sex <- compute_attgt_by_group(data_overall_sex, 
                                              group_vars = c("analytical_age_group", "gender"),
                                              xvars = main_vars) 


results_age_sex <- compute_attgt_by_group(data_age_sex, 
                                          group_vars = c("analytical_age_group", "gender"),
                                          xvars = main_vars) 


# need a slightly different loop for running the class models -------------

run_attgt_for_class <- function(class_letter_input, data, xvars) {
  
  data_m <- data %>%
    filter((MB == 1 & class_letter == class_letter_input) | MB == 0)
  
  fit_mod <- att_gt(
    yname = "visits_per_population",
    tname = "year",
    idname = "consolidated_tds_number",
    gname = "first.treat",
    xformla = as.formula(paste("~", paste(xvars, collapse = " + "))),
    data = data_m,
    panel = TRUE
  )
  
  print(fit_mod)
  tidy(fit_mod) %>%
    mutate(group = class_letter_input)
}

xvars <- main_vars
class_letters <- c("A", "B", "C","D", "No class")

results_mold_AC <- list()

for (cl in class_letters) {
  results_mold_AC[[cl]] <- run_attgt_for_class(cl, data_m, xvars)
}

combined_results_AC <- bind_rows(results_mold_AC, .id = "group")



xvars2 <- c("building_age", "ppt",
            "tdmean", "pop_young_old2",
            "annual_pm", 
            "ndvi", "borough")
class_letters2 <- c("E")

results_mold_DE <- list()

for (cl in class_letters2) {
  results_mold_DE[[cl]] <- run_attgt_for_class(cl, data_m, xvars2)
}

combined_results_DE <- bind_rows(results_mold_DE, .id = "group")




data_results <- rbind(results_overall,
                      results_age,
                      results_overall_sex,
                      results_age_sex,
                      combined_results_AC,
                      combined_results_DE,
                      results_sens2,
                      results_sens3,
                      results_sens4,
                      results_sens5,
                      results_sens6,
                      results_sens7,
                      results_sens,
                      results_proximate,
                      results_low_income)

setwd(here("data", "results-datasets-did-aggregated"))

write.csv(data_results, "results.csv", row.names = FALSE)


