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
# identify units that meet all thresholds in every year
keep_units <- read.csv("data_agegroup_ct.csv")%>%
  group_by(geo_group) %>%
  filter(all(prop_female >= .45),
         all(pop_young_old >= .5),
         all( building_age <= 95)) %>%
  pull(geo_group) %>%
  unique()

# bind nycha and control datasets before saving out -----------------------
setwd(here("data", "did-analytical-datasets"))

data <- read.csv("data_agegroup_ct.csv") %>%
  mutate(denominator = denominator_age) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population = 1000 *(total_ed / denominator),
         visits_non_asthma_per_pop = 1000 *(total_ed_non_asthma / denominator)) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in% c(2019, 2020)) %>%
  filter(nycha_neighb_250 == 1) %>%
  mutate(borough = substr(geoid, 1, 5),
         borough = if_else(borough == "36085", "36047", borough))%>%
  filter(geo_group %in% keep_units)

summary(data)

baseline_2016 <- data %>%
  filter(year == 2016) %>%
  select(geo_group, analytical_age_group, baseline_2016 = visits_non_asthma_per_pop)

data <- data %>%
  left_join(baseline_2016, by = c("geo_group", "analytical_age_group")) %>%
  mutate(per_diff = 100 * (visits_non_asthma_per_pop - baseline_2016) / baseline_2016)

data_overall <- data %>% filter(analytical_age_group == "all")
data_age     <- data %>% filter(analytical_age_group != "all")

data_s <- read.csv("data_agesexgroup_ct.csv") %>%
  mutate(denominator = denominator_age_sex) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population = 1000 * (total_ed / denominator)) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in% c(2019, 2020)) %>%
  filter(nycha_neighb_250 == 1) %>%
  mutate(borough = substr(geoid, 1, 5),
         borough = if_else(borough == "36085", "36047", borough))%>%
  filter(geo_group %in% keep_units)

data_overall_sex <- data_s %>% filter(analytical_age_group == "all")
data_age_sex     <- data_s %>% filter(analytical_age_group != "all")

data_m <- read.csv(here("data", "did-analytical-datasets", "data_lcgagroup_ct.csv")) %>%
  mutate(denominator = denominator) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population = 1000 * (total_ed / denominator)) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in% c(2019, 2020)) %>%
  mutate(class_letter = if_else(is.na(class_letter) & MB == 1, "No class", class_letter)) %>%
  filter(analytical_age_group == "all") %>%
  filter(nycha_neighb_250 == 1) %>%
  mutate(borough = substr(geoid, 1, 5),
         borough = if_else(borough == "36085", "36047", borough)) %>%
  filter(geo_group %in% keep_units)

data_sens <- read.csv(here("data", "did-analytical-datasets", "data_lcgagroup_ct.csv")) %>%
  mutate(denominator = denominator) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population = 1000 * (total_ed / denominator)) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in% c(2019, 2020)) %>%
  mutate(class_letter = if_else(is.na(class_letter) & MB == 1, "No class", class_letter)) %>%
  filter(!(MB == 1 & class_letter == "No class")) %>%
  filter(analytical_age_group == "all") %>%
  filter(nycha_neighb_250 == 1) %>%
  mutate(borough = substr(geoid, 1, 5),
         borough = if_else(borough == "36085", "36047", borough)) %>%
  filter(geo_group %in% keep_units)


# --- Model helpers -------------------------------------------------------

main_vars <- c("building_age", "ppt", "pop_young_old",
               "tdmean", "annual_pm", "ndvi", "borough")
xvars2    <- c("building_age", "ppt", "tdmean", "pop_young_old2",
               "annual_pm", "ndvi", "borough")

compute_attgt_by_group <- function(data, group_vars, xvars,
                                   outcome_var = "visits_per_population") {
  results <- data.frame()
  
  data <- data %>%
    mutate(group_id = interaction(across(all_of(group_vars)), drop = TRUE, sep = "_"))
  
  grouped_data <- split(data, data$group_id)
  
  for (group_name in names(grouped_data)) {
    
    df <- grouped_data[[group_name]] %>%
      filter(denominator != 0)
    
    fit_mod <- att_gt(
      yname      = outcome_var,
      tname      = "year",
      idname     = "geo_group",
      gname      = "first.treat",
      est_method = "dr",
      xformla    = as.formula(paste("~", paste(xvars, collapse = " + "))),
      data       = df,
      panel      = TRUE
    )
    
    print(group_name)
    print(fit_mod)
    
    results <- bind_rows(results, tidy(fit_mod) %>% mutate(group = group_name))
  }
  
  results
}

run_attgt_for_class <- function(class_letter_input, data, xvars) {
  
  data_filtered <- data %>%
    filter((MB == 1 & class_letter == class_letter_input) | MB == 0) %>%
    filter(denominator != 0)
  
  fit_mod <- att_gt(
    yname      = "visits_per_population",
    tname      = "year",
    idname     = "geo_group",
    gname      = "first.treat",
    est_method = "dr",
    xformla    = as.formula(paste("~", paste(xvars, collapse = " + "))),
    data       = data_filtered,
    panel      = TRUE
  )
  
  print(fit_mod)
  tidy(fit_mod) %>% mutate(group = class_letter_input)
}


# --- Run models ----------------------------------------------------------

results_overall     <- compute_attgt_by_group(data_overall,     group_vars = "analytical_age_group",              xvars = main_vars)
results_age         <- compute_attgt_by_group(data_age,         group_vars = "analytical_age_group",              xvars = main_vars)
results_overall_sex <- compute_attgt_by_group(data_overall_sex, group_vars = c("analytical_age_group", "gender"), xvars = main_vars)
results_age_sex     <- compute_attgt_by_group(data_age_sex,     group_vars = c("analytical_age_group", "gender"), xvars = main_vars)
results_sens        <- compute_attgt_by_group(data_sens,        group_vars = "analytical_age_group",              xvars = main_vars)                         %>% mutate(group = "sens")
results_sens2       <- compute_attgt_by_group(data_overall,     group_vars = "analytical_age_group",              xvars = c("building_age", "ppt", "pop_young_old", "tdmean", "annual_pm", "per_diff", "ndvi", "borough")) %>% mutate(group = "with_non_asthma")
results_sens3       <- compute_attgt_by_group(data_overall,     group_vars = "analytical_age_group",              xvars = c("deathrate_quartile"))            %>% mutate(group = "with_deathrate")

results_AC <- bind_rows(lapply(c("A", "B", "C", "D", "No class"), run_attgt_for_class, data = data_m, xvars = main_vars), .id = "group")
results_DE <- bind_rows(lapply("E",                                run_attgt_for_class, data = data_m, xvars = xvars2),    .id = "group")


# --- Combine and save ----------------------------------------------------

data_results <- bind_rows(
  results_overall, results_age, results_overall_sex, results_age_sex,
  results_AC, results_DE,
  results_sens, results_sens2, results_sens3
)

setwd(here("data", "results-datasets-did"))
write.csv(data_results, "results_notrim.csv", row.names = FALSE)