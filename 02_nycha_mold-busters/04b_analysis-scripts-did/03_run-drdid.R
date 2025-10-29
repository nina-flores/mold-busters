library(ggplot2)
library(patchwork)
library(dplyr)
library(paletteer)
library(here)
library(fst)
library(tidyverse)
library(DRDID)
library(did)

# bind nycha and control datasets before saving out -----------------------
setwd(here("data", "did-analytical-datasets"))

data <- read.csv("data_agegroup.csv") %>%
  mutate(denominator = denominator_age) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population = 1000 *(total_ed / denominator)) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in%   c(2019, 2020, 2021)) %>%
  mutate(borough = substr(geoid,1,5)) 

data_overall <- data %>%
  filter(analytical_age_group == "all")

data_age <- data %>%
  filter(analytical_age_group != "all")

data_s <- read.csv("data_agesexgroup.csv") %>%
  mutate(denominator = denominator_age_sex) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population = 1000 * (total_ed / denominator)) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in%   c(2019, 2020, 2021)) %>%
  mutate(borough = substr(geoid,1,5)) %>%
  mutate(median_household_income = scale(median_household_income))

data_overall_sex <- data_s %>%
  filter(analytical_age_group == "all")

data_age_sex <- data_s %>%
  filter(analytical_age_group != "all")


data_m_AC <- read.csv(here("data", "did-analytical-datasets","data_lcgagroup.csv")) %>%
  mutate(denominator = denominator) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population =  1000 * (total_ed/denominator)) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in%   c(2019, 2020, 2021)) %>%
  mutate(class_letter = if_else(is.na(class_letter) & MB == 1, "No class", class_letter)) %>%
  filter((MB == 1 & class_letter != "E") | MB ==0) %>%
  filter(analytical_age_group == "all") %>%
  mutate(borough = substr(geoid,1,5)) 

data_sens <- read.csv(here("data", "did-analytical-datasets","data_lcgagroup.csv")) %>%
  mutate(denominator = denominator) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population =  1000 * (total_ed/denominator)) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in%   c(2019, 2020, 2021)) %>%
  mutate(class_letter = if_else(is.na(class_letter) & MB == 1, "No class", class_letter)) %>%
  filter(!(MB == 1 & class_letter == "No class")) %>%
  filter(analytical_age_group == "all") %>%
  mutate(borough = substr(geoid,1,5)) 


data_m_DE <- read.csv(here("data", "did-analytical-datasets","data_lcgagroup.csv")) %>%
  mutate(denominator = denominator) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population =  1000 * (total_ed/denominator)) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in%   c(2019, 2020, 2021)) %>%
  filter((MB == 1 & class_letter == "E") | MB ==0 | (MB == 1 & class_letter == "D")) %>%
  filter(analytical_age_group == "all") %>%
  mutate(borough = substr(geoid,1,5)) 
  


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


  
results_overall <- compute_attgt_by_group(data_overall, 
                                        group_vars = "analytical_age_group",
                                        xvars = c( "building_age","ppt", "pop_young_old",
                                                  "tdmean", 
                                                  "annual_pm", 
                                                  "ndvi", "nycha_neighb_1000")
  )

results_sens <- compute_attgt_by_group(data_sens, 
                                          group_vars = "analytical_age_group",
                                          xvars = c("building_age", "ppt", "pop_young_old",
                                                    "tdmean", 
                                                    "annual_pm", 
                                                    "ndvi", "nycha_neighb_1000")) %>%
  mutate(group = "sens")



results_age <- compute_attgt_by_group(data_age, 
                                   group_vars = "analytical_age_group",
                                   xvars = c("building_age", "ppt", "pop_young_old",
                                             "tdmean",
                                             "annual_pm", "nycha_neighb_1000",
                                             "ndvi")
                                )


results_overall_sex <- compute_attgt_by_group(data_overall_sex, 
                                          group_vars = c("analytical_age_group", "gender"),
                                          xvars = c("building_age", "ppt",  
                                                    "tdmean", "pop_young_old",
                                                    "annual_pm" , "nycha_neighb_1000",
                                                    "ndvi")) 


results_age_sex <- compute_attgt_by_group(data_age_sex, 
                                      group_vars = c("analytical_age_group", "gender"),
                                      xvars = c("building_age", "ppt", "pop_young_old",
                                                "tdmean",  "nycha_neighb_1000",
                                                "annual_pm", 
                                                "ndvi")) 


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

xvars <- c("building_age", "ppt", "pop_young_old",
           "tdmean", 
           "annual_pm", 
           "ndvi", "nycha_neighb_1000")
class_letters <- c("A", "B", "C", "No class")

results_mold_AC <- list()

for (cl in class_letters) {
  results_mold_AC[[cl]] <- run_attgt_for_class(cl, data_m_AC, xvars)
}

combined_results_AC <- bind_rows(results_mold_AC, .id = "group")



xvars2 <- c("building_age", "ppt",
           "tdmean", 
           "annual_pm", 
           "ndvi", "nycha_neighb_1000")
class_letters2 <- c("D", "E")

results_mold_DE <- list()

for (cl in class_letters2) {
  results_mold_DE[[cl]] <- run_attgt_for_class(cl, data_m_DE, xvars2)
}

combined_results_DE <- bind_rows(results_mold_DE, .id = "group")




data_results <- rbind(results_overall,
                      results_age,
                      results_overall_sex,
                      results_age_sex,
                      combined_results_AC,
                      combined_results_DE,
                      results_sens)

setwd(here("data", "results-datasets-did"))

write.csv(data_results, "results.csv", row.names = FALSE)


