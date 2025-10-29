# Load packages
require(sf)
require(dplyr)
require(tidyverse)
require(ggplot2)
require(splines)
require(MASS)
require(purrr)
require(DHARMa)
require(lme4)
require(splines2)
require(forestploter)
require(patchwork)
require(gridExtra)
require(glmmTMB)
require(broom.mixed)
require(fst)

set.seed(444)

# Load data
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/time_series")
analytical_dat <- read.fst("em_analytical_data.fst") %>%
  filter(!is.na(class5))
sum(analytical_dat$n_founded)

# Function to process GLMM results using broom.mixed
process_glmmTMB_results <- function(model) {
  tryCatch({
    tidy_results <- tidy(model, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
    
    results <- tidy_results %>%
      rename(Variable = term, 
             RateRatio = estimate,
             Lower = conf.low, 
             Upper = conf.high, 
             PValue = p.value) %>%
      mutate(across(c(RateRatio, Lower, Upper), round, 2)) %>%
      filter(grepl("sw", Variable)) %>%
      rename(`Weeks since extreme precipitation      ` = Variable) %>%
      mutate(`Weeks since extreme precipitation      ` = as.numeric(gsub("sw_ind_", "", `Weeks since extreme precipitation      `))) %>%
      mutate(`RR (95% CI)` = paste0(RateRatio, " (", Lower, ", ", Upper, ")"))
      
    
    return(results)
  }, error = function(e) {
    return(NULL)
  })
}



# Function to run stratified models with error handling
run_stratified_models <- function(outcome_var, strat_var, data, sw_weeks = 6, spline_df) {
  sw_terms <- paste0("sw_ind_", 0:sw_weeks, collapse = "+")
  formula_str <- paste0(
    outcome_var, " ~ ", sw_terms, "+",
    "windspeed_weekly_mean + is_holiday + year_month_MB_group +",
    "ns(year_week_date, df = ", spline_df, ") * year_month_MB_group +",
    "ns(scale(x), df = 10) + ns(scale(y), df = 10) +",
    "(1 | development_factor) + (1 | id) + (1 | consolidated_tds_number)"
  )
  
  model_null <- tryCatch(
    glmmTMB(as.formula(formula_str),
            offset = data$household_offset,  
            family = nbinom2, 
            data = data),
    error = function(e) NULL
  )
  
  interaction_terms <- paste0("sw_ind_", 0:sw_weeks, "*", strat_var, collapse = " + ")
  formula_interaction <- paste0(formula_str, " + ", interaction_terms)
  
  model_alt <- tryCatch(
    glmmTMB(as.formula(formula_interaction),
            offset = data$household_offset,  
            family = nbinom2, 
            data = data),
    error = function(e) NULL
  )
  
  p_value <- if (!is.null(model_null) & !is.null(model_alt)) {
    f_test <- anova(model_null, model_alt, test = "Chisq")
    f_test$`Pr(>Chisq)`[2]
  } else {
    NA
  }
  
  stratified_results <- list()
  split_data <- split(data, data[[strat_var]])
  
  for (group in names(split_data)) {
    cat("Running model for group:", group, "\n")
    
    model_group <- tryCatch(
      glmmTMB(as.formula(formula_str), 
              data = split_data[[group]],
              offset = split_data[[group]]$household_offset,  
              family = nbinom2),
      error = function(e) NULL
    )
    
    if (!is.null(model_group)) {
      results <- process_glmmTMB_results(model_group) %>% mutate(Group = group, P_FTest = p_value)
    } else {
      results <- data.frame(
        `Weeks since extreme precipitation` = NA,
        RateRatio = NA,
        Lower = NA,
        Upper = NA,
        PValue = NA,
        `OR (95% CI)` = NA,
        Group = group,
        P_FTest = p_value
      )
    }
    
    stratified_results[[group]] <- results
  }
  
  return(stratified_results)
}

# Function to create a forest plot
create_forest_plot <- function(model_output, title, xlim = c(0.7, 1.4), ticks = c(0.75, 1, 1.25)) {
  if (all(is.na(model_output$RateRatio))) {
    return(NULL)
  }
  
  forest_plot <- forest(
    model_output[, c(3, 10)],
    est = model_output$RateRatio,
    lower = model_output$Lower,
    upper = model_output$Upper,
    ci_column = 1,
    ref_line = 1,
    arrow_lab = c("Fewer", paste("More", title)),
    xlim = xlim,
    ticks_at = ticks
  ) 
  
  return(forest_plot)
}

# Define outcomes
outcomes <- list(
  founded = list(var = "n_founded", title = "founded mold complaints")
)

# Define stratification variable
stratifying_var <- "class_letter"

# Run models
stratified_results <- list()
for (outcome_name in names(outcomes)) {
  outcome_info <- outcomes[[outcome_name]]
  stratified_results[[outcome_name]] <- run_stratified_models(outcome_info$var, stratifying_var, analytical_dat, sw_weeks = 6, spline_df = 9)
}

# Arrange results into a single data frame
combined_results <- bind_rows(
  lapply(names(stratified_results), function(outcome) {
    bind_rows(lapply(stratified_results[[outcome]], function(df) df %>% mutate(Outcome = outcome)), .id = "Group")
  })
)

# Create forest plots
forest_plots <- list()
for (outcome in names(outcomes)) {
  for (group in names(stratified_results[[outcome]])) {
    plot <- create_forest_plot(stratified_results[[outcome]][[group]], outcomes[[outcome]]$title)
    if (!is.null(plot)) {
      forest_plots[[paste(outcome, group, sep = "_")]] <- plot
    }
  }
}

setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/figures/time-series")

# Arrange in grid
if (length(forest_plots) > 0) {
  pdf("stratified_forest_plot_with_main.pdf", width = 10, height = 9)
  do.call(grid.arrange, c(main_results_combined,forest_plots, ncol = 2, nrow = 3))
  dev.off()
}

# # Print F-test significance at the top
# f_test_results <- unique(combined_results %>% select(Outcome, Group, P_FTest))
# print(f_test_results)



if (length(forest_plots) > 0) {
  pdf("stratified_forest_plot_with_main.pdf", width = 10, height = 9)
  do.call(grid.arrange, 
          c(list(main_results_combined), forest_plots, 
            list(ncol = 2, nrow = 3)))
  dev.off()
}

