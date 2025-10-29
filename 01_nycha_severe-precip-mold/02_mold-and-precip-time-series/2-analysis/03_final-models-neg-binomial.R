# Load packages -----------------------------------------------------------
require(dplyr)
require(tidyverse)
require(splines)
require(MASS)
require(purrr)
require(glmmTMB)
require(splines2)
require(forestploter)
require(patchwork)
require(gridExtra)
require(broom.mixed)

set.seed(444)

# Load analysis objects ---------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/time_series")
analytical_dat <- readRDS("spatial_analytical_dat_frame.rds") 


sum(analytical_dat$n_founded)

process_glmmTMB_results <- function(model) {
  tidy_results <- tidy(model, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
  
  results <- tidy_results %>%
    rename(Variable = term, 
           RateRatio = estimate,
           Lower = conf.low, 
           Upper = conf.high, 
           PValue = p.value) %>%
    mutate(across(c(RateRatio, Lower, Upper), round, 2))
  
  return(results)
}

# Function to run models and extract results
run_model_and_extract <- function(outcome_var, data, sw_weeks = sw_weeks, spline_df) {
  sw_terms <- paste0("sw_ind_", 0:sw_weeks,collapse = "+")
  formula_str <- paste0(
    outcome_var, " ~ ", 
    sw_terms, "+",
    " windspeed_weekly_mean  + is_holiday + year_month_MB_group +",
    "ns(year_week_date, df = ", spline_df, ") * year_month_MB_group+",
    "ns(scale(x), df = 10) + ns(scale(y), df = 10) +",
    " (1 | consolidated_tds_number) + (1 | development_factor ) + (1| id)"
  )
  
  model <- glmmTMB(
    as.formula(formula_str),
    offset = household_offset,  
    family = nbinom2, 
    data = data)
  
  model_output <- process_glmmTMB_results(model) %>%
    mutate(`RR (95% CI)` = paste0(RateRatio, " (", Lower, ", ", Upper, ")")) %>%
    filter(grepl("sw", Variable)) %>%
    rename(`Weeks since extreme precipitation      ` = Variable) %>%
    mutate(
      `Weeks since extreme precipitation      ` = as.numeric(gsub("sw_ind_", "", `Weeks since extreme precipitation      `)),
      `                                                       ` = ""
    )
  
  return(list(model = model, output = model_output))
}

create_forest_plot <- function(model_output, title, xlim = c(.85, 1.15), ticks = c(0.9, 1, 1.1)) {
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


outcomes_6week <- list(
  founded = list(var = "n_founded", title = "founded mold complaints")
)

main_results_list <- lapply(outcomes_6week, function(outcome) {
  result <- run_model_and_extract(outcome$var, analytical_dat, sw_weeks = 6, spline_df = 9)
  plot <- create_forest_plot(result$output, outcome$title)
  return(list(model = result$model, output = result$output, plot = plot))
})

main_results_combined <- arrangeGrob(
  main_results_list$founded$plot,  ncol = 1
)

setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/figures/time-series")
pdf("main_result_forest_plot.pdf", width = 10, height = 6)
grid::grid.draw(main_results_combined)
dev.off()

outcomes_other <- list(
  any_report = list(var = "n", title = "mold complaints"),
  severe = list(var = "n_severe", title = "severe mold"),
  non_severe = list(var = "n_founded_no_severe", title = "founded, not severe"),
  repeat_reports = list(var = "n_repeat_reports", title = "founded, 6 month repeats")
)

other_results_list <- lapply(outcomes_other, function(outcome) {
  result <- run_model_and_extract(outcome$var, analytical_dat, sw_weeks = 6, spline_df = 9)
  plot <- create_forest_plot(
    result$output, 
    outcome$title,
    xlim = c(.8, 1.2), ticks = c(0.9, 1, 1.1)
  )
  return(list(model = result$model, output = result$output, plot = plot))
})

other_plots <- lapply(other_results_list, function(x) x$plot)
combined_plot_6_hr <- arrangeGrob(
  other_plots$any_report, 
  other_plots$severe, 
  other_plots$non_severe, 
  other_plots$repeat_reports,
  ncol = 2
)

pdf("other_6hr_results.pdf", width = 11, height = 8.5)
grid::grid.draw(combined_plot_6_hr)
dev.off()



outcomes_room <- list(
  n_bathroom = list(var = "n_bathroom", title = "founded mold in bathroom"),
  n_non_bathroom = list(var = "n_non_bathroom", title = "founded mold in non-bathroom"),
  n_multiple_room = list(var = "n_multiple_room", title = "founded mold in multiple rooms")
)

room_results_list <- lapply(outcomes_room, function(outcome) {
  result <- run_model_and_extract(outcome$var, analytical_dat, sw_weeks = 6, spline_df = 9)
  plot <- create_forest_plot(
    result$output, 
    outcome$title,
    xlim = c(.8, 1.3), ticks = c(0.9, 1, 1.1, 1.2)
  )
  return(list(model = result$model, output = result$output, plot = plot))
})

room_plots <- lapply(room_results_list, function(x) x$plot)
room_plot_6_hr <- arrangeGrob(
  room_plots$n_bathroom, 
  room_plots$n_non_bathroom, 
  room_plots$n_multiple_room, 
  ncol = 2
)

setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/figures/time-series")

pdf("room_6hr_results.pdf", width = 11, height = 8.5)
grid::grid.draw(room_plot_6_hr)
dev.off()



analytical_dat_no_2021 <- analytical_dat %>%
  filter(year > 2021)

results_no_2021_list <- lapply(outcomes_6week, function(outcome) {
  result <- run_model_and_extract(outcome$var, analytical_dat_no_2021, sw_weeks = 6, spline_df = 6)
  plot <- create_forest_plot(result$output, outcome$title, xlim = c(.8, 1.2), ticks = c(0.9, 1, 1.1))
  return(list(model = result$model, output = result$output, plot = plot))
})

results_no_2021_combined <- arrangeGrob(
  results_no_2021_list$founded$plot
)

pdf("result_forest_plot_2022_2023.pdf", width = 10, height = 6)
grid::grid.draw(results_no_2021_combined)
dev.off()



setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/time_series")
dat <- readRDS("spatial_analytical_dat_frame-first-only.rds")

results_first <- lapply(outcomes_6week, function(outcome) {
  result <- run_model_and_extract(outcome$var, dat, sw_weeks = 6, spline_df = 9)
  plot <- create_forest_plot(result$output, outcome$title)
  return(list(model = result$model, output = result$output, plot = plot))
})

results_first_combined <- arrangeGrob(
  results_first$founded$plot
)

setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/figures/time-series")

pdf("result_forest_plot_first.pdf", width = 10, height = 6)
grid::grid.draw(results_first_combined)
dev.off()



results_8 <- lapply(outcomes_6week, function(outcome) {
  result <- run_model_and_extract(outcome$var, analytical_dat, sw_weeks = 8, spline_df = 9)
  plot <- create_forest_plot(result$output, outcome$title)
  return(list(model = result$model, output = result$output, plot = plot))
})

results_8_combined <- arrangeGrob(
  results_8$founded$plot
)

pdf("result_forest_plot_8.pdf", width = 10, height = 6)
grid::grid.draw(results_8_combined)
dev.off()
