library(ggplot2)
library(patchwork)
library(dplyr)
library(paletteer)
library(here)
library(fst)
library(tidyverse)
library(forestploter)
library(Cairo)

setwd(here("data", "results-datasets-mold"))

data <- read.csv("results.csv")


# make overall plot -------------------------------------------------------

data_overall <- data %>%
  filter(group == "all") %>%
  arrange(match(exposure, c("total_reports", "total_repeats_6"))) %>%
  mutate(`RR (95% CI)` = sprintf("%.2f (%.2f, %.2f)",
                                 exp(10 * estimate),
                                 exp(10 * conf.low),
                                 exp(10 * conf.high)),
         " " = "                            ",  
         `Mold report` = case_when(exposure == "total_reports" ~ "Any report",
                                   exposure == "total_repeats_6" ~ "Repeat report")) %>%
         select(`Mold report`, ` `, `RR (95% CI)`, estimate, conf.low, conf.high, exposure)



plot_overall <- forest(data = data_overall[, c("Mold report",  " ","RR (95% CI)")],
                       est = exp(10 * data_overall$estimate),
                       lower = exp(10 * data_overall$conf.low),
                       upper = exp(10 * data_overall$conf.high),
                       ref_line = 1,
                       ci_column = 2,
                       ticks_at = c(1, 1.03, 1.06),
                       theme = forest_theme(base_size = 14,
                                            core = list(bg_params = list(fill = "white"))))

print(plot_overall)


data_wide <- data %>%
  mutate(group = case_when(group == "all" ~ "",
                           group == "5-17" ~ "5-17",
                           group == "18-39" ~ "18-39",
                           group == "40+" ~ "40+",
                           group == "F" ~ "F",
                           group == "M" ~ "M",
                           group == "18-39_F" ~ "F 18-39",
                           group == "18-39_M" ~ "M 18-39",
                           group == "5-17_F" ~ "F 5-17",
                           group == "5-17_M" ~ "M 5-17",
                           group == "40+_F" ~ "F 40+",
                           group == "40+_M" ~ "M 40+"
                           )) %>%
  mutate(group = factor(group, levels = c("",
                                          "5-17",
                                          "18-39",
                                          "40+",
                                          "F",
                                          "M",
                                          "F 5-17",
                                          "M 5-17",
                                          "F 18-39",
                                          "M 18-39",
                                          "F 40+",
                                          "M 40+"
                                          ))) %>%
  group_by(group) %>%
  mutate(exposure = case_when(
    exposure == "total_reports" ~ "exp1",
    exposure == "total_repeats_6" ~ "exp2"
  )) %>%
  select(exposure, estimate, conf.low, conf.high) %>%
  pivot_wider(names_from = exposure, values_from = c(estimate, conf.low, conf.high)) %>%
  ungroup() %>%
  arrange(group) %>%
  mutate(row = 1:n()) %>%
  mutate(Subgroup = paste0("     ", group))


header_rows <- tibble(Subgroup = c("Overall", "Age", "Sex", "Age and sex"),
                      row = c(.5, 1.5,4.5,6.5),
                      `Mold report` = "                                      ",
                      `RR (95% CI)` = "")

# Add label and blank cells for spacing / aesthetics
data_wide <- data_wide %>%
  mutate(`RR (95% CI)` = paste0(sprintf("%.2f (%.2f, %.2f)", exp(10 * estimate_exp1), exp(10 * conf.low_exp1), exp(10 * conf.high_exp1)), "\n",
                      sprintf("%.2f (%.2f, %.2f)", exp(10 * estimate_exp2), exp(10 * conf.low_exp2), exp(10 * conf.high_exp2))),
         `Mold report` = "                                      ") %>%
  bind_rows(header_rows) %>%
  arrange(row) %>%
  mutate(`RR (95% CI)`= if_else(`RR (95% CI)` == "NA (NA, NA)\nNA (NA, NA)\nNA (NA, NA)", "", `RR (95% CI)`))

# These should be interpreted as: A 10 unit increase in mold complaints per 
# 100 households is associated with 2 more visit per 100 for any report and 2 
# more visits per 100.


theme <- forest_theme(base_size = 8,
                      ci_col = c("#72874EFF", "#7570b3"),  # blue, green, purple
                      legend_name = "Exposure",
                      legend_value = c("Any report","Repeat reportᵃ"),
                      core = list(bg_params = list(fill = "white")))

p <- forest(data = data_wide[, c(12, 14, 13)],
            est = list(exp(10 * data_wide$estimate_exp1),
                       exp(10 * data_wide$estimate_exp2)),
            lower = list(exp(10 * data_wide$conf.low_exp1),
                         exp(10 * data_wide$conf.low_exp2)),
            upper = list(exp(10 * data_wide$conf.high_exp1),
                         exp(10 * data_wide$conf.high_exp2)),
            ci_column = 2,
            ref_line = 1,
            nudge_y = .3,
            ticks_at = c(1, 1.05, 1.1, 1.2),
            theme = theme)


plot(p)


setwd("C:/Users/nf2497/Desktop/mold-busters-project/figures")
ggsave("complaints-forest-plot.pdf",
       plot = p,
       device = cairo_pdf,
       width = 8,
       height = 11)



