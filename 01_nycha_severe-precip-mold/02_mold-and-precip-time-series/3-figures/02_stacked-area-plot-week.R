### Load packages
library(fst)
library(lubridate)
library(tidyverse)
library(dplyr)
library(zoo)
library(gtsummary)
library(naniar)
library(visdat)
library(sf)
library(ggplot2)
library(tigris)
library(ggpubr)
library(grid)

# Function to prepare data and create the plot
create_plot <- function(data, title) {
  # Summarize data by week
  figure_dat <- data %>%
    group_by(MMWRweek, year_week_date) %>%
    summarise(total_complaints = sum(mold_report, na.rm = TRUE),
              founded_complaints = sum(founded_mold, na.rm = TRUE),
              severe_complaints = sum(severe_mold, na.rm = TRUE),
              severe_event_week = sum(week_95th_ind_precip, na.rm = TRUE),
              severe_event_week = ifelse(severe_event_week > 0, 1, 0)) %>%
    mutate(date_label = format(as.Date(year_week_date), "%m/%d"))
  
  # Identify severe weeks
  figure_dat_severe <- figure_dat %>%
    filter(severe_event_week == 1)
  
  # Reshape data to long format
  long_data <- figure_dat %>%
    pivot_longer(cols = c(total_complaints, founded_complaints, severe_complaints),
                 names_to = "complaint_type",
                 values_to = "count")
  
  # Create the plot
  plot <- long_data %>%
    ggplot(aes(x = MMWRweek, y = count, fill = complaint_type)) +
    geom_area(position = "identity", alpha = .6) +
    scale_fill_manual(values = c("total_complaints" = "#c7e9c0", 
                                 "founded_complaints" = "#41ab5d", 
                                 "severe_complaints" = "#00441b"),
                      labels = c("total_complaints" = "Total Complaints", 
                                 "founded_complaints" = "Founded Complaints", 
                                 "severe_complaints" = "Severe Complaints"),
                      breaks = c("total_complaints", "founded_complaints", "severe_complaints")) +
    scale_x_continuous(breaks = figure_dat$MMWRweek, labels = figure_dat$date_label, expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 525), breaks = seq(0, 500, by = 100)) +
    theme_bw() +
    theme(legend.position = "bottom",
          text = element_text(size = 14),
          axis.text.x = element_text(angle = 90, vjust = .4)) +
    labs(x = "", y = "", fill = "") +
    ggtitle(title) +
    geom_vline(xintercept = figure_dat_severe$MMWRweek, color = "#2c7fb8", alpha = .8, linetype = "dashed")
  
  return(plot)
}

# Load data for 2021
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/time_series")
analytical_dat_2021 <- readRDS("spatial_analytical_dat_frame.rds") %>%
  dplyr::select(year_week_date, MMWRyear, MMWRweek, mold_report, 
                founded_mold, severe_mold, week_95th_ind_precip) %>%
  filter(MMWRyear == 2021) %>%
  arrange(MMWRyear, MMWRweek)

sum(analytical_dat_2021$founded_mold)


# Create 2021 plot
fig_2021 <- create_plot(analytical_dat_2021, title = "a. 2021")

# Load data for 2022
analytical_dat_2022 <- readRDS("spatial_analytical_dat_frame.rds") %>%
  dplyr::select(year_week_date, MMWRyear, MMWRweek, mold_report, 
                founded_mold, severe_mold, week_95th_ind_precip) %>%
  filter(MMWRyear == 2022) %>%
  arrange(MMWRyear, MMWRweek)

# Create 2022 plot
fig_2022 <- create_plot(analytical_dat_2022, title = "b. 2022")

# Load data for 2023
analytical_dat_2023 <- readRDS("spatial_analytical_dat_frame.rds") %>%
  dplyr::select(year_week_date, MMWRyear, MMWRweek, mold_report, 
                founded_mold, severe_mold, week_95th_ind_precip) %>%
  filter(MMWRyear == 2023) %>%
  arrange(MMWRyear, MMWRweek)


sum(analytical_dat_2023$founded_mold)


# Create 2023 plot
fig_2023 <- create_plot(analytical_dat_2023, title = "c. 2023")

# Combine all three plots
fig <- ggarrange(fig_2021, fig_2022, fig_2023, ncol = 1, nrow = 3, heights = c(1, 1), 
                 common.legend = TRUE, legend = "bottom")

# Annotate and save the figure
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/figures/time-series")
annotate_figure(fig, left = textGrob("Weekly number of mold complaints", rot = 90, vjust = 1, gp = gpar(cex = 1.3)))

ggsave("any_stacked_area.pdf", fig, width = 11, height = 8.5)
