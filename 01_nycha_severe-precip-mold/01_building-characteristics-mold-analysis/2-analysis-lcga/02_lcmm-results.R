# Here we link the lcmm results to the development data and we plot the median
# IQR with time

library(fst)
library(lme4)
library(dplyr)
library(ggplot2)
library(broom.mixed)
library(patchwork)
library(gtsummary)
library(ggrepel)
library(geomtextpath)
library(tidyverse)

# Set working directory
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")

# Read in dataset
data <- read.fst("analytical_data_clean.fst") %>%
  #filter(year != 2021) %>%
  filter(!(year == 2019 & qtr ==1)) %>%
  filter(!(year == 2019 & qtr ==2)) %>%
  filter(!(year == 2019 & qtr ==3)) %>%
  mutate(qtr = factor(qtr)) %>%
  mutate(mechanical_ventilation = if_else(mechanical_ventilation == 1, "Yes", "No"),
         sheetrock_status_flag = if_else(sheetrock_status_flag  == 1, "Yes", "No"),
         senior_development_flag = if_else(senior_development_flag == "exclusively", "Yes", "No"),
         buildings = building_total,
         sandy_development_flag = if_else(sandy_development_flag  == 1, "Yes", "No"),
         eop = if_else(is.na(eop), 0,1),
         eop = as.factor(eop)
  )


sum(data$total_founded_reports)

lcga_classes <- read.fst("classes_lcga2021.fst") %>%
  dplyr::select(class5, tds_number) %>%
  mutate(tds_number = as.character(tds_number)) %>%
  unique()

data <- data %>%
  mutate(tds_number = as.character(tds_number)) %>%
  left_join(lcga_classes, by = c("tds_number")) 


data <- data %>%
  filter(!is.na(class5))
  # these are the 15 pilot buildings that are not also
# converted to rad pact before 2020

setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")
rad <- read.csv( "rad_data_table.csv") %>%
  mutate(tds_number = as.character(tds_number)) %>%
  dplyr::select(-c(ndvi_quartile, heat_quartile)) %>%
  mutate(class5= class) %>%
  select(-class)



data_table <- data %>%
  filter(year == 2022 & qtr == 1) %>%
  dplyr::select(class5,   archetype, building_age, building_sq_footage, buildings,
         building_floors, sheetrock_status_flag, mechanical_ventilation,
         scattered_site_flag, total_households, pct_female, pct_pop_under_4, 
         senior_development_flag, building_median_income, pct_pop_nhblack, pct_pop_nhwhite, 
         pct_pop_hispanic, svi,
         ct_percent_pov,ct_percent_nhb, ct_percent_nhw, ct_percent_h,
         sandy_development_flag, intersects_coastal_flood_max, intersects_nuisance_flood_max,year_month_MB_factor, eop,
         intersects_deep_flood_max, f_deviation_smooth, ndvi, geographical_borough, tds_number) %>%
    rbind(rad) %>%
  mutate(year_month_MB_group = case_when(
    year_month_MB_factor == "Apr 2019" ~ "early",
    year_month_MB_factor  == "Jul 2019" | year_month_MB_factor  == "Aug 2019" ~ "mid",
    year_month_MB_factor  == "Sep 2019" | year_month_MB_factor  == "Nov 2019" | year_month_MB_factor  == "Oct 2019"~ "late")) %>%
  group_by(class5) %>%
  mutate(tot_dev = n(),
         tot_build = sum(buildings)) %>%
  ungroup() %>%
  mutate(class_letter_5 = case_when(
    class5 == 1 ~ "D:",
    class5 == 2 ~ "E:",
    class5 == 3 ~ "A:",
    class5 == 4 ~ "C:",
    class5 == 5 ~ "B:",
    class5 == "rad" ~ "RAD/PACT")) %>% 
  dplyr::select(-c(class5, year_month_MB_factor)) 

tbl_summary(data_table, by = class_letter_5, missing = "no") %>%
  modify_header(label = "**Variable**") %>% 
  bold_labels() %>% add_overall()


# plot the trajectories ---------------------------------------------------


# Create a new variable for year-quarter
data$year_qtr <- paste0(data$year, " Q", data$qtr)

# Create label expressions formatted as strings
data_table_lab <- data_table %>%
  mutate(
  class_labels = paste0("'", class_letter_5, "' ~ n[d] == ", tot_dev, " ~ n[b] == ", tot_build)
  ) %>%
  dplyr::select(tds_number, class_labels, class_letter_5)

# Merge label data into the main dataset
data <- data %>% mutate(tds_number = as.factor(tds_number)) %>%
  left_join(data_table_lab) %>%
  mutate(mold_rate = (total_founded_reports/total_households) *1000) %>%
  arrange(year, qtr) %>%
  group_by(tds_number) %>%
  mutate(time = row_number())%>%
  ungroup()

unique(data$class_labels)

# Define specific x-axis locations for the lines
covid_x <- 2  # Replace with actual x-axis value
end_rollout_x <- 1  # Replace with actual x-axis value

b <- ggplot(data, aes(x = time, y = mold_rate, color = as.factor(class_labels), fill = as.factor(class_labels), group = as.factor(class_labels))) +
  # First: Add ribbons (IQR)
  stat_summary(
    fun.data = function(x) {
      data.frame(
        y = median(x),
        ymin = quantile(x, 0.25),
        ymax = quantile(x, 0.75)
      )
    },
    geom = "ribbon",
    alpha = 0.3,
    color = NA
  ) +
  # Then: Add median lines (so they appear on top)
  stat_summary(fun = median, geom = "line", size = 1) +
  
  # Scales
  scale_color_manual(
    values = c("#bc3c29ff", "#0072b5ff", "#e18727ff", "#ee4c97ff", "#7876b1ff"), 
    labels = function(x) parse(text = x),
    name = "Class"
  ) +
  scale_fill_manual(
    values = c("#bc3c29ff", "#0072b5ff", "#e18727ff", "#ee4c97ff", "#7876b1ff"), 
    labels = function(x) parse(text = x),
    name = "Class"
  ) +
  
  # Axes and theme
  scale_x_continuous(breaks = unique(data$time), labels = unique(data$year_qtr)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  theme_minimal(base_size = 22) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "right") +
  
  # Labels and legend
  labs(
    title = "", 
    x = "Year-Quarter", 
    y = "Founded mold rate \n(per 1,000 households)"
  ) +
  guides(
    color = guide_legend(override.aes = list(fill = c("#bc3c29ff", "#0072b5ff", "#e18727ff", "#ee4c97ff", "#7876b1ff"))),
    fill = "none"
  )+ coord_fixed(ratio = .25) +
  geom_vline(xintercept = end_rollout_x , linetype = "dashed", color = "black") +
  geom_vline(xintercept = covid_x , linetype = "dashed", color = "black") +
  
  # Add labels for vertical lines at different positions
  annotate("text", x = end_rollout_x , y =50, label = "End Rollout", angle = 90, vjust = -0.2, hjust = -0.2, size = 5.2) +
  annotate("text", x = covid_x, y = 50, label = " COVID-19", angle = 90, vjust = -0.2, hjust = -0.2, size = 5.2)+
  theme(legend.position = "none")
 # theme(legend.position.inside  = c(.75,.95))

b





#c("#FED789FF", "#72874EFF", "#476F84FF", "#453947FF", "grey50")
#c("#F0e442", "#0072b2", "#d55e00", "#cc79a7", "#009e73")



b

a <- ggplot(data, aes(x = time, y = mold_rate, color = as.factor(class_labels), group = as.factor(class_labels))) +
  stat_summary(fun = median, geom = "line", size = 1) +
  scale_color_manual(
    values = c("#FED789FF", "#476F84FF", "#72874EFF", "#453947FF","grey50"), 
    labels = function(x) parse(text = x),
    name = "Class"
  ) +
  scale_x_continuous(
    breaks = unique(data$time), 
    labels = unique(data$year_qtr)
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "a. Median founded mold report", 
    x = "Year-Quarter", 
    y = "Founded mold rate \n(per 1,000 households)"
  ) +
  guides(
    color = guide_legend(override.aes = list(fill = c("#FED789FF", "#476F84FF", "#72874EFF", "#453947FF", "grey50")))
  ) 



print(b)

# Arrange with relative widths
combined_plot <-  a + b   

print(combined_plot)


setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/figures")

# Save the figure
ggsave("combined_figure-with-ci.pdf", b, width = 10, height = 8, dpi = 300)



# 

a <- print(lcgaMix_cb)


