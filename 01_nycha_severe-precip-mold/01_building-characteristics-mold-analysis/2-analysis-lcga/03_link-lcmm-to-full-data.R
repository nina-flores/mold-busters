# here we plot any repeat report over the entire time period.

library(fst)
library(lme4)
library(dplyr)
library(ggplot2)
library(broom.mixed)
library(patchwork)
library(gtsummary)
library(ggrepel)
library(geomtextpath)
library(lubridate)
library(tidyverse)

# Set working directory
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")
lcga_classes <- read.fst("classes_lcga.fst") %>%
  mutate(tds_number = as.numeric(as.character(tds_number)),
         year = as.numeric(year),
         qtr = as.numeric(qtr)) %>%
  select(class, tds_number)

setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
dta_hh <- read.fst("full_households.fst") %>%
  mutate(tds_number = as.numeric(as.character(tds_number)),
         year = as.numeric(year))


x <- read.fst("mold-through-2023.fst") %>%
  janitor::clean_names() %>%
  filter(failure_code == "Mold Condition") %>%
  filter(category == "Mold Busters")

sum(x$founded_flag)

summary(x$zz_sq_feet)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 #  0.00    0.00   10.00   20.57   30.00  999.00    9738 

# read in the dta --------------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
dta <- read.fst("mold-through-2023.fst") %>%
  janitor::clean_names() %>%
  filter(failure_code == "Mold Condition") %>%
  filter(category == "Mold Busters" | category == "Pre-Mold Busters") %>%
  filter(zz_is_apt == 1) %>%
  mutate(
    datetime_nyc = as.POSIXct(zzcreate_date, format = "%m/%d/%y %H:%M", tz = "America/New_York"),
    report_date_nyc = as.Date(datetime_nyc, format = "%m/%d/%y"),
    qtr = quarter(report_date_nyc ),
    report_date_year_nyc = year(report_date_nyc),
  ) %>%
  arrange(report_date_nyc) %>%
  filter( report_date_year_nyc < 2024) 

sum(dta$founded_flag)

n_distinct(dta$tds_number, dta$building_number)

# overall reports ---------------------------------------------------------
# identify repeat reports for each tds-quarter
dta_reports <- dta %>%
  group_by(tds_number, building_number, wo_location, report_date_nyc) %>%
  slice(1) %>%  # Remove duplicates within the same wo_location-date
  ungroup() %>%
  group_by(tds_number, building_number, wo_location) %>%
  arrange(report_date_nyc) %>%
  mutate(
    n_any = n(),
    repeat_flag = if_else(
      lag(report_date_nyc, 1) %within% interval(report_date_nyc - dyears(1), report_date_nyc - 1),
      1,
      0
    ), 
    
    repeat_flag_6 = if_else(
      lag(report_date_nyc, 1) %within% interval(report_date_nyc - dmonths(6), report_date_nyc - 1),
      1,
      0
    )
  ) %>%
  ungroup() %>%
  group_by(tds_number, report_date_year_nyc, qtr) %>%
  summarize(
    total_reports = sum(n_any, na.rm = TRUE), 
    total_repeats_6 = sum(repeat_flag_6, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(year = report_date_year_nyc) %>%
  mutate(tds_number = as.numeric(tds_number),
         year = as.numeric(year),
         qtr = as.numeric(qtr))




# Create expanded building-year combinations
expanded_combinations <- expand_grid(
  tds_number = unique(lcga_classes$tds_number),
  year = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),
  qtr = c(1:4))  %>%
  mutate(tds_number = as.numeric(tds_number),
         year = as.numeric(year),
         qtr = as.numeric(qtr))


dta_reports_exp <- expanded_combinations %>%
  left_join(dta_reports, by = c("tds_number", "year", "qtr")) %>%
  replace_na(list(
    total_repeats_6 = 0,
    total_reports = 0)) %>% 
  left_join(dta_hh, by = c("tds_number", "year")) %>%
  mutate(mold_rate = (total_reports/total_households)*1000) 
  

dta <- dta_reports_exp %>%
  left_join(lcga_classes, by = c("tds_number")) %>%
  unique() %>%
  arrange(year) %>%
  group_by(tds_number) %>%
  mutate(time = row_number()) %>%
  ungroup()

dta %>%
  dplyr::select(tds_number) %>%
  n_distinct()

# plot the trajectories ---------------------------------------------------

dta$year_qtr <- paste0(dta$year, " Q", dta$qtr)


# Create a new variable for year-quarter
dta <- dta %>%
  mutate(class_letter = case_when(
    
    class == 1 ~ "A",
    class == 2 ~ "B",
    class == 3 ~ "C"
    
    
  )) 


# Define specific x-axis locations for the lines
pilot_x <- 7  # Replace with actual x-axis value
begin_rollout_x <- 14  # Replace with actual x-axis value
end_rollout_x <- 16  # Replace with actual x-axis value

a <- ggplot(dta, aes(x = time, y = mold_rate, color = as.factor(class_letter), fill = as.factor(class_letter))) +
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
  stat_summary(
    fun = median,
    geom = "line",
    size = 1
  ) +
  scale_color_manual(values = c( "#FED789FF",  "#476F84FF","#72874EFF"), 
                     labels = function(x) parse(text = x),
                     name = "Class") +
  scale_fill_manual(values = c( "#FED789FF",  "#476F84FF","#72874EFF"), 
                    labels = function(x) parse(text = x),
                    name = "Class") +
  scale_x_continuous(breaks = unique(dta$time), labels = unique(dta$year_qtr)) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "b. Mold report", x = "Year-Quarter", y = "Mold report rate\n (per 1,000 households)", color = "Class", fill = "Class") +
  theme(legend.position = "none") +
  
  # Add vertical lines at specific x-axis locations
 # geom_vline(xintercept = pilot_x, linetype = "dashed", color = "black") +
  geom_vline(xintercept = begin_rollout_x, linetype = "dashed", color = "black") +
  geom_vline(xintercept = end_rollout_x, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 17, linetype = "dashed", color = "blue") +
  
  
  # Add labels for vertical lines at different positions
 # annotate("text", x = pilot_x, y = 28, label = "   Pilot", angle = 90, vjust = -0.5, hjust = -0.2) +
  annotate("text", x = begin_rollout_x, y =350, label = "Begin Rollout", angle = 90, vjust = -0.5, hjust = -0.2) +
  annotate("text", x = end_rollout_x, y = 350, label = " End Rollout", angle = 90, vjust = -0.5, hjust = -0.2) +
  annotate("text", x = 17, y = 350, label = " COVID-19", angle = 90, vjust = 1.5, hjust = -0.2)


print(a)





# Define specific x-axis locations for the lines
pilot_x <- 7  # Replace with actual x-axis value
begin_rollout_x <- 10  # Replace with actual x-axis value
end_rollout_x <- 12  # Replace with actual x-axis value

c <- ggplot(dta, aes(x = time, y = n_per_hh, color = as.factor(class_letter), fill = as.factor(class_letter))) +
  stat_summary(fun = median, geom = "line", size = 1) +
  scale_color_manual(values = c( "#FED789FF",  "#476F84FF"), 
                     labels = function(x) parse(text = x),
                     name = "Class") +
  scale_fill_manual(values = c( "#FED789FF",  "#476F84FF"), 
                    labels = function(x) parse(text = x),
                    name = "Class") +
  scale_x_continuous(breaks = unique(dta$time), labels = unique(dta$year_qtr)) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "b. Mold report", x = "Year-Quarter", y = "Mold report rate\n (per 1,000 households)", color = "Class", fill = "Class") +
  theme(legend.position = "none") +
  
  # Add vertical lines at specific x-axis locations
  # geom_vline(xintercept = pilot_x, linetype = "dashed", color = "black") +
  geom_vline(xintercept = begin_rollout_x, linetype = "dashed", color = "black") +
  geom_vline(xintercept = end_rollout_x, linetype = "dashed", color = "black") +
  
  # Add labels for vertical lines at different positions
  # annotate("text", x = pilot_x, y = 28, label = "   Pilot", angle = 90, vjust = -0.5, hjust = -0.2) +
  annotate("text", x = begin_rollout_x, y =28, label = "Begin Rollout", angle = 90, vjust = -0.5, hjust = -0.2) +
  annotate("text", x = end_rollout_x, y = 28, label = " End Rollout", angle = 90, vjust = -0.5, hjust = -0.2)

print(c)





