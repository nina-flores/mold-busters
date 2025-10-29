library(dplyr)
library(tidyverse)

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/prism-monthly")

data <- read.csv("prism-nyc-ct-monthly.csv") %>%
  janitor::clean_names() %>%
  mutate(year = substr(system_index, 1,4),
         month = substr(system_index, 5,6))


data_annual <- data %>%
  group_by(geoid, year) %>%
  summarize(ppt = sum(ppt),
            tdmean = mean(tdmean),
            tmax_max = max(tmax),
            tmin_min = min(tmin)) %>%
  ungroup() %>%
  filter(year != 2025)


write.csv(data_annual, "prism_by_tract_2016_2024.csv", row.names = FALSE)



