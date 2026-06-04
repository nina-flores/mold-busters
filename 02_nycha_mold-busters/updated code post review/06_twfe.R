library(ggplot2)
library(patchwork)
library(dplyr)
library(paletteer)
library(here)
library(fst)
library(tidyverse)
library(fixest)
library(ggfixest)

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
  filter(analytical_age_group == "all") %>%
  mutate(consolidated_tds_number = geo_group) %>%
  mutate(denominator = denominator_age) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population = 1000 *(total_ed / denominator)) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in% c(2019, 2020))  %>%
  filter(nycha_neighb_250 == 1) %>%
  mutate(borough = substr(geoid, 1, 5),
         borough = if_else(borough == "36085", "36047", borough)) %>%
  filter(geo_group %in% keep_units)

est_did <- feols(visits_per_population ~  building_age + ppt +  tdmean + 
                  pop_young_old + annual_pm + 
                  ndvi + i(year, MB, 2018)| consolidated_tds_number + year, data)

a <- ggiplot(est_did)

print(a)

setwd("C:/Users/nf2497/Desktop/mold-busters-project/figures")

combined <- a + b  # side by side by default

pdf("other-model-versions.pdf", width = 8, height = 4.5)  # wider for two plots
print(combined)
dev.off()



