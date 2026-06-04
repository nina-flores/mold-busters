library(ggplot2)
library(patchwork)
library(dplyr)
library(paletteer)
library(here)
library(fst)
library(tidyverse)
library(DRDID)
library(did)
library(MatchIt)


setwd(here("data", "did-analytical-datasets"))
# identify units that meet all thresholds in every year
keep_units <- read.csv("data_agegroup_ct.csv")%>%
  group_by(geo_group) %>%
  filter(all(prop_female >= .45),
         all(pop_young_old >= .5),
         all( building_age <= 95)) %>%
  pull(geo_group) %>%
  unique()

overall <- read.csv(here("data", "did-analytical-datasets","data_agegroup_ct.csv")) %>%
  filter(analytical_age_group == "all") %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population = 1000*(total_ed / denominator_age) ) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in% c(2019, 2020)) %>%
  mutate(borough = substr(geoid,1,5)) %>%
  filter(denominator_age !=0) %>%
  mutate(consolidated_tds_number = geo_group) %>%
  filter(geo_group %in% keep_units)
  

pre_period <- c(2016, 2017, 2018)
post_period <- c( 2022, 2023)


change <- overall %>% 
  group_by(consolidated_tds_number, MB) %>%
  summarize(pre = mean(visits_per_population[year %in% pre_period]),
            post = mean(visits_per_population[year %in% post_period])) %>%
  mutate(change = post-pre)

overall <- overall %>% left_join(change)

overall_pre <- overall %>%
  filter(year %in% pre_period) %>%
  group_by(consolidated_tds_number, MB) %>%
  summarize(pre = pre,
            building_age = mean(building_age),
            pop_young_old = mean(pop_young_old),
            ppt = mean(ppt),
            tdmean = mean(tdmean),
            annual_pm = mean(annual_pm),
            ndvi = mean(ndvi),
           nycha_neighb_250 = mean(nycha_neighb_250)) %>%
  ungroup() %>%
  unique()

match_model <- matchit(MB ~ pre +
                         building_age  +
                         pop_young_old +
                         ppt  +
                         tdmean +
                         annual_pm +
                         ndvi+ 
                        nycha_neighb_250,
                       data = overall_pre,
                       method = "nearest")

summary(match_model)

matched_data <- match.data(match_model)

matched_change <- change %>% semi_join(matched_data, by = "consolidated_tds_number")

mod <- lm(change ~ MB, data = matched_change) 


# Extract model summary as a tidy data frame
results <- coef(summary(mod)) %>%
  as.data.frame() %>%
  mutate(term = rownames(.)) %>%
  select(term, estimate = Estimate, std_error = `Std. Error`) %>%
  mutate(
    lower = estimate - 1.96 * std_error,
    upper = estimate + 1.96 * std_error
  ) %>%
  filter(term == "MB")


# Quick plot
b <- ggplot(results, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(y = "Estimate (95% CI)", x = "") +
  theme_bw()






