library(ggplot2)
library(patchwork)
library(dplyr)
library(paletteer)
library(here)
library(fst)
library(tidyverse)
library(DRDID)
library(did)


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
  mutate(denominator = denominator_age) %>%
  mutate(MB = if_else(group == "nycha", 1, 0),
         visits_per_population = 1000 *(total_ed / denominator),
         visits_non_asthma_per_pop = 1000 *(total_ed_non_asthma / denominator)) %>%
  mutate(first.treat = if_else(group == "nycha", 2019, 0)) %>%
  filter(!year %in% c(2019, 2020)) %>%
  filter(nycha_neighb_250 == 1) %>%
  mutate(borough = substr(geoid, 1, 5),
         borough = if_else(borough == "36085", "36047", borough)) %>%
  mutate(consolidated_tds_number = geo_group)

data_overall <- data %>%
  filter(analytical_age_group == "all")

ps_data <- data_overall %>%
  filter(geo_group %in% keep_units) %>%
  filter(year == 2018, first.treat %in% c(2019, 0)) %>%
  mutate(D = as.integer(first.treat == 2019)) %>%
  select(D, building_age, ppt, pop_young_old, tdmean, annual_pm, ndvi, borough, prop_female) %>%
  tidyr::drop_na() 
 

ps_mod <- glm(D ~  building_age +  ppt + pop_young_old + tdmean + annual_pm +borough + ndvi,
              data = ps_data, family = binomial(link = "logit"))

summary(ps_mod)

ps_data %>%
  group_by(D) %>%
  summarise(
    ps_min   = min(ndvi),
    ps_p25   = quantile(ndvi, 0.25),
    ps_p50   = median(ndvi),
    ps_p75   = quantile(ndvi, 0.75),
    ps_max   = max(ndvi)
  )

ps_data %>%
  group_by(D) %>%
  summarise(
    ps_min   = min(building_age),
    ps_p25   = quantile(building_age, 0.25),
    ps_p50   = median(building_age),
    ps_p75   = quantile(building_age, 0.75),
    ps_max   = max(building_age)
  )

ps_data %>%
  group_by(D) %>%
  summarise(
    ps_min   = min(pop_young_old),
    ps_p25   = quantile(pop_young_old, 0.25),
    ps_p50   = median(pop_young_old),
    ps_p75   = quantile(pop_young_old, 0.75),
    ps_max   = max(pop_young_old)
  )


ps_data %>%
  group_by(D) %>%
  summarise(
    ps_min   = min(prop_female),
    ps_p25   = quantile(prop_female, 0.25),
    ps_p50   = median(prop_female),
    ps_p75   = quantile(prop_female, 0.75),
    ps_max   = max(prop_female)
  )

# Updated: use predict() with type = "response" to explicitly apply the
# inverse logit link and get P(D=1 | X) on the [0, 1] probability scale
ps_data$ps_treat <- predict(ps_mod, newdata = ps_data, type = "response")
ps_data$ps_ctrl  <- 1 - ps_data$ps_treat

# View distributions by treatment group
ps_data %>%
  group_by(D) %>%
  summarise(
    n        = n(),
    ps_mean  = mean(ps_treat),
    ps_min   = min(ps_treat),
    ps_p05   = quantile(ps_treat, 0.05),
    ps_p10   = quantile(ps_treat, 0.10),
    ps_p25   = quantile(ps_treat, 0.25),
    ps_p50   = median(ps_treat),
    ps_p75   = quantile(ps_treat, 0.75),
    ps_max   = max(ps_treat)
  )

treated_rows     <- ps_data$D == 1
not_treated_rows <- ps_data$D == 0

treated_units <- ps_data %>%
  filter(D == 1)

control_units <- ps_data %>%
  filter(D == 0)

hist(treated_units$building_age)
hist(control_units$building_age)

hist(treated_units$ndvi)
hist(control_units$ndvi)

hist(treated_units$pop_young_old)
hist(control_units$pop_young_old)

hist(treated_units$prop_female)
hist(control_units$prop_female)

# P(D=1 | X) among treated units
ps_treat_among_treated     <- ps_data$ps_treat[treated_rows]

# P(D=0 | X) among treated units
ps_ctrl_among_treated      <- ps_data$ps_ctrl[treated_rows]

# P(D=1 | X) among not-treated units
ps_treat_among_not_treated <- ps_data$ps_treat[not_treated_rows]

# P(D=0 | X) among not-treated units
ps_ctrl_among_not_treated  <- ps_data$ps_ctrl[not_treated_rows]

# Summarise all four
summary(ps_treat_among_treated)
summary(ps_ctrl_among_treated)
summary(ps_treat_among_not_treated)
summary(ps_ctrl_among_not_treated)


# Propensity score quantiles for all units
ps_all <- ps_data$ps_treat

# P(Trt=1) quantiles for all units
quantile(ps_all, probs = c(0, 0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1))

# P(Trt=0) quantiles for all units
quantile(1 - ps_all, probs = c(0, 0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1))



# everything below is with the pre-trimmed data ---------------------------
# ask why the trimmer below is only concerned with higher ends


xvars <- c("building_age", "ppt", "pop_young_old", "tdmean", "annual_pm", "ndvi", "borough")

# now run the same thing after applying a trim: 
a <- did::trimmer(g  = 2019,
  tname         = "year",
  idname        = "consolidated_tds_number",
  gname         = "first.treat",
  xformla       = as.formula(paste("~", paste(xvars, collapse = " + "))),
  data          = data_overall,
  control_group = "nevertreated",
  threshold     = 0.95
)

data_trimmed <- data_overall %>%
  filter(!consolidated_tds_number %in% a)




ps_data_trimmed <- data_trimmed %>%
  filter(year == 2018, first.treat %in% c(2019, 0)) %>%
  mutate(D = as.integer(first.treat == 2019)) %>%
  select(D, building_age, ppt, pop_young_old, tdmean, annual_pm, ndvi, borough) %>%
  tidyr::drop_na()

ps_mod_trimmed <- glm(D ~ building_age +ppt+ pop_young_old + tdmean + annual_pm + ndvi+borough,
              data = ps_data_trimmed, family = binomial(link = "logit"))

# Updated: use predict() with type = "response" to explicitly apply the
# inverse logit link and get P(D=1 | X) on the [0, 1] probability scale
ps_data_trimmed$ps_treat <- predict(ps_mod_trimmed, newdata = ps_data_trimmed, type = "response")
ps_data_trimmed$ps_ctrl  <- 1 - ps_data_trimmed$ps_treat

# View distributions by treatment group
a <- ps_data_trimmed %>%
  group_by(D) %>%
  summarise(
    n        = n(),
    ps_mean  = mean(ps_ctrl),
    ps_min   = min(ps_ctrl),
    ps_p01   = quantile(ps_ctrl, 0.01),
    ps_p05   = quantile(ps_ctrl, 0.05),
    ps_p15   = quantile(ps_ctrl, 0.15),
    ps_p10   = quantile(ps_ctrl, 0.10),
    ps_p25   = quantile(ps_ctrl, 0.25),
    ps_p50   = median(ps_ctrl),
    ps_p75   = quantile(ps_ctrl, 0.75),
    ps_p90   = quantile(ps_ctrl, 0.90),
    ps_p95   = quantile(ps_ctrl, 0.95),
    ps_p99   = quantile(ps_ctrl, 0.99),
    ps_max   = max(ps_ctrl)
  )

write.csv(a, "ps_ctrl.csv")

b <- ps_data_trimmed %>%
  group_by(D) %>%
  summarise(
    n        = n(),
    ps_mean  = mean(ps_treat),
    ps_min   = min(ps_treat),
    ps_p01   = quantile(ps_treat, 0.01),
    ps_p05   = quantile(ps_treat, 0.05),
    ps_p10   = quantile(ps_treat, 0.10),
    ps_p15   = quantile(ps_treat, 0.15),
    ps_p25   = quantile(ps_treat, 0.25),
    ps_p50   = median(ps_treat),
    ps_p75   = quantile(ps_treat, 0.75),
    ps_p90   = quantile(ps_treat, 0.90),
    ps_p95   = quantile(ps_treat, 0.95),
    ps_p99   = quantile(ps_treat, 0.99),
    ps_max   = max(ps_treat)
  )

write.csv(b, "ps_treat.csv")


treated_rows     <- ps_data_trimmed$D == 1
not_treated_rows <- ps_data_trimmed$D == 0

# P(D=1 | X) among treated units
ps_treat_among_treated_trimmed     <- ps_data_trimmed$ps_treat[treated_rows]

# P(D=0 | X) among treated units
ps_ctrl_among_treated_trimmed       <- ps_data_trimmed$ps_ctrl[treated_rows]

# P(D=1 | X) among not-treated units
ps_treat_among_not_treated_trimmed  <- ps_data_trimmed$ps_treat[not_treated_rows]

# P(D=0 | X) among not-treated units
ps_ctrl_among_not_treated_trimmed   <- ps_data_trimmed$ps_ctrl[not_treated_rows]

# Summarise all four
summary(ps_treat_among_treated_trimmed )
summary(ps_ctrl_among_treated_trimmed )
summary(ps_treat_among_not_treated_trimmed )
summary(ps_ctrl_among_not_treated_trimmed )




