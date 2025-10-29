# the purpose of this script is to run the time series analysis and check
# model diagnostics. 
# load packages -----------------------------------------------------------

require(sf)
require(dplyr)
require(tidyverse)
require(sp)
require(spacetime)
require(gstat)
require(ggplot2)
require(spdep)
require(INLA)
require(splines)
require(MASS)
require(purrr)
require(ggplot2)
require(DHARMa)
require(lme4)
require(glmmTMB)
require(splines2)
require(mgcv)

set.seed(555)

# load analysis objects ---------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/time_series")
analytical_dat <- readRDS("spatial_analytical_dat_frame.rds")%>%
  mutate(year = if_else(year == "2020", "2021", year))


mod <- glmmTMB(
  n_founded ~ 
    sw_ind_0 +
    sw_ind_1 +
    sw_ind_2 +
    sw_ind_3 +
    sw_ind_4 +
    sw_ind_5 +
    sw_ind_6 +
    windspeed_weekly_mean +
    is_holiday +
    year_month_MB_factor +
    ns(year_week_date, df = 9)  +
    scale(x) + 
    scale(y) +
    (1 | development_factor) +
    (1 | id),
  offset = household_offset,  
  family = nbinom2,  
  data = analytical_dat)

mod1 <- glmmTMB(
  n_founded ~ 
    sw_ind_0 +
    sw_ind_1 +
    sw_ind_2 +
    sw_ind_3 +
    sw_ind_4 +
    sw_ind_5 +
    sw_ind_6 +
    windspeed_weekly_mean +
    is_holiday +
    year_month_MB_factor +
    ns(year_week_date, df = 9)  +
    scale(x) + 
    scale(y) +
    (1 | development_factor) +
    (1 | id),
  offset = household_offset,  
  family = nbinom1,  
  data = analytical_dat)

summary(mod) # 185785.0 186102.
summary(mod1) # 185868.2 186186.0

# nbinom2 has a better aic and bic

# since we have unique id's by development - we can and should just use
# (1|dev) + (1|id) per documentation

summary(mod)


# check multicollinearity -------------------------------------------------

car::vif(mod)
# need to remove temperature. checked df again and time df is still best at 9 
# even with temperature removed.

# now that we have final predictors, see if including a mold busters
# group interaction improves model fit

mod_int <- glmmTMB(
  n_founded ~ 
    sw_ind_0 +
    sw_ind_1 +
    sw_ind_2 +
    sw_ind_3 +
    sw_ind_4 +
    sw_ind_5 +
    sw_ind_6 +
    windspeed_weekly_mean +
    is_holiday +
    year_month_MB_group + # simpler grouping (early/late) for interaction with time
    ns(year_week_date, df = 9) * year_month_MB_group +
    scale(x) + 
    scale(y) +
    (1 | development_factor) +
    (1| id),
  offset = household_offset,  # Ensure offset is in log scale
  family = nbinom2, 
  data = analytical_dat)



anova(mod, mod_int, test = "Chisq")
# LRT suggests the interaction model improves fit
summary(mod_int)

# dharma checks
simulationOutput <- DHARMa::simulateResiduals(fittedModel = mod_int, re.form = NULL)
plot(simulationOutput)
testUniformity(simulationOutput)
# qq plots and residuals look pretty good. 

#Check to see if there are any remaining patterns across time:
analytical_dat$resids <- simulationOutput$scaledResiduals
ggplot(analytical_dat %>% sample_n(30000), aes(x = year_week_date, y = resids)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) 

ggplot(analytical_dat, aes(x = year_week_date, y = resids)) +
  stat_summary(fun = "mean", geom = "line", color = "blue")  
# these look pretty good


# check dispersion --------------------------------------------------------
testDispersion(simulationOutput)
#No overdispersion to worry about

testZeroInflation(simulationOutput)
#No zero inflation to worry about

# assess remaining spatial autocorrelation --------------------------------
res <- simulationOutput
res2 <- recalculateResiduals(res, group = analytical_dat$development_factor)

testSpatialAutocorrelation(res2,
                           x = aggregate(analytical_dat$x, list(analytical_dat$development_factor), mean)$x,
                           y = aggregate(analytical_dat$y, list(analytical_dat$development_factor), mean)$x)

# slight autocorrelation
class(analytical_dat$y)
summary(mod)


mod_int <- glmmTMB(
  n_founded ~ 
    sw_ind_0 +
    sw_ind_1 +
    sw_ind_2 +
    sw_ind_3 +
    sw_ind_4 +
    sw_ind_5 +
    sw_ind_6 +
 #   ns(year_week_date, df = 9)  * may_2021 +
    windspeed_weekly_mean +
    is_holiday +
    year_month_MB_group +
    ns(year_week_date, df = 9)  * year_month_MB_group+
    (1 | consolidated_tds_number) +
    (1 | development_factor) +
    (1| id) +
    ns(scale(x), 10) + ns(scale(y), 10),
  offset = household_offset,
  family = nbinom2, 
  data = analytical_dat)

#167741.4 168313.5 without interaction for may 2021

# no longer significant autocorrelation
# observed = 0.0171265, expected = -0.0042735, sd = 0.0139843, p-value = 0.1259

summary(mod_int)


mod_int <- glmmTMB(
  n_founded ~ 
    sw_ind_0 +
    sw_ind_1 +
    sw_ind_2 +
    sw_ind_3 +
    sw_ind_4 +
    sw_ind_5 +
    sw_ind_6 +
    as.factor(year)/as.factor(season) +
    is_holiday +
    year_month_MB_group +
    as.factor(year)  * year_month_MB_group+
    (1 | development_factor) +
    (1| id) +
    ns(scale(x), 5) + ns(scale(y), 5),
  offset = household_offset,
  family = nbinom2, 
  data = analytical_dat)

summary(mod_int)



mod_int2 <- glmmTMB(
  n_founded ~ 
    sw_ind_0 +
    sw_ind_1 +
    sw_ind_2 +
    sw_ind_3 +
    sw_ind_4 +
    sw_ind_5 +
    sw_ind_6 +
    as.factor(year) +
    as.factor(month) +
    is_holiday +
    year_month_MB_group +
    as.factor(year)  * year_month_MB_group+
    (1 | development_factor) +
    (1| id) +
    ns(scale(x), 5) + ns(scale(y), 5),
  offset = household_offset,
  family = nbinom2, 
  data = analytical_dat)

summary(mod_int2)



# confirmed that the results are consistent when using year/season or year and month
# indicators



