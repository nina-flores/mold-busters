require(dplyr)
require(tidyverse)
require(tidyr)
library(readr)
library(dplyr) 
library(survival)
library(splines)
library(ggplot2)
library(lubridate)
require(lme4)
require(fst)

# load analysis objects ---------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/time_series")
analytical_dat <- readRDS("spatial_analytical_dat_frame.rds")

####********************************************
####  Choosing df for Confounders  ####
####********************************************
# we will first identify the best-fitting degrees of freedom for 
# the temperature-mold relationship 
# and then for WS
# and then for time
# Process for dta selection 
# 1: eliminate any dta that lead to models that are too wiggly to be plausible 
# 2: choose the dta that leads to the lowest aic. 
# 1: Temperature
# 1A Create the models

# temperature -------------------------------------------------------------


mod.lin <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    as.factor(season) +
    as.factor(MMWRyear) +
    temp_weekly_mean + 
    windspeed_weekly_mean+
    is_holiday +    
     
    (1| development_factor),   
  nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)


mod.ns.2 <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    as.factor(season) +
    as.factor(MMWRyear) +
    ns(temp_weekly_mean, 2) + 
    windspeed_weekly_mean+
    is_holiday +    
     
  (1| development_factor),   
    nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)

mod.ns.3 <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    as.factor(season) +
    as.factor(MMWRyear) +
    ns(temp_weekly_mean, 3) + 
    windspeed_weekly_mean+
    is_holiday +    
     
  (1| development_factor),   
    nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)

mod.ns.4 <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    as.factor(season) +
    as.factor(MMWRyear) +
    ns(temp_weekly_mean, 4) + 
    windspeed_weekly_mean+
    is_holiday +    
     
  (1| development_factor),   
    nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)

mod.ns.5 <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    as.factor(season) +
    as.factor(MMWRyear) +
    ns(temp_weekly_mean, 5) + 
    windspeed_weekly_mean+
    is_holiday +    
     
  (1| development_factor),   
    nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)

# 1B create predictions for each model

dta.pred <- analytical_dat

# Create a new data frame for prediction
new_data <- data.frame(
  temp_weekly_mean = seq(min(analytical_dat$temp_weekly_mean), max(analytical_dat$temp_weekly_mean), length.out = 100),
  sw_ind_0 = mean(analytical_dat$sw_ind_0),
  sw_ind_1 = mean(analytical_dat$sw_ind_1),
  sw_ind_2 = mean(analytical_dat$sw_ind_2),
  sw_ind_3 = mean(analytical_dat$sw_ind_3),
  sw_ind_4 = mean(analytical_dat$sw_ind_4),
  sw_ind_5 = mean(analytical_dat$sw_ind_5),
  sw_ind_6 = mean(analytical_dat$sw_ind_6),
  windspeed_weekly_mean = mean(analytical_dat$windspeed_weekly_mean),
  season = names(which.max(table(analytical_dat$season))),
  MMWRyear = names(which.max(table(analytical_dat$MMWRyear))),
  year_month_MB_factor = names(which.max(table(analytical_dat$year_month_MB_factor))),
  is_holiday = 0,
  year = 2022,
  household_offset = mean(analytical_dat$household_offset),
  development_factor = names(which.max(table(analytical_dat$development_factor)))
  
)

# Generate predicted values
new_data$founded_mold_pred_lin <- predict(mod.lin, newdata = new_data, type = "response")
new_data$founded_mold_pred_2 <- predict(mod.ns.2, newdata = new_data, type = "response")
new_data$founded_mold_pred_3 <- predict(mod.ns.3, newdata = new_data, type = "response")
new_data$founded_mold_pred_4 <- predict(mod.ns.4, newdata = new_data, type = "response")
new_data$founded_mold_pred_5 <- predict(mod.ns.5, newdata = new_data, type = "response")



# 1C Plot 
temp_nb <- ggplot(new_data, aes(temp_weekly_mean)) + 
  geom_line(aes(y = founded_mold_pred_lin), color = "springgreen3", size = 1) + 
  geom_line(aes(y = founded_mold_pred_2), color = "blue", size = 1) +
  geom_line(aes(y = founded_mold_pred_3), color = "red", size = 1) +
  geom_line(aes(y = founded_mold_pred_4), color = "lightsalmon4", size = 1) +
  geom_line(aes(y = founded_mold_pred_5), color = "black", size = 1) +
  xlab("Daily mean temperature")+
  ylab("Report of mold")

temp_nb

# 1D AIC

models_aic_temp_nb <- data.frame(
  ModelName = c("linear", "2 dta", "3 dta", "4 dta", "5 dta"),
  AIC = c(AIC(mod.lin), AIC(mod.ns.2), AIC(mod.ns.3), AIC(mod.ns.4), AIC(mod.ns.5)),
  stringsAsFactors = FALSE
)
models_aic_temp_nb$ModelName[which(models_aic_temp_nb$AIC == min(models_aic_temp_nb$AIC))]

# best is 3

# windspeed ---------------------------------------------------------------

mod.lin <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    as.factor(season) +
    as.factor(MMWRyear) +
    temp_weekly_mean + 
    windspeed_weekly_mean+
    is_holiday +    
     
    (1| development_factor),   
  nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)

mod.ns.2 <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    as.factor(season) +
    as.factor(MMWRyear) +
    temp_weekly_mean + 
    ns(windspeed_weekly_mean, 2)+
    is_holiday +    
     
    (1| development_factor),   
  nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)

mod.ns.3 <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    as.factor(season) +
    as.factor(MMWRyear) +
    temp_weekly_mean + 
    ns(windspeed_weekly_mean, 3)+
    is_holiday +    
     
    (1| development_factor),   
  nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)

mod.ns.4 <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    as.factor(season) +
    as.factor(MMWRyear) +
    temp_weekly_mean + 
    ns(windspeed_weekly_mean, 4)+
    is_holiday +    
     
    (1| development_factor),   
  nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)

mod.ns.5 <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    as.factor(season) +
    as.factor(MMWRyear) +
    temp_weekly_mean + 
    ns(windspeed_weekly_mean, 5)+
    is_holiday +    
     
    (1| development_factor),   
  nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)

# 1B create predictions for each model

dta.pred <- analytical_dat

# Create a new data frame for prediction
# Create a new data frame for prediction
new_data <- data.frame(
  windspeed_weekly_mean = seq(min(analytical_dat$windspeed_weekly_mean), max(analytical_dat$windspeed_weekly_mean), length.out = 100),
  sw_ind_0 = mean(analytical_dat$sw_ind_0),
  sw_ind_1 = mean(analytical_dat$sw_ind_1),
  sw_ind_2 = mean(analytical_dat$sw_ind_2),
  sw_ind_3 = mean(analytical_dat$sw_ind_3),
  sw_ind_4 = mean(analytical_dat$sw_ind_4),
  sw_ind_5 = mean(analytical_dat$sw_ind_5),
  sw_ind_6 = mean(analytical_dat$sw_ind_6),
  temp_weekly_mean = mean(analytical_dat$temp_weekly_mean),
  season = names(which.max(table(analytical_dat$season))),
  MMWRyear = names(which.max(table(analytical_dat$MMWRyear))),
  year_month_MB_factor = names(which.max(table(analytical_dat$year_month_MB_factor))),
  is_holiday = 0,
  year = 2022,
  household_offset = mean(analytical_dat$household_offset),
  development_factor = names(which.max(table(analytical_dat$development_factor)))
  
)

# Generate predicted values
new_data$founded_mold_pred_lin <- predict(mod.lin, newdata = new_data, type = "response")
new_data$founded_mold_pred_2 <- predict(mod.ns.2, newdata = new_data, type = "response")
new_data$founded_mold_pred_3 <- predict(mod.ns.3, newdata = new_data, type = "response")
new_data$founded_mold_pred_4 <- predict(mod.ns.4, newdata = new_data, type = "response")
new_data$founded_mold_pred_5 <- predict(mod.ns.5, newdata = new_data, type = "response")



# 1C Plot 
ws_nb <- ggplot(new_data, aes(windspeed_weekly_mean)) + 
  geom_line(aes(y = founded_mold_pred_lin), color = "springgreen3", size = 1) + 
  geom_line(aes(y = founded_mold_pred_2), color = "blue", size = 1) +
  geom_line(aes(y = founded_mold_pred_3), color = "red", size = 1) +
  geom_line(aes(y = founded_mold_pred_4), color = "lightsalmon4", size = 1) +
  geom_line(aes(y = founded_mold_pred_5), color = "black", size = 1) +
  xlab("Daily mean WS")+
  ylab("Report of mold")

ws_nb

# 1D AIC
models_aic_ws_nb <- data.frame(
  ModelName = c("linear", "2 dta", "3 dta", "4 dta", "5 dta"),
  AIC = c(AIC(mod.lin), AIC(mod.ns.2), AIC(mod.ns.3), AIC(mod.ns.4), AIC(mod.ns.5) ),
  stringsAsFactors = FALSE
)
models_aic_ws_nb$ModelName[which(models_aic_ws_nb$AIC == min(models_aic_ws_nb$AIC))]


# linear model is best here


# date instead ------------------------------------------------------------

mod.strata <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    as.factor(season) +
    as.factor(MMWRyear) +
  #  temp_weekly_mean + 
    windspeed_weekly_mean +
    is_holiday +    
     
    (1| development_factor),   
  nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)

mod.lin<- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    as.factor(season) +
    as.factor(MMWRyear) +
    year_week_date +
    windspeed_weekly_mean +
    is_holiday +    
     
    (1| development_factor),   
  nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)


mod.ns.2 <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    ns(year_week_date, 2) +
  #  temp_weekly_mean + 
    windspeed_weekly_mean +
    is_holiday +    
     
    (1| development_factor),   
  nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)

mod.ns.4 <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    ns(year_week_date, 4) +
   # temp_weekly_mean + 
    windspeed_weekly_mean +
    is_holiday +    
     
    (1| development_factor),   
  nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)

mod.ns.6 <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    ns(year_week_date, 6) +
   # temp_weekly_mean + 
    windspeed_weekly_mean +
    is_holiday +    
     
    (1| development_factor),   
  nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)


mod.ns.8 <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    ns(year_week_date, 8) +
  #  temp_weekly_mean + 
    windspeed_weekly_mean +
    is_holiday +    
     
    (1| development_factor),   
  nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)

mod.ns.9 <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    ns(year_week_date, 9) +
 #   temp_weekly_mean + 
    windspeed_weekly_mean +
    is_holiday +    
     
    (1| development_factor),   
  nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)



mod.ns.10 <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    ns(year_week_date, 10) +
 #   temp_weekly_mean + 
    windspeed_weekly_mean +
    is_holiday +    
     
    (1| development_factor),   
  nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)


mod.ns.12 <- glmer.nb(
  n_founded ~
    sw_ind_0+
    sw_ind_1+
    sw_ind_2+
    sw_ind_3+
    sw_ind_4+
    sw_ind_5+
    sw_ind_6+
    year_month_MB_factor +
    ns(year_week_date, 12) +
 #   temp_weekly_mean + 
    windspeed_weekly_mean +
    is_holiday +    
     
    (1| development_factor),   
  nAGQ=0,
  offset = household_offset,
  control=glmerControl(optimizer = "nloptwrap"),
  data = analytical_dat)

summary(mod.ns.12)

# 1B create predictions for each model

dta.pred <- analytical_dat

# Create a new data frame for prediction
new_data <- data.frame(
  year_week_date = seq(min(analytical_dat$year_week_date), max(analytical_dat$year_week_date), length.out = 100),
  sw_ind_0 = mean(analytical_dat$sw_ind_0),
  sw_ind_1 = mean(analytical_dat$sw_ind_1),
  sw_ind_2 = mean(analytical_dat$sw_ind_2),
  sw_ind_3 = mean(analytical_dat$sw_ind_3),
  sw_ind_4 = mean(analytical_dat$sw_ind_4),
  sw_ind_5 = mean(analytical_dat$sw_ind_5),
  sw_ind_6 = mean(analytical_dat$sw_ind_6),
  windspeed_weekly_mean = mean(analytical_dat$windspeed_weekly_mean),
 # temp_weekly_mean = mean(analytical_dat$temp_weekly_mean),
  weeks_since_intervention = mean(analytical_dat$weeks_since_intervention),
  is_holiday = 0,
  year = 2022,
  household_offset = mean(analytical_dat$household_offset),
  development_factor = names(which.max(table(analytical_dat$development_factor))),
  MMWRyear = names(which.max(table(analytical_dat$MMWRyear))),
  year_month_MB_factor = names(which.max(table(analytical_dat$year_month_MB_factor))),
  season = names(which.max(table(analytical_dat$season)))
)


# Generate predicted values
new_data$founded_mold_pred_strat <- predict(mod.strata, newdata = new_data, type = "response")
new_data$founded_mold_pred_lin <- predict(mod.lin, newdata = new_data, type = "response")
new_data$founded_mold_pred_2 <- predict(mod.ns.2, newdata = new_data, type = "response")
new_data$founded_mold_pred_4 <- predict(mod.ns.4, newdata = new_data, type = "response")
new_data$founded_mold_pred_6 <- predict(mod.ns.6, newdata = new_data, type = "response")
new_data$founded_mold_pred_8 <- predict(mod.ns.8, newdata = new_data, type = "response")
new_data$founded_mold_pred_9 <- predict(mod.ns.9, newdata = new_data, type = "response")
new_data$founded_mold_pred_10 <- predict(mod.ns.10, newdata = new_data, type = "response")
new_data$founded_mold_pred_12 <- predict(mod.ns.12, newdata = new_data, type = "response")





# 1C Plot 
time_nb <- ggplot(new_data, aes(year_week_date)) + 
  geom_line(aes(y = founded_mold_pred_strat), color = "pink", size = 1) + 
  geom_line(aes(y = founded_mold_pred_lin), color = "springgreen3", size = 1) + 
  geom_line(aes(y = founded_mold_pred_2), color = "blue", size = 1) +
  geom_line(aes(y = founded_mold_pred_4), color = "red", size = 1) +
  geom_line(aes(y = founded_mold_pred_6), color = "lightsalmon4", size = 1) +
  geom_line(aes(y = founded_mold_pred_8), color = "black", size = 1) +
  geom_line(aes(y = founded_mold_pred_9), color = "yellow", size = 1) +
  geom_line(aes(y = founded_mold_pred_10), color = "orange", size = 1) +
  geom_line(aes(y = founded_mold_pred_12), color = "purple", size = 1) +
  xlab("Time since intervention")+
  ylab("Report of mold")

time_nb

# 1D AIC


models_aic_time_nb <- data.frame(
  ModelName = c("strata","linear", "2 dta", "4 dta", "6 dta", "8 dta", "9 dta","10 dta", "12 dta"),
  AIC = c(AIC(mod.strata), AIC(mod.lin), AIC(mod.ns.2), AIC(mod.ns.4), AIC(mod.ns.6), AIC(mod.ns.8), AIC(mod.ns.9), AIC(mod.ns.10), AIC(mod.ns.12)),
  stringsAsFactors = FALSE
)
models_aic_time_nb$ModelName[which(models_aic_time_nb$AIC == min(models_aic_time_nb$AIC))]

# 9 df with the spline is selected as best, beyond that you only get incremental improvements
# and the plot looks super wobbly. Fit is actually worse increasing from 9 to 10 df
