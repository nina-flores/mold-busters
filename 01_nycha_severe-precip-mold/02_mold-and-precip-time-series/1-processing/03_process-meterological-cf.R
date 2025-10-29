### the objective of this script is to process the meteorological covariates
### by aggregating them to the correct weekly time scale and for merging


### load required packages
require(tidyverse)
require(lubridate)
require(dplyr)
require(data.table)
require(weathermetrics)
require(MMWRweek)
require(fst)

# Read in the data
setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/data/nldas-data-raw")
nldas <- fread("nldas-nycha.csv")
setDT(nldas)

# Split "system:index" column and drop the leading 'A' from date
nldas <- nldas[, c("date", "hour") := tstrsplit(`system:index`, "_", fixed = TRUE)[1:2]]
nldas <- nldas[, date := substr(date, 2, nchar(date))]
nldas <- nldas[, date := ymd(date)]

# Create datetime column in UTC
nldas <- nldas %>%
  mutate(datetime_utc = as.POSIXct(paste(date, hour), format = "%Y-%m-%d %H", tz = "UTC"),
# Convert to America/New_York time zone
  datetime_local = with_tz(datetime_utc, tzone = "America/New_York"),
# Extract local date
  date_local = as.Date(datetime_local))

##' Convert specific humidity to relative humidity
##'
##' converting specific humidity into relative humidity
##' NCEP surface flux data does not have RH
##' from Bolton 1980 The computation of Equivalent Potential Temperature 
##' \url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
##' @title qair2rh
##' @param qair specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass
##' @param temp degrees C
##' @param press pressure in mb
##' @return rh relative humidity, ratio of actual water mixing ratio to saturation mixing ratio
##' @export
##' @author David LeBauer
qair2rh <- function(qair, temp, press){
  es <-  6.112 * exp((17.67 * temp)/(temp + 243.5))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  return(rh)
}


## Second function to convert relative humidity to absolute humidity
#' Function to calculate absolute humidity from temperature and relative 
#' humidity. 
#' 
#' Absolute humidity is the amount of amount of water vapour in air, usually
#' expressed in \code{g.m-3}. 
#' 
#' @param air_temp Air temperature in degrees Celsius. 
#' @param rh Relative humidity in percentage. 
#' @return Numeric vector, absolute humidity in \code{g.m-3}.
#' @author Stuart K. Grange

absolute_humidity <- function(air_temp, rh) {
  # https://carnotcycle.wordpress.com/2012/08/04/how-to-convert-relative-humidity-to-absolute-humidity/
  (6.112 * exp((17.67 * air_temp) / (air_temp + 243.5)) * rh * 2.1674) / 
    (273.15 + air_temp)
}



radian2degree = 45/atan(1)

# Calculate hourly windspeed and relative humidity
nldas <- nldas %>%
  mutate(abs_wind_speed = sqrt((wind_u)^2 + (wind_v)^2),
         abs_wind_speed_knots = speed_to_knots(abs_wind_speed, unit = "mps", round = 3),
         wind_dir_met = atan2(wind_u, wind_v)*radian2degree + 180) %>% 
  mutate(wind_dir_met_cat = as.factor(case_when(
    wind_dir_met >= 337.5 | wind_dir_met < 22.5 ~ "North",
    wind_dir_met >= 22.5 & wind_dir_met < 67.5 ~ "North-east",
    wind_dir_met >= 67.5 & wind_dir_met < 112.5 ~ "East",
    wind_dir_met >= 112.5 & wind_dir_met < 157.5 ~ "South-east",
    wind_dir_met >= 157.5 & wind_dir_met < 202.5 ~ "South",
    wind_dir_met >= 202.5 & wind_dir_met < 247.5 ~ "South-west",
    wind_dir_met >= 247.5 & wind_dir_met < 292.5 ~ "West",
    wind_dir_met >= 292.5 & wind_dir_met < 337.5 ~ "North-west"
  )))

nldas <- nldas %>%
  mutate(pres_mb = .01 * pressure,
         rh = qair2rh(specific_humidity, temperature, pres_mb),
         rh_percentage = rh * 100,
         ah = absolute_humidity(temperature, rh_percentage))


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate daily average and mean for temperature, windspeed, and relative humidity (local time)
nldas_daily <- nldas %>%
  group_by(date_local, bldng_n, tds_nmb) %>%
  summarize(
    temp_daily_mean = mean(temperature, na.rm = TRUE),
    temp_daily_max = max(temperature, na.rm = TRUE),
    relative_humidity_daily_mean = mean(rh, na.rm = TRUE),
    relative_humidity_daily_max = max(rh, na.rm = TRUE),
    absolute_humidity_daily_mean = mean(ah, na.rm = TRUE),
    absolute_humidity_daily_max = max(ah, na.rm = TRUE),
    windspeed_daily_mean = mean(abs_wind_speed_knots, na.rm = TRUE),
    windspeed_daily_max = max(abs_wind_speed_knots, na.rm = TRUE),
    wind_dir_met_cat = Mode(wind_dir_met_cat)
  ) %>%
  ungroup()

# Add MMWRweek and year_week_date
nldas_weekly <- nldas_daily %>%
  mutate(MMWRweek(date_local),
         year_week_date = MMWRweek2Date(MMWRyear, MMWRweek, 1)
  )




# Group by and calculate weekly summaries
nldas_weekly <- nldas_weekly %>%
  group_by(bldng_n, tds_nmb, MMWRweek, MMWRyear, year_week_date) %>%
  mutate(temp_weekly_mean = mean(temp_daily_mean, na.rm = TRUE),
         temp_weekly_max = max(temp_daily_mean, na.rm = TRUE),
         relative_humidity_weekly_mean = mean(relative_humidity_daily_mean, na.rm = TRUE),
         relative_humidity_weekly_max = max(relative_humidity_daily_mean, na.rm = TRUE),
         absolute_humidity_weekly_mean = mean(absolute_humidity_daily_mean, na.rm = TRUE),
         absolute_humidity_weekly_max = max(absolute_humidity_daily_mean, na.rm = TRUE),
         windspeed_weekly_mean = mean(windspeed_daily_mean, na.rm = TRUE),
         windspeed_weekly_max = max(windspeed_daily_mean, na.rm = TRUE),
         wind_dir_met_cat_weekly = Mode(wind_dir_met_cat)
  ) %>%
  select(  bldng_n, tds_nmb,  MMWRweek, MMWRyear, year_week_date, temp_weekly_mean,
           temp_weekly_max, relative_humidity_weekly_mean, relative_humidity_weekly_max,
           windspeed_weekly_mean, windspeed_weekly_max, absolute_humidity_weekly_mean,
           absolute_humidity_weekly_max, wind_dir_met_cat_weekly) %>%
  unique()
  

### save the data 
setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/data/nldas-data-processed")

#### pre-period
#nldas_pre <- nldas_weekly %>%
#  filter(MMWRyear <= 2018)
#write.fst(nldas_pre, "nldas_pre.fst")

### post-period
nldas_post <- nldas_weekly %>%
  filter(MMWRyear >= 2020)
write.fst(nldas_post, "nldas_post.fst")


