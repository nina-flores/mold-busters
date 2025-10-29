# as a sensitivity analysis, we want to make sure that the 
# model is null if we assign exposure on random weeks

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
require(fst)
require(forestploter)
require(broom.mixed)
library(dplyr)
library(purrr)

set.seed(444)

# functions to extract model results --------------------------------------

process_glmmTMB_results <- function(model) {
  tidy_results <- tidy(model, effects = "fixed", conf.int = TRUE, exponentiate = FALSE)
  
  results <- tidy_results %>%
    rename(Variable = term, 
           Est = estimate,
           Lower = conf.low, 
           Upper = conf.high, 
           PValue = p.value) 
  
  return(results)
}



# update dataset accordingly ----------------------------------------------

# set working directory and read in data 

# anlaytical data
setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
analytical_dat <- read.fst("mold-met-data-post-mb.fst") %>%
  mutate(week = MMWRweek,
         year = MMWRyear)

actual_weeks <- analytical_dat %>%
  filter(week_95th_ind_precip == 1) %>%
  dplyr::select(year_week_date) %>%
  unique()


# Get unique year-weeks, excluding the weeks in actual_weeks
unique_weeks <- as.Date(setdiff(unique(analytical_dat$year_week_date), actual_weeks))

# Initialize a list to store the results
results <- list()

# Run the model 50 times
  for (i in 1:50) {
    temp_analytical_dat <- analytical_dat  # Preserve original data
    selected_year_weeks <- sample(unique_weeks, 25)
    temp_analytical_dat <- temp_analytical_dat %>%
      mutate(week_95th_ind_precip = if_else(year_week_date %in% selected_year_weeks, 1, 0))
    
    # Spatial data
    setwd("~/Desktop/projects/Mattlab/F31/data/NYCHA_buildings_shapefile")
    nycha_building_shape <- st_read("nycha_buildings_with_bins2.shp")
    nycha_building_shape_unique <- nycha_building_shape %>%
      group_by(tds_nmb, bldng_n) %>%
      dplyr::slice(1) %>%
      st_transform(crs = 32618) %>%
      mutate(geometry = st_centroid(geometry),
             tds_number = as.numeric(tds_nmb),
             building_number = as.character(bldng_n)) %>%
      mutate(building_number = if_else(building_number %in% c("4e", "4E"), "4.3", building_number),
             building_number = if_else(building_number %in% c("4w", "4W"), "4.7", building_number)) %>%
      mutate(building_number = as.numeric(building_number))



# make glm dataset --------------------------------------------------------

glm_analytical_dat <- temp_analytical_dat %>%
  rename(borough = geographical_borough) %>%
  dplyr::select(
    year_week_date, 
    MMWRyear, 
    MMWRweek,
    tds_number,
    building_number,
    n,
    n_founded,
    n_severe,
    n_repeat_reports,
    n_founded_no_severe,
    week_95th_ind_precip,
    total_households,
    temp_weekly_mean,
    windspeed_weekly_mean,
    relative_humidity_weekly_mean,
    absolute_humidity_weekly_mean,
    is_holiday,
    sum_week_precip,
    intervention_date,
    year_month_MB_factor,
    consolidated_tds_number,
    borough) %>%
  group_by(year_week_date, tds_number, building_number,MMWRyear, 
           MMWRweek) %>%
  mutate(
    consolidated_tds_number = as.factor(consolidated_tds_number),
    temp_weekly_mean = mean(temp_weekly_mean, na.rm = TRUE),
    windspeed_weekly_mean = mean(windspeed_weekly_mean, na.rm = TRUE),
    relative_humidity_weekly_mean = mean(relative_humidity_weekly_mean, na.rm = TRUE),
    n = sum(n, na.rm = TRUE),
    n_founded = sum(n_founded, na.rm = TRUE),
    n_severe = sum(n_severe, na.rm = TRUE),
    n_founded_no_severe = sum(n_founded_no_severe, na.rm = TRUE),
    n_founded_no_severe = if_else(n_founded_no_severe < 0, 0 ,n_founded_no_severe),
    total_households = total_households,
    week_95th_ind_precip = sum(week_95th_ind_precip, na.rm = TRUE),
    week_95th_ind_precip = if_else(week_95th_ind_precip > 1, 1, week_95th_ind_precip),
    is_holiday = sum(is_holiday, na.rm = T),
    is_holiday = if_else(is_holiday > 0, 1, 0)
  ) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(weeks_since_intervention = as.numeric(difftime(year_week_date, intervention_date, units = "weeks"))) %>%
  mutate(month = month(year_week_date),
         season = case_when(
           month %in% c(12, 1, 2) ~ "Winter",
           month %in% 3:5 ~ "Spring",
           month %in% 6:8 ~ "Summer",
           month %in% 9:11 ~ "Fall"
         )) %>%
  group_by(tds_number, building_number) %>%
  arrange(MMWRyear, 
          MMWRweek) %>%
  mutate(
    timepoint = 1,
    timepoint = cumsum(timepoint),
    period = 52
  ) %>%
  mutate(
    sw_ind_0 = week_95th_ind_precip,
    sw_ind_1 = lag(week_95th_ind_precip, 1),
    sw_ind_2 = lag(week_95th_ind_precip, 2),
    sw_ind_3 = lag(week_95th_ind_precip, 3),
    sw_ind_4 = lag(week_95th_ind_precip, 4),
    sw_ind_5 = lag(week_95th_ind_precip, 5),
    sw_ind_6 = lag(week_95th_ind_precip, 6),
    sw_ind_7 = lag(week_95th_ind_precip, 7),
    sw_ind_8 = lag(week_95th_ind_precip, 8),
    sum_week_precip_0 = sum_week_precip,
    sum_week_precip_1 = lag(sum_week_precip, 1),
    sum_week_precip_2 = lag(sum_week_precip, 2),
    sum_week_precip_3 = lag(sum_week_precip, 3),
    sum_week_precip_4 = lag(sum_week_precip, 4),
    sum_week_precip_5 = lag(sum_week_precip, 5),
    sum_week_precip_6 = lag(sum_week_precip, 6)
    
  ) %>%
  ungroup() %>%
  group_by(tds_number, building_number, MMWRyear, 
           MMWRweek) %>%
  mutate(multiple_6 = if_else(sw_ind_0 +
                                sw_ind_1 +
                                sw_ind_2 +
                                sw_ind_3 +
                                sw_ind_4 +
                                sw_ind_5 +
                                sw_ind_6 > 1, 1, 0))%>%
  mutate(multiple_8 = if_else(sw_ind_0 +
                                sw_ind_1 +
                                sw_ind_2 +
                                sw_ind_3 +
                                sw_ind_4 +
                                sw_ind_5 +
                                sw_ind_6 +
                                sw_ind_7+ 
                                sw_ind_8 > 1, 1,0 )) %>%
  ungroup()%>%
  mutate(
    cos_annual =
      cos(2 * 180 * timepoint / period),
    sin_annual =
      sin(2 * 180 * timepoint / period),
    timepoint_sq = timepoint ^ 2,
    timepoint_ctr_sd = scale(timepoint),
    timepoint_sq_ctr_sd = scale(timepoint_sq),
    founded_mold = n_founded,
    mold_report = n,
    severe_mold = n_severe,
    household_offset = log(total_households),
    year = as.character(MMWRyear),
    week = MMWRweek,
    development_factor = as.factor(tds_number),
    id = paste0(tds_number,"_",building_number)
  )  %>%
  arrange(year_week_date,
          tds_number,
          building_number) %>%
  ungroup()%>%
  drop_na(temp_weekly_mean)%>%
  drop_na(n_founded) %>%
  drop_na(household_offset) %>%
  drop_na(weeks_since_intervention) %>%
  drop_na(windspeed_weekly_mean) %>%
  drop_na(relative_humidity_weekly_mean)%>%
  drop_na(sw_ind_6)%>%
  unique()  %>%
  filter(year_month_MB_factor != "Unknown/privately managed") %>%
  mutate(any_founded = if_else(founded_mold >=1,1,0),
         any_report = if_else(mold_report >=1,1,0),
         any_severe = if_else(severe_mold >=1,1,0),
         any_non_severe = if_else(n_founded_no_severe>=1,1,0),
         multiple_founded = if_else(founded_mold >1,1,0),
         year_month_MB_group = if_else(year_month_MB_factor == "Jul 2018" | year_month_MB_factor == "Apr 2019", "early", "late"))




# make spatiotemporal object ----------------------------------------------

spatial_analytical_dat <-
  left_join(nycha_building_shape_unique, glm_analytical_dat) %>%
  arrange(year_week_date, tds_number, building_number) %>%
  drop_na(temp_weekly_mean)%>%
  drop_na(n_founded)

coordinates <- st_coordinates(spatial_analytical_dat)

# Add the coordinates to the data frame
spatial_analytical_dat$x <- coordinates[, 1]
spatial_analytical_dat$y <- coordinates[, 2]


spatial_analytical_dat_frame <- spatial_analytical_dat %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  arrange(year_week_date, tds_number, building_number)


# run model ---------------------------------------------------------------

model <- glmmTMB(
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
    year_month_MB_group +
    ns(year_week_date, df = 9)  * year_month_MB_group +
    (1 | consolidated_tds_number) +
    (1 | development_factor) +
    (1| id) +
    ns(scale(x), 10) + ns(scale(y), 10),
  offset = household_offset,
  family = nbinom2, 
  data = spatial_analytical_dat_frame)


# Process the results
results[[i]] <- process_glmmTMB_results(model)
}

# Combine the results into a single data frame
results_df <- bind_rows(results, .id = "run")


setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/figures/time-series")
write.csv(results_df, "sensitivity-results.csv", row.names = FALSE)


results_df <- read.csv("sensitivity-results.csv") 



aggregated_results <- results_df %>%
  filter(grepl("sw_ind_", Variable)) %>%
  rename(logRR = Est, SE = std.error) %>%
  mutate(var = SE^2) %>%
  group_by(Variable) %>%
  summarise(
    m = n(),
    logRR_mean = mean(logRR),            # Q̄
    U_bar = mean(var),                   # Within-imputation variance
    B = var(logRR),                      # Between-imputation variance
    .groups = "drop"
  ) %>%
  mutate(
    total_var = U_bar + (1 + 1/m) * B,           # Rubin's total variance
    SE_total = sqrt(total_var),
    Lower = exp(logRR_mean - 1.96 * SE_total),
    Upper = exp(logRR_mean + 1.96 * SE_total),
    RateRatio = exp(logRR_mean)
  ) %>%
  dplyr::select(Variable, RateRatio, Lower, Upper, m)



# 
# results_df_sw_0 <- results_df %>%
#   filter(grepl("sw_ind_0", Variable)) %>%
#   summarize(Variable = "sw_ind_0",
#             RateRatio = mean(RateRatio),
#             Lower = mean(Lower),
#             Upper = mean(Upper))
# 
# results_df_sw_1 <- results_df %>%
#   filter(grepl("sw_ind_1", Variable))%>%
#   summarize(Variable = "sw_ind_1",
#             RateRatio = mean(RateRatio),
#             Lower = mean(Lower),
#             Upper = mean(Upper))
# 
# results_df_sw_2 <- results_df %>%
#   filter(grepl("sw_ind_2", Variable))%>%
#   summarize(Variable = "sw_ind_2",
#             RateRatio = mean(RateRatio),
#             Lower = mean(Lower),
#             Upper = mean(Upper))
# 
# 
# results_df_sw_3 <- results_df %>%
#   filter(grepl("sw_ind_3", Variable))%>%
#   summarize(Variable = "sw_ind_3",
#             RateRatio = mean(RateRatio),
#             Lower = mean(Lower),
#             Upper = mean(Upper))
# 
# 
# results_df_sw_4 <- results_df %>%
#   filter(grepl("sw_ind_4", Variable))%>%
#   summarize(Variable = "sw_ind_4",
#             RateRatio = mean(RateRatio),
#             Lower = mean(Lower),
#             Upper = mean(Upper))
# 
# 
# results_df_sw_5 <- results_df %>%
#   filter(grepl("sw_ind_5", Variable))%>%
#   summarize(Variable = "sw_ind_5",
#             RateRatio = mean(RateRatio),
#             Lower = mean(Lower),
#             Upper = mean(Upper))
# 
# 
# results_df_sw_6 <- results_df %>%
#   filter(grepl("sw_ind_6", Variable))%>%
#   summarize(Variable = "sw_ind_6",
#             RateRatio = mean(RateRatio),
#             Lower = mean(Lower),
#             Upper = mean(Upper))
# 
# 
# 
# aggregated_results = rbind(results_df_sw_0,
#                            results_df_sw_1,
#                            results_df_sw_2,
#                            results_df_sw_3,
#                            results_df_sw_4,
#                            results_df_sw_5,
#                            results_df_sw_6)




aggregated_results_plot <- aggregated_results %>%
  mutate(`RR (95% CI)` = paste0(round(RateRatio,2)," (",round(Lower,2) ,", ",round(Upper,2),")")) %>%
  rename(`Weeks since extreme precipitation      ` = Variable)%>%
  mutate(`Weeks since extreme precipitation      ` = 
           case_when(`Weeks since extreme precipitation      ` == "sw_ind_0" ~ 0,
                     `Weeks since extreme precipitation      ` == "sw_ind_1" ~ 1,
                     `Weeks since extreme precipitation      ` == "sw_ind_2" ~ 2,
                     `Weeks since extreme precipitation      ` == "sw_ind_3" ~ 3,
                     `Weeks since extreme precipitation      ` == "sw_ind_4" ~ 4,
                     `Weeks since extreme precipitation      ` == "sw_ind_5" ~ 5,
                     `Weeks since extreme precipitation      ` == "sw_ind_6" ~ 6,
                     `Weeks since extreme precipitation      ` == "sw_ind_7" ~ 7,
                     `Weeks since extreme precipitation      ` == "sw_ind_8" ~ 8,
           )) %>%
  mutate(`                                                       ` = "")



p_null <- forest(aggregated_results_plot[,c(1, 6)],
                   est = aggregated_results_plot$RateRatio,
                   lower = aggregated_results_plot$Lower, 
                   upper = aggregated_results_plot$Upper,
                   ci_column = 1,
                   ref_line = 1,
                   arrow_lab = c("Fewer", "More founded mold"),
                   xlim = c(.8, 1.2),
                   ticks_at = c(0.9, 1,1.1))



setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/figures/time-series")

# Open a PDF device
pdf("null_model.pdf", width = 8, height = 6)  # Set the file name and dimensions

# Draw the gtable object (main_result) on the PDF device
p_null

# Close the PDF device
dev.off()

