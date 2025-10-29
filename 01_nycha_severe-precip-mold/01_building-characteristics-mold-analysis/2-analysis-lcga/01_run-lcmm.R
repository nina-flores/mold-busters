# Here we run a series of LCGA models to identify if its better to run the 
# model linearly or nonlinearly and to identify the appropraite number of 
# classes. We use the flexmix package and use the negative binomial family 
# since the data is overdispersed. 

library(dplyr)
library(tidyverse)
library(flexmix)
library(countreg)
library(splines)
library(fst)


# Save processed datasets
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")
analytical_dat <- read.fst( "analytical_data_clean.fst") %>%
  filter(year != 2024) %>%
  filter(!(year == 2019 & qtr ==1)) %>%
  filter(!(year == 2019 & qtr ==2)) %>%
  filter(!(year == 2019 & qtr ==3)) %>%
  mutate(year_MB = year(intervention_date)) %>%
  filter(year_MB != 2018) %>%
  dplyr::select(tds_number, total_severe_reports, total_founded_reports, year, qtr, total_households, consolidated_name) %>%
  mutate(founded_mold_rate = 1000*(total_founded_reports/(total_households))) %>%
  group_by(tds_number) %>% 
  arrange(year, qtr) %>%
  mutate(time = row_number(),
         qtr = as.factor(qtr)) %>%
  mutate(tds_number = as.factor(tds_number))

class(analytical_dat$tds_number)
class(analytical_dat$qtr)



a <- analytical_dat %>%
  group_by(year) %>%
  summarize(s = sum(total_founded_reports))

# 2021 - 13300
# 2022 - 8520
# 2023 - 7634

#(13300 - 7634) / 13300 = 42.6% reduction 

sum(analytical_dat$total_founded_reports)
n_distinct(analytical_dat)

hist(analytical_dat$founded_mold_rate)
mean(analytical_dat$founded_mold_rate)
var(analytical_dat$founded_mold_rate)
# overdispersed, the variance is much greater than the mean


summary(analytical_dat$total_founded_reports)
table(is.na(analytical_dat$total_founded_reports))
table(analytical_dat$total_founded_reports < 0)

unique(analytical_dat$total_founded_reports)

# Estimate a global theta first (from a glm.nb)
#theta_est <- glm.nb(total_founded_reports ~ time + offset(log(total_households)), data = analytical_dat)$theta

# set.seed(1)
# 
# 
# lcgaMix_lin2 <- stepFlexmix(
#   total_founded_reports ~ time | tds_number,  
#   k = 1:5,   
#   nrep = 100,  
#   model = FLXMRnegbin(theta = theta_est),  
#   data = analytical_dat,
#   control = list(iter.max = 500, minprior = 0)
# )

set.seed(1)
# # Include offset properly using the offset argument
 lcgaMix_lin2 <- stepFlexmix(
   total_founded_reports ~ time | tds_number,  
   k = 1:6,   
   nrep = 100,  
   model = FLXMRnegbin( offset = log(analytical_dat$total_households)),  
   data = analytical_dat,
   control = list(iter.max = 500, minprior = 0)
 )

analytical_dat$time_sq = (analytical_dat$time)^2
analytical_dat$time_cub = (analytical_dat$time)^3

# # Inspect and compare model-fit results
 print(lcgaMix_lin2)
 
  set.seed(1)
  lcgaMix_sq <- stepFlexmix(
    total_founded_reports ~ time + time_sq | tds_number,  
    k = 1:6,   
    nrep = 100,  
    model = FLXMRnegbin(offset = log(analytical_dat$total_households)),  
    data = analytical_dat,
    control = list(iter.max = 500, minprior = 0)
  )
  
  summary( lcgaMix_sq)
  print(lcgaMix_sq)
 
 set.seed(1)
 lcgaMix_cb_4 <- stepFlexmix(
   total_founded_reports ~ time +time_sq+ time_cub | tds_number,  
   k = 4,   
   nrep = 100,  
   model = FLXMRnegbin(offset = log(analytical_dat$total_households)),  
   data = analytical_dat,
   control = list(iter.max = 500, minprior = 0)
 )
 
 summary( lcgaMix_cb_4)
 print(lcgaMix_cb_4)
 
 
 lcgaMix_cb_5 <- stepFlexmix(
   total_founded_reports ~ time +time_sq+ time_cub | tds_number,  
   k = 5,   
   nrep = 100,  
   model = FLXMRnegbin(offset = log(analytical_dat$total_households)),  
   data = analytical_dat,
   control = list(iter.max = 500, minprior = 0)
 )
 
 summary( lcgaMix_cb_5)
 print(lcgaMix_cb_5)
 
 
 # set.seed(1)
 # lcgaMix_ns9_4 <- stepFlexmix(
 #   total_founded_reports ~ ns(time, 9) | tds_number,  
 #   k = 4,   
 #   nrep = 100,  
 #   model = FLXMRnegbin(offset = log(analytical_dat$total_households)),  
 #   data = analytical_dat,
 #   control = list(iter.max = 500, minprior = 0)
 # )
 # 
 # summary( lcgaMix_ns9_4)
 # print(lcgaMix_ns9_4)
 # 
 # 
 set.seed(1)
 lcgaMix_ns9 <- stepFlexmix(
   total_founded_reports ~ ns(time, 9) | tds_number,
   k = 4:5,
   nrep = 100,
   model = FLXMRnegbin(offset = log(analytical_dat$total_households)),
   data = analytical_dat,
   control = list(iter.max = 500, minprior = 0)
 )

 summary( lcgaMix_ns9)
 print(lcgaMix_ns9)
 
 
 set.seed(1)
 lcgaMix_ns_season2 <- stepFlexmix(
   total_founded_reports ~ ns(time, 2) + qtr| tds_number,
   k = 5,
   nrep = 100,
   model = FLXMRnegbin(offset = log(analytical_dat$total_households)),
   data = analytical_dat,
   control = list(iter.max = 500, minprior = 0)
 )
 
 set.seed(1)
 lcgaMix_ns_season3 <- stepFlexmix(
   total_founded_reports ~ ns(time, 3) + qtr| tds_number,
   k = 5,
   nrep = 100,
   model = FLXMRnegbin(offset = log(analytical_dat$total_households)),
   data = analytical_dat,
   control = list(iter.max = 500, minprior = 0)
 )
 
 set.seed(1)
 lcgaMix_ns_season4 <- stepFlexmix(
   total_founded_reports ~ ns(time, 4) + qtr| tds_number,
   k = 5,
   nrep = 100,
   model = FLXMRnegbin(offset = log(analytical_dat$total_households)),
   data = analytical_dat,
   control = list(iter.max = 500, minprior = 0)
 )
 
 set.seed(1)
 lcgaMix_ns_season5 <- stepFlexmix(
   total_founded_reports ~ ns(time, 5) + qtr| tds_number,
   k = 5,
   nrep = 100,
   model = FLXMRnegbin(offset = log(analytical_dat$total_households)),
   data = analytical_dat,
   control = list(iter.max = 500, minprior = 0)
 )
summary(lcgaMix_ns_season2 ) # 'log Lik.' -9823.133 (df=39) AIC: 19724.27   BIC: 19967.11 
summary(lcgaMix_ns_season3 ) # 'log Lik.' -9759.252 (df=44) AIC: 19606.5   BIC: 19880.48 * best model
summary(lcgaMix_ns_season4 ) # 'log Lik.' -9752.026 (df=49) AIC: 19602.05   BIC: 19907.17 
summary(lcgaMix_ns_season5 ) # 'log Lik.' -9751.341 (df=54) AIC: 19610.68   BIC: 19946.93 
 



set.seed(1)
lcgaMix_ns_season3 <- stepFlexmix(
  total_founded_reports ~ ns(time, 3) + qtr| tds_number,
  k = 1:5,
  nrep = 100,
  model = FLXMRnegbin(offset = log(analytical_dat$total_households)),
  data = analytical_dat,
  control = list(iter.max = 500, minprior = 0)
)
 
summary( lcgaMix_ns_season3)
print(lcgaMix_ns_season3) 

# Call:
#   stepFlexmix(total_founded_reports ~ ns(time, 3) + qtr | tds_number, model = FLXMRnegbin(offset = log(analytical_dat$total_households)), 
#               data = analytical_dat, control = list(iter.max = 500, minprior = 0), k = 1:6, nrep = 100)
# 
# iter converged k k0     logLik      AIC      BIC      ICL
# 1    3      TRUE 1  1 -10528.903 21073.81 21123.62 21123.62
# 2   14      TRUE 2  2 -10055.600 20145.20 20251.06 20268.62
# 3   18      TRUE 3  3  -9879.665 19811.33 19973.23 20009.75
# 4   25      TRUE 4  4  -9803.547 19677.09 19895.03 19949.31
# 5   20      TRUE 5  5  -9759.252 19606.50 19880.48 19963.30
# did not converge 


 # Call:
 #   stepFlexmix(total_founded_reports ~ time | tds_number, model = FLXMRnegbin(offset = log(analytical_dat$total_households)), 
 #               data = analytical_dat, control = list(iter.max = 500, minprior = 0), k = 1:6, nrep = 100)
 # 
 # iter converged k k0    logLik      AIC      BIC      ICL
 # 1    3      TRUE 1  1 -10735.90 21477.79 21496.47 21496.47
 # 2   23      TRUE 2  2 -10331.22 20676.45 20720.04 20737.65
 # 3   12      TRUE 3  3 -10196.58 20415.17 20483.66 20528.26
 # 4   20      TRUE 4  4 -10144.59 20319.17 20412.58 20469.26
 # 5   38      TRUE 5  5 -10130.26 20298.52 20416.83 20517.24
 # 6   34      TRUE 6  6 -10121.72 20289.45 20432.66 20562.92
 # 
 # Call:
 #   stepFlexmix(total_founded_reports ~ time + time_sq | tds_number, model = FLXMRnegbin(offset = log(analytical_dat$total_households)), 
 #               data = analytical_dat, control = list(iter.max = 500, minprior = 0), k = 1:6, nrep = 100)
 # 
 # iter converged k k0    logLik      AIC      BIC      ICL
 # 1    3      TRUE 1  1 -10733.16 21474.33 21499.23 21499.23
 # 2   23      TRUE 2  2 -10327.32 20672.64 20728.69 20747.16
 # 3   27      TRUE 3  3 -10191.55 20411.09 20498.27 20542.83
 # 4   19      TRUE 4  4 -10138.99 20315.98 20434.29 20490.48
 # 5   33      TRUE 5  5 -10121.64 20291.28 20440.72 20537.04
 # 6   40      TRUE 6  6 -10113.12 20284.23 20464.81 20588.64
 # 
 # stepFlexmix(total_founded_reports ~ time + time_sq + time_cub | tds_number, model = FLXMRnegbin(offset = log(analytical_dat$total_households)), 
 #             data = analytical_dat, control = list(iter.max = 500, minprior = 0), k = 1:6, nrep = 100)
 # 
 # iter converged k k0    logLik      AIC      BIC      ICL
 # 1    3      TRUE 1  1 -10686.93 21383.86 21414.99 21414.99
 # 2   26      TRUE 2  2 -10268.62 20559.23 20627.73 20646.49
 # 3   28      TRUE 3  3 -10123.16 20280.32 20386.17 20428.73
 # 4   20      TRUE 4  4 -10067.86 20181.72 20324.94 20379.22
 # 5   19      TRUE 5  5 -10049.74 20157.47 20338.05 20436.67
 # 6   32      TRUE 6  6 -10038.04 20146.09 20364.02 20488.49
 
 # tested 2-12 and 9 was the best
 # stepFlexmix(total_founded_reports ~ ns(time, 9) | tds_number, model = FLXMRnegbin(offset = log(analytical_dat$total_households)), 
 #             data = analytical_dat, control = list(iter.max = 500, minprior = 0), k = 1:6, nrep = 100)
 # 
 # iter converged k k0     logLik      AIC      BIC      ICL
 # 1    3      TRUE 1  1 -10552.483 21126.97 21195.46 21195.46
 # 2   15      TRUE 2  2 -10089.668 20225.34 20368.55 20386.75
 # 3   14      TRUE 3  3  -9915.697 19901.39 20119.33 20155.50
 # 4   20      TRUE 4  4  -9844.910 19783.82 20076.48 20131.03
 # 5   17      TRUE 5  5  -9796.812 19711.62 20079.01 20161.00
 # 6   24      TRUE 6  6  -9777.504 19697.01 20139.11 20230.21
 # 
 # 

# Extract the best-fitting model
lcga5 <- getModel(lcgaMix_ns_season3, which = 5)
lcga4 <- getModel(lcgaMix_ns_season3, which = 1)

print(lcgaMix_ns9)

# View model summary and parameter estimates



# Calculate class sizes
class_sizes <- lcgaMix_ns_season3@models$`5`@size / length(unique(analytical_dat$time))
print(class_sizes)


# Add class membership to dataset
#analytical_dat$class4 <- clusters(lcga4)
analytical_dat$class5 <- clusters(lcga5)

#analytical_dat$class5 <- clusters(lcgaMix_ns_season)



# Compute Mean, Standard Error, and Confidence Intervals by Class & Time
class_summary <- analytical_dat %>%
  group_by(time, class5) %>%
  summarise(
    mean_rate = mean(founded_mold_rate, na.rm = TRUE),
    se = sd(founded_mold_rate, na.rm = TRUE) / sqrt(n()),  # Standard Error
    lower_ci = mean_rate - 1.96 * se,  # 95% CI lower bound
    upper_ci = mean_rate + 1.96 * se   # 95% CI upper bound
  ) %>%
  ungroup()


# Plot latent class trajectories
ggplot(class_summary, aes(x = time, y = mean_rate, color = as.factor(class5))) +
  geom_line(size = 1.2) +  
  geom_point(size = 2) +  
  labs(title = "Latent Class Trajectories Over Time",
       x = "Time",
       y = "Mean Founded Mold Rate",
       color = "Latent Class") +
  theme_minimal()

ggplot() +
  
  # Mean trajectory per class
  geom_smooth(data = class_summary, aes(x = time, y = mean_rate, color = as.factor(class5)), 
            size = 1.2) +
  
  # Confidence interval ribbons per class
 # geom_ribbon(data = class_summary, aes(x = time, ymin = lower_ci, ymax = upper_ci, fill = as.factor(class)), 
      #        alpha = 0.2) +
  
  # Labels and Theme
  labs(title = "",
       x = "Time",
       y = "Founded Mold Rate",
       color = "Latent Class",
       fill = "Latent Class") +
  theme_minimal()

write.fst(analytical_dat, "classes_lcga2021.fst")

