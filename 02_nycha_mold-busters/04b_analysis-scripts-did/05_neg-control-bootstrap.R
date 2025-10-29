
# Load required libraries
library(ggplot2)
library(patchwork)
library(dplyr)
library(paletteer)
library(here)
library(fst)
library(tidyverse)
library(DRDID)
library(did)


# Run models with 15 different seeds
all_results <- list()
for (seed in 1:50) {
  set.seed(seed)
  
  overall <- read.csv(here("data", "did-analytical-datasets", "data_agegroup.csv")) %>%
    filter(analytical_age_group == "all") %>%
    filter(group != "nycha") %>%
    filter(nycha_neighb_1000 == 1)
  
  
  a <- overall %>%
    filter(nycha_neighb_1000 == 1)
  
  unique_ids <- unique(a$consolidated_tds_number)
  treated_ids <- sample(unique_ids, size = floor(0.4 * length(unique_ids)))
  
  overall <- overall %>%
    mutate(first.treat = if_else(consolidated_tds_number %in% treated_ids, 2019, 0),
           visits_per_population = 1000 * (total_ed / denominator_age)) %>%
    filter(!year %in% c(2019, 2020, 2021)) %>%
    mutate(borough = substr(geoid, 1, 5)) 
  
  b <- overall %>%
    filter(year == 2016,
           first.treat == 2019)
  
  print(sum(b$denominator_age))
  
  mb.attgt_with_cov <- att_gt(yname = "visits_per_population",
                              tname = "year",
                              idname = "consolidated_tds_number",
                              gname = "first.treat",
                              xformla = ~ building_age +
                                pop_young_old +
                                ppt +
                                tdmean +
                               # nycha_neighb_1000 +
                                annual_pm+ #annual_bc+ annual_o3+ annual_no2+
                                ndvi,
                              data = overall)
  
  print(summary(mb.attgt_with_cov))
  
  res_df <- broom::tidy(mb.attgt_with_cov) %>%
    mutate(seed = seed)
  
  all_results[[seed]] <- res_df
}

# Combine all results
did_results <- bind_rows(all_results)


# Check structure
print(head(did_results))
print(table(did_results$time))

# Get unique event times
event_times <- sort(unique(did_results$time))

# Non-parametric bootstrap function (ignores SEs)
boot_by_time_nonparametric <- function(data, event_time, R = 1000) {
  time_data <- data %>% filter(time == event_time)
  
  set.seed(4)
  boot_means <- replicate(R, {
    sampled_estimates <- sample(time_data$estimate, replace = TRUE)
    mean(sampled_estimates)
  })
  
  ci <- quantile(boot_means, c(0.025, 0.975))
  
  data.frame(
    time = event_time,
    avg_estimate = mean(time_data$estimate),
    lower_ci = ci[1],
    upper_ci = ci[2],
    std_error = sd(boot_means),
    n_estimates = nrow(time_data)
  )
}

# Apply the bootstrap to each event time
bootstrap_results <- lapply(event_times, function(t) {
  boot_by_time_nonparametric(did_results, t)
})

# Combine results into one dataframe
bootstrap_summary <- bind_rows(bootstrap_results) %>%
  arrange(time) %>%
  mutate(MB = if_else(time > 2019, "Post", "Pre-intervention")) %>%
  mutate(MB = factor(MB, levels = c("Pre-intervention", "Post"))) %>%
  mutate(time = as.character(time)) 

# Print results
print(bootstrap_summary)

# Plot results with 95% bootstrap CIs
a <- ggplot(bootstrap_summary, aes(x = as.character(time), y = avg_estimate, color = MB)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  theme_minimal() +
  labs(
    title = "Non-Parametric Bootstrap Estimates with 95% Confidence Intervals",
    x = "Time",
    y = "Average ATT estimate from 50 samples"
  ) +
  theme(text = element_text(size = 12)) +
  ylim(-5, 5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Pre-intervention" = "#72874EFF", "Post" = "#7570b3"))


a

setwd("C:/Users/nf2497/Desktop/mold-busters-project/figures")
ggsave("negative-control.pdf", a,width = 6, height = 4.5)
