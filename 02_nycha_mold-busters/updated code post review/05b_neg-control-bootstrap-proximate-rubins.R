
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

setwd(here("data", "did-analytical-datasets"))
# identify units that meet all thresholds in every year
keep_units <- read.csv("data_agegroup_ct.csv")%>%
  group_by(geo_group) %>%
  filter(all(prop_female >= .45),
         all(pop_young_old >= .5),
         all( building_age <= 95)) %>%
  pull(geo_group) %>%
  unique()

# Run models with 50 different seeds
all_results <- list()
for (seed in 1:100) {
  set.seed(seed)
  
  overall <- read.csv(here("data", "did-analytical-datasets", "data_agegroup_ct.csv")) %>%
    filter(analytical_age_group == "all") %>%
    filter(group != "nycha")  %>%
    mutate(consolidated_tds_number = geo_group)%>%
    filter(geo_group %in% keep_units)
  
  
  a <- overall %>%
    filter(nycha_neighb_250 == 1)
  
  unique_ids <- unique(a$consolidated_tds_number)
  treated_ids <- sample(unique_ids, size = floor(0.4 * length(unique_ids)))
  
  overall <- overall %>%
    mutate(first.treat = if_else(consolidated_tds_number %in% treated_ids, 2019, 0),
           visits_per_population = 1000 * (total_ed / denominator_age)) %>%
    filter(!year %in% c(2019, 2020)) %>%
    mutate(borough = substr(geoid, 1, 5),
           borough = if_else(borough == "36085", "36047", borough)) 
  
  unique(overall$borough)
  
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
                                annual_pm + 
                                ndvi + borough,
                              data = overall)
  
  names(overall)
  
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
  
  set.seed(1)
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

rubins_by_time <- function(data, event_time) {
  time_data <- data %>% filter(time == event_time)
  
  M <- nrow(time_data)
  theta_bar <- mean(time_data$estimate)
  W <- mean(time_data$std.error^2)
  B <- var(time_data$estimate)
  T <- W + (1 + 1/M) * B
  
  # Degrees of freedom
  df <- (M - 1) * (1 + W / ((1 + 1/M) * B))^2
  t_crit <- qt(0.975, df)
  
  lower <- theta_bar - t_crit * sqrt(T)
  upper <- theta_bar + t_crit * sqrt(T)
  
  data.frame(
    time = event_time,
    avg_estimate = theta_bar,
    lower_ci = lower,
    upper_ci = upper,
    std_error = sqrt(T),
    n_estimates = M
  )
}

rubins_results <- lapply(event_times, function(t) {
  rubins_by_time(did_results, t)
})

rubins_summary <- bind_rows(rubins_results) %>%
  arrange(time) %>%
  mutate(MB = if_else(time > 2019, "Post", "Pre")) %>%
  mutate(MB = factor(MB, levels = c("Pre", "Post"))) %>%
  mutate(time = as.character(time))

# Print result
print(rubins_summary)


# Plot results with 95% bootstrap CIs
p_a <- ggplot(rubins_summary, aes(x = as.character(time), y = avg_estimate, color = MB)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  facet_grid(.~MB, scales = "free_x", space = "free_x") +
  theme_minimal() +
  labs(
    title = "a. Null exposure annually",
    x = " ",
    y = "Average ATT estimate from 100 samples"
  ) +
  theme(text = element_text(size = 12), legend.position  = "none") +
  ylim(-10, 10) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Pre" = "#BF5700", "Post" = "#405A95FF")) 
  


p_a

setwd("C:/Users/nf2497/Desktop/mold-busters-project/figures")
ggsave("negative-control-proximate.pdf", p_a,width = 6, height = 4.5)


combined_plot <- p_a / p_b 

setwd("C:/Users/nf2497/Desktop/mold-busters-project/figures")
ggsave("negative-control-proximate-both.pdf", combined_plot,width = 6, height = 8)




