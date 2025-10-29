library(ggplot2)
library(dplyr)
library(patchwork)
library(here)
library(tidyverse)



setwd(here("data", "results-datasets-did"))
did_results <- read.csv("results.csv") %>%
  mutate(MB = if_else(time > 2019, "Post", "Pre-intervention")) %>%
  mutate(MB = factor(MB, levels = c("Pre-intervention", "Post"))) %>%
  mutate(time = as.character(time)) %>%
  mutate(group = factor(group, levels = c("all",
                                          "all_F",
                                          "all_M",
                                          "5-17",
                                          "18-39",
                                          "40+",
                                          "5-17_F",
                                          "5-17_M",
                                          "18-39_F",
                                          "18-39_M",
                                          "40+_F",
                                          "40+_M",
                                          "sens",
                                          "A",
                                          "B",
                                          "C",
                                          "D",
                                          "E",
                                          "No class"))) %>%
  arrange(group)


group_list <- unique(did_results$group)


plot_att_by_group <- function(data, title_prefix = "", ncol = 3, a = 0, b = 0) {
  group_list <- unique(data$group)
  
  
  plots <- map(group_list, function(grp) {
    ggplot(data %>% filter(group == grp),
           aes(x = time, y = estimate, ymin = conf.low, ymax = conf.high, color = MB)) +
      geom_pointrange(fatten = 4) +
      theme_minimal(base_size = 12) +
      labs(
        title = paste0(title_prefix, grp),
        x = "",
        y = "ATT (per 1,000 residents)"
      ) +
      theme(
        plot.title = element_text(size = 16),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "bottom"
      ) +
      coord_cartesian(ylim=c(a,b)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_color_manual(values = c("Pre-intervention" = "#72874EFF", "Post" = "#7570b3"))
  })
  
  # Combine into side-by-side layout
  wrap_plots(plots, ncol = ncol)
  
}
  

age_data <- did_results %>%
  filter(group %in% c( "5-17",
                       "18-39",
                        "40+")) %>%
  mutate(group = case_when(group == "5-17"~ "c. 5-17",
                           group == "18-39"~ "d. 18-39",
                           group == "40+"~ "e. 40+"))

p3 <- plot_att_by_group(age_data, title_prefix = "", ncol = 3, a = -20, b = 20) 
p3

sex_data <- did_results %>%
  filter(group %in% c("all_F",
                      "all_M")) %>%
  mutate(group = if_else(group == "all_F", "a. F", "b. M"))

p2<- plot_att_by_group(sex_data, title_prefix = "", ncol = 2, a = -20, b = 20) 
print(p2)

age_sex_data <- did_results %>%
  filter(!group %in% c("all_F",
                      "all_M",
                      "all",
                      "5-17",
                      "18-39",
                      "40+",
                      "A",
                      "B",
                      "C",
                      "D",
                      "E",
                      "No class",
                      "sens")) %>%
  mutate(group = case_when(group == "5-17_F" ~ "F 5-17",
                   group == "5-17_M" ~ "M 5-17",
                   group == "18-39_F" ~ "F 18-39",
                   group == "18-39_M" ~ "M 18-39",
                   group == "40+_F" ~ "F 40+",
                   group == "40+_M" ~ "M 40+"
                   ))

d <- plot_att_by_group(age_sex_data , title_prefix = "", ncol = 2, a = -15, b = 5) 

all <- did_results %>%
  filter(group == c("all")) %>%
  mutate(group = if_else(group == "all", "",""))

p1 <- plot_att_by_group(all , title_prefix = "",ncol = 1, a = -15, b = 5) 
print(p1)


sens <- did_results %>%
  filter(group == "sens") %>%
  mutate(group = if_else(group == "sens", "",""))

unique(did_results$group)

p5 <- plot_att_by_group(sens , title_prefix = "",ncol = 1, a = -15, b = 5) 


lcga <- did_results %>%
  filter(group %in% c("A", "B", "C", "D", "E", "No class")) %>%
  mutate(group = case_when(group == "A"~ "A (consistently high)",
                           group == "B"~ "B (medium to low)",
                           group == "C"~ "C (low to high)",
                           group == "D"~ "D (low to no)",
                           group == "E"~ "E (no to low)",
                           group == "No class"~ "No class"))

lcga_plot <- plot_att_by_group(lcga , title_prefix = "",ncol = 2, a = -20, b = 10) 

  

# arrange overall, sex, and age results
combined_plot <-  p2/ p3 
print(combined_plot)

setwd("C:/Users/nf2497/Desktop/mold-busters-project/figures")
# save out each plot
ggsave("main-did.pdf", p1,width = 6, height = 4.5)
ggsave("main-did-strata.pdf", combined_plot,width = 11, height = 7)
ggsave("agesex-strata.pdf", d,width = 7, height = 8)
ggsave("lcga-strata.pdf", lcga_plot,width = 7, height = 8)
ggsave("sens-did.pdf", p5,width = 6, height = 4.5)


