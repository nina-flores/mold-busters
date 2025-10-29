library(ggplot2)
library(patchwork)
library(dplyr)
library(paletteer)
library(here)
library(fst)
library(tidyverse)
library(forestploter)
library(Cairo)

setwd(here("data", "results-datasets-did-aggregated"))

data <- read.csv("results.csv")

unique(data$group)


# make overall plot -------------------------------------------------------

data_overall <- data %>%
  mutate(group = case_when(group == "all"~ "Overall",
                           group == "18-39"~ "   18-39",
                           group == "40+"~ "   40+",
                           group == "5-17"~ "   5-17",
                           group == "all_M"~ "   M",
                           group == "all_F"~ "   F",
                           group == "5-17_F"~ "F 5-17",
                           group == "5-17_M"~ "M 5-17",
                           group == "18-39_F"~ "F 18-39",
                           group == "18-39_M"~ "M 18-39",
                           group == "40+_F"~ "F 40+",
                           group == "40+_M"~ "M 40+",
                           group == "A"~ "A",
                           group == "B"~ "B",
                           group == "C"~ "C",
                           group == "D"~ "D",
                           group == "E"~ "E",
                           group == "No class"~ "RAD/PACT",
                           group == "with_non_asthma"~ "   with other ED outcomes",
                           group == "with_uninsured"~ "   with uninsured rate",
                           group == "with_deathrate"~ "   with COVID deathrate",
                           group == "with_traffic"~ "   with traffic patterns",
                           group == "without_RAD" ~ "   excluding RAD/PACT",
                           group == "Restricted control proximity" ~ "   stricter control proximity",
                           group == "Restricted control income" ~ "   stricter control income",
                           group == "10_denominator" ~ "   10% larger NYCHA denominator",
                           group == "40_denominator" ~ "   40% larger NYCHA denominator")) %>%
  mutate(group = factor(group, levels = c("Overall",
                                          "   F",
                                          "   M",
                                          "   5-17",
                                          "   18-39",
                                          "   40+",
                                          "F 5-17",
                                          "M 5-17",
                                          "F 18-39",
                                          "M 18-39",
                                          "F 40+",
                                          "M 40+",
                                          "   with other ED outcomes",
                                          "   with uninsured rate",
                                          "   with COVID deathrate",
                                          "   with traffic patterns",
                                          "   stricter control proximity",
                                          "   stricter control income",
                                          "   excluding RAD/PACT",
                                          "   10% larger NYCHA denominator",
                                          "   40% larger NYCHA denominator",
                                          "A",
                                          "B",
                                          "C",
                                          "D",
                                          "E",
                                          "RAD/PACT")))
                           
                           
                           
  
  # main result
  data_main <- data_overall %>%
    filter(group %in% c("Overall","   F","   M","   5-17","   18-39","   40+")) %>%
    mutate(`ATT per 1,000 residents (95% CI)` = sprintf("%.1f (%.1f, %.1f)",
                                                        estimate,
                                                        conf.low,
                                                        conf.high),
           Group = group) %>%
    select(Group,`ATT per 1,000 residents (95% CI)`, estimate, conf.low, conf.high)
  
  # Add blank spacer rows where needed
  data_main <- bind_rows(
    data_main %>% filter(Group == "Overall"),
    tibble(Group = "Sex", `ATT per 1,000 residents (95% CI)` = "", 
           estimate = NA, conf.low = NA, conf.high = NA),
    data_main %>% filter(Group %in% c("   F","   M")),
    tibble(Group = "Age", `ATT per 1,000 residents (95% CI)` = "", 
           estimate = NA, conf.low = NA, conf.high = NA),
    data_main %>% filter(Group %in% c("   5-17","   18-39","   40+"))
  ) %>%
    mutate( ` `= "                                                  ",
           Group = factor(Group, levels = c("Overall","Sex","   F","   M","Age","   5-17","   18-39","   40+")))%>%
             arrange(Group) %>%
    mutate(label = `ATT per 1,000 residents (95% CI)`)

  
 p_main <-  ggplot(data_main, aes(x = estimate, y = Group)) +
    # vertical reference line
    geom_vline(xintercept = 0, linetype = "dashed") +
    
    # CI bars and points (skip spacer rows via na.rm = TRUE)
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0, linewidth = 0.6, color = "#405A95FF",
                   na.rm = TRUE) +
    geom_point(size = 2.2, color = "#405A95FF", na.rm = TRUE) +
    
    # centered labels on the point; nothing drawn for blank rows
    geom_text(aes(label = label),
              hjust = 0.5, vjust = -0.6, size = 3.7, na.rm = TRUE) +
    
    # axis + room for labels on the right
    scale_x_continuous(
      name = "ATT per 1,000 residents",
      
      expand = expansion(mult = c(0.02, 0.25))
    ) +
    # keep the exact order (incl. blanks) and show top-to-bottom in the same order as data_main
    scale_y_discrete(limits = rev(levels(data_main$Group)), drop = FALSE) +
    labs(y = NULL, x= "Average Treatement Effect on the Treated per 1,000 residents" ) +
    theme_minimal(base_size = 15) +
    theme(
     # panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(hjust = 0),
      axis.title.x = element_text(hjust = 0)
    )+xlim(-20,5)
  
  
  setwd("C:/Users/nf2497/Desktop/mold-busters-project/figures")
  ggsave("main_aggregated.pdf",
         plot = p_main,
         device = cairo_pdf,
         width = 8,
         height = 4)
  

# LCGA strata
  data_lcga <- data_overall %>%
    filter(group %in% c("A","B","C","D","E","RAD/PACT")) %>%
    mutate(`ATT per 1,000 residents (95% CI)` = sprintf("%.1f (%.1f, %.1f)",
                                                        estimate,
                                                        conf.low,
                                                        conf.high),
           Group = factor(group, levels = c("A","B","C","D","E","RAD/PACT")),
           label = `ATT per 1,000 residents (95% CI)`) %>%
    select(Group, `ATT per 1,000 residents (95% CI)`, estimate, conf.low, conf.high, label) %>%
    arrange(Group)
  
  p_lcga<- ggplot(data_lcga, aes(x = estimate, y = Group, color = Group)) +
    # vertical reference line
    geom_vline(xintercept = 0, linetype = "dashed") +
    
    # CI bars and points (colors mapped to Group)
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0, linewidth = 0.6, na.rm = TRUE) +
    geom_point(size = 2.2, na.rm = TRUE) +
    
    # centered labels on the point
    geom_text(aes(label = label),
              hjust = 0.5, vjust = -0.6, size = 5, na.rm = TRUE,
              color = "black") +   # keep labels black for readability
    
    # axis + room for labels on the right
    scale_x_continuous(
      name = "Average Treatement Effect on the Treated per 1,000 residents",
      expand = expansion(mult = c(0.02, 0.25))
    ) +
    # keep the exact order
    scale_y_discrete(limits = rev(levels(data_lcga$Group)), drop = FALSE) +
    
    # manual color palette
    scale_color_manual(name = "LCGA group", values = c(
      "A"        = "#bc3c29ff",
      "B"        = "#0072b5ff",
      "C"        = "#e18727ff",
      "D"        = "#ee4c97ff",
      "E"        = "#7876b1ff",
      "RAD/PACT" = "grey50"
    )) +
    
    xlim(-20, 5) +
    labs(y = NULL,x = " ") +
    theme_minimal(base_size = 20) +
    theme(
    #  panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
   #   axis.text.y = element_text(hjust = .5),  # right-align y labels
    axis.text.y = element_blank(),
   axis.title.x = element_text(hjust = 0),
      legend.position = "right"
    )
  
  
  setwd("C:/Users/nf2497/Desktop/mold-busters-project/figures")
  ggsave("lcga_aggregated.pdf",
         plot = p_lcga,
         device = cairo_pdf,
         width = 6,
         height = 7)

  
 # sensitivity analyses
  
  

  
  data_sentivities <- data_overall %>%
    filter(group %in% c("   with other ED outcomes",
                        "   with uninsured rate",
                        "   with COVID deathrate",
                        "   with traffic patterns",
                        "   excluding RAD/PACT",
                        "   stricter control proximity",
                        "   stricter control income",
                        "   10% larger NYCHA denominator",
                        "   40% larger NYCHA denominator")) %>%
    mutate(`ATT per 1,000 residents (95% CI)` = sprintf("%.1f (%.1f, %.1f)",
                                                        estimate,
                                                        conf.low,
                                                        conf.high),
           label = `ATT per 1,000 residents (95% CI)`, Group = group) %>%
    select(Group, `ATT per 1,000 residents (95% CI)`, estimate, conf.low, conf.high, label) 
  
  

  
  data_sentivities <- bind_rows(
    tibble(Group = "Alternative model adjustment", `ATT per 1,000 residents (95% CI)` = "", 
           estimate = NA, conf.low = NA, conf.high = NA),
    data_sentivities %>% filter(Group %in% c("   with other ED outcomes",
                                      "   with uninsured rate",
                                      "   with COVID deathrate",
                                      "   with traffic patterns")),
    tibble(Group = "Alternative selection criteria", `ATT per 1,000 residents (95% CI)` = "", 
           estimate = NA, conf.low = NA, conf.high = NA),
    data_sentivities %>% filter(Group %in% c("   excluding RAD/PACT",
                                      "   stricter control proximity",
                                      "   stricter control income")),
    tibble(Group = "Alternative NYCHA population", `ATT per 1,000 residents (95% CI)` = "", 
           estimate = NA, conf.low = NA, conf.high = NA),
    data_sentivities %>% filter(Group %in% c(  "   10% larger NYCHA denominator",
                                               "   40% larger NYCHA denominator")),
    
  ) %>%
    mutate( Group = factor(Group, levels = c("Alternative model adjustment",
                                             "   with other ED outcomes",
                                             "   with uninsured rate",
                                             "   with COVID deathrate",
                                             "   with traffic patterns",
                                             "Alternative selection criteria",
                                             "   excluding RAD/PACT",
                                             "   stricter control proximity",
                                             "   stricter control income",
                                             "Alternative NYCHA population",
                                             "   10% larger NYCHA denominator",
                                             "   40% larger NYCHA denominator"
                                             )))%>%
    arrange(Group) %>%
    mutate(label = `ATT per 1,000 residents (95% CI)`)
  
  
  p_sens <- ggplot(data_sentivities, aes(x = estimate, y = Group)) +
    # vertical reference line
    geom_vline(xintercept = 0, linetype = "dashed") +
    
    # CI bars and points (skip spacer rows via na.rm = TRUE)
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0, linewidth = 0.6, color = "#405A95FF",
                   na.rm = TRUE) +
    geom_point(size = 2.2, color = "#405A95FF", na.rm = TRUE) +
    
    # centered labels on the point; nothing drawn for blank rows
    geom_text(aes(label = label),
              hjust = 0.5, vjust = -0.6, size = 3.7, na.rm = TRUE) +
    
    # axis + room for labels on the right
    scale_x_continuous(
      name = "Average Treatement Effect on the Treated per 1,000 residents",
      
      expand = expansion(mult = c(0.02, 0.25))
    ) +
    # keep the exact order (incl. blanks) and show top-to-bottom in the same order as data_main
    scale_y_discrete(limits = rev(levels(data_sentivities$Group)), drop = FALSE) +
    labs(y = NULL, x= "Average Treatement Effect on the Treated per 1,000 residents" ) +
    theme_minimal(base_size = 14) +
    theme(
      # panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(hjust = 0)
    )+xlim(-20,5)
  
  
  
  setwd("C:/Users/nf2497/Desktop/mold-busters-project/figures")
  ggsave("sensitivity_aggregated.pdf",
         plot = p_sens,
         device = cairo_pdf,
         width = 8,
         height = 6)
  
  
  
  # Age and sex strata 
  data_age_sex <- data_overall %>%
    filter(group %in% c("F 5-17",
                        "M 5-17",
                        "F 18-39",
                        "M 18-39",
                        "F 40+",
                        "M 40+")) %>%
    mutate(`ATT per 1,000 residents (95% CI)` = sprintf("%.1f (%.1f, %.1f)",
                                                        estimate,
                                                        conf.low,
                                                        conf.high),
           Group = group, label = `ATT per 1,000 residents (95% CI)`) %>%
    select(Group,`ATT per 1,000 residents (95% CI)`, estimate, conf.low, conf.high, label) %>%
    arrange(Group) %>%
    mutate( Group = factor(Group, levels = c("F 5-17",
                                             "M 5-17",
                                             "F 18-39",
                                             "M 18-39",
                                             "F 40+",
                                             "M 40+")))
  
 p_agesex<-  ggplot(data_age_sex, aes(x = estimate, y = Group)) +
    # vertical reference line
    geom_vline(xintercept = 0, linetype = "dashed") +
    
    # CI bars and points (skip spacer rows via na.rm = TRUE)
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0, linewidth = 0.6, color = "#405A95FF",
                   na.rm = TRUE) +
    geom_point(size = 2.2, color = "#405A95FF", na.rm = TRUE) +
    
    # centered labels on the point; nothing drawn for blank rows
    geom_text(aes(label = label),
              hjust = 0.5, vjust = -0.6, size = 3.7, na.rm = TRUE) +
    
    # axis + room for labels on the right
    scale_x_continuous(
      name = "Average Treatement Effect among the Treated per 1,000 residents",
      
      expand = expansion(mult = c(0.02, 0.25))
    ) +
    # keep the exact order (incl. blanks) and show top-to-bottom in the same order as data_main
    scale_y_discrete(limits = rev(levels(data_age_sex$Group)), drop = FALSE) +
    labs(y = NULL, x= "Average Treatement Effect on the Treated per 1,000 residents" ) +
    theme_minimal(base_size = 14) +
    theme(
      # panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(hjust = 0)
    )
  
  
  setwd("C:/Users/nf2497/Desktop/mold-busters-project/figures")
  ggsave("agesex_aggregated.pdf",
         plot = p_agesex,
         device = cairo_pdf,
         width = 8,
         height = 5)
  

### season data 
  
  setwd(here("data", "results-datasets-did-aggregated"))
  
  season_data <- read.csv("season-results.csv")
  
  
  data_age_season <- season_data %>%
    separate(group, into = c("age","season"), sep = "_") %>%
    mutate(
      age = if_else(age == "all", "Overall", age),
      season = tools::toTitleCase(season),          # "summer" -> "Summer"
      age    = factor(age, levels = c("Overall","5-17","18-39","40+")),
      season = factor(season, levels = c("Summer","Fall","Winter","Spring")),
      label  = sprintf("%.1f (%.1f, %.1f)", estimate, conf.low, conf.high)
    )
  

  
  # assume columns: season, age, estimate, conf.low, conf.high
  season_order <- c("Spring","Summer","Fall","Winter")   # set your order
  age_order    <- rev(c("Overall","5-17","18-39","40+"))
  
  data_fs <- data_age_season %>%             # <-- use your df name
    mutate(
      season = factor(season, levels = season_order),
      age    = factor(age,    levels = age_order),
      label  = sprintf("%.1f (%.1f, %.1f)", estimate, conf.low, conf.high)
    )
  
  p_season <- ggplot(data_fs, aes(x = estimate, y = age)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0, linewidth = 0.6, color = "#405A95FF") +
    geom_point(size = 2.2, color = "#405A95FF") +
    geom_text(aes(label = label), hjust = 0.5, vjust = -0.6, size = 3.7) +
    scale_x_continuous(
      name = "Average Treatement Effect on the Treated per 1,000 residents",
      expand = expansion(mult = c(0.02, 0.25))
    ) +
    labs(y = NULL) +
    facet_grid(season ~ ., scales = "free_y", space = "free_y") +
    theme_minimal(base_size = 14) +
    theme(
      #panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(hjust = 1),            # right-align age labels
      strip.placement = "outside",
      strip.text.y.left = element_text(face = "bold"),  # bold season headers
      strip.background.y = element_blank(),
      panel.spacing.y = unit(10, "pt")                  # space between seasons
    )
  
  
  
  setwd("C:/Users/nf2497/Desktop/mold-busters-project/figures")
  ggsave("season_aggregated.pdf",
         plot = p_season,
         device = cairo_pdf,
         width = 6,
         height = 8)
  
  
  
  