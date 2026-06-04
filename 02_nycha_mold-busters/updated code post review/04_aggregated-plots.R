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

data_notrim <- read.csv("results_notrim.csv") %>% mutate(trim = "no_trim")
season_data <- read.csv("season-results.csv") %>% mutate(trim = "no_trim")
data_trim   <- read.csv("results_trim_0.95.csv") %>% mutate(group = "trim_0.95",
                                                            trim = "trim_0.95")

# --- Combine and apply global FDR adjustment -----------------------------

data_all <- bind_rows(data_notrim, data_trim, season_data) %>%
  mutate(p.fdr = p.adjust(p.value, method = "fdr"),
         sig   = p.fdr < 0.05)

data        <- data_all %>% filter(!str_detect(group, "_spring|_summer|_fall|_winter"))
season_data <- data_all %>% filter(str_detect(group, "_spring|_summer|_fall|_winter"))


# --- Recode groups -------------------------------------------------------

data_overall <- data %>%
  mutate(group = case_when(
    group == "all"                          ~ "Overall",
    group == "18-39"                        ~ "   18-39",
    group == "40+"                          ~ "   40+",
    group == "5-17"                         ~ "   5-17",
    group == "all_M"                        ~ "   M",
    group == "all_F"                        ~ "   F",
    group == "5-17_F"                       ~ "F 5-17",
    group == "5-17_M"                       ~ "M 5-17",
    group == "18-39_F"                      ~ "F 18-39",
    group == "18-39_M"                      ~ "M 18-39",
    group == "40+_F"                        ~ "F 40+",
    group == "40+_M"                        ~ "M 40+",
    group == "A"                            ~ "A (11%)",
    group == "B"                            ~ "B (20%)",
    group == "C"                            ~ "C (21%)",
    group == "D"                            ~ "D (25%)",
    group == "E"                            ~ "E (13%)",
    group == "No class"                     ~ "RAD/PACT (10%)",
    group == "with_non_asthma"              ~ "   with other ED outcomes",
    group == "with_uninsured"               ~ "   with uninsured rate",
    group == "with_deathrate"               ~ "   with COVID deathrate",
    group == "with_traffic"                 ~ "   with traffic patterns",
    group == "without_RAD"                  ~ "   excluding RAD/PACT",
    group == "Restricted control proximity" ~ "   stricter control proximity",
    group == "Restricted control income"    ~ "   stricter control income",
    group == "10_denominator"               ~ "   10% larger NYCHA denominator",
    group == "40_denominator"               ~ "   40% larger NYCHA denominator",
    group == "trim_0.95"                    ~ "   trimmed (0.95) overall"
  )) %>%
  mutate(group = factor(group, levels = c(
    "Overall",
    "   F", "   M",
    "   5-17", "   18-39", "   40+",
    "F 5-17", "M 5-17", "F 18-39", "M 18-39", "F 40+", "M 40+",
    "   with other ED outcomes", "   with uninsured rate",
    "   with COVID deathrate", "   with traffic patterns",
    "   stricter control proximity", "   stricter control income",
    "   excluding RAD/PACT", "   trimmed (0.95) overall",
    "   10% larger NYCHA denominator", "   40% larger NYCHA denominator",
    "A (11%)", "B (20%)", "C (21%)", "D (25%)", "E (13%)", "RAD/PACT (10%)"
  )))


# --- Helpers -------------------------------------------------------------

# Full CI label with asterisk if significant
make_label <- function(estimate, conf.low, conf.high, sig) {
  base <- sprintf("%.1f (%.1f, %.1f)", estimate, conf.low, conf.high)
  if_else(sig & !is.na(sig), paste0(base, " *"), base)
}

# Asterisk only (for plots without CI text)
make_sig_marker <- function(sig) {
  if_else(sig & !is.na(sig), "*", "")
}


# --- Main result plot ----------------------------------------------------

data_main <- data_overall %>%
  filter(group %in% c("Overall", "   F", "   M", "   5-17", "   18-39", "   40+")) %>%
  mutate(`ATT per 1,000 residents (95% CI)` = make_label(estimate, conf.low, conf.high, sig),
         Group = group) %>%
  select(Group, `ATT per 1,000 residents (95% CI)`, estimate, conf.low, conf.high, sig)

data_main <- bind_rows(
  data_main %>% filter(Group == "Overall"),
  tibble(Group = "Sex", `ATT per 1,000 residents (95% CI)` = "",
         estimate = NA, conf.low = NA, conf.high = NA, sig = NA),
  data_main %>% filter(Group %in% c("   F", "   M")),
  tibble(Group = "Age", `ATT per 1,000 residents (95% CI)` = "",
         estimate = NA, conf.low = NA, conf.high = NA, sig = NA),
  data_main %>% filter(Group %in% c("   5-17", "   18-39", "   40+"))
) %>%
  mutate(` ` = "                                                  ",
         Group = factor(Group, levels = c("Overall", "Sex", "   F", "   M",
                                          "Age", "   5-17", "   18-39", "   40+"))) %>%
  arrange(Group) %>%
  mutate(label = `ATT per 1,000 residents (95% CI)`)

p_main <- ggplot(data_main, aes(x = estimate, y = Group)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.6, color = "#405A95FF", na.rm = TRUE) +
  geom_point(size = 2.2, color = "#405A95FF", na.rm = TRUE) +
  geom_text(aes(label = label), hjust = 0.5, vjust = -0.6, size = 3.7, na.rm = TRUE) +
  scale_x_continuous(name = "ATT per 1,000 residents",
                     expand = expansion(mult = c(0.02, 0.25))) +
  scale_y_discrete(limits = rev(levels(data_main$Group)), drop = FALSE) +
  labs(y = NULL, x = "Average Treatment Effect on the Treated per 1,000 residents") +
  theme_minimal(base_size = 15) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(hjust = 0),
        axis.title.x = element_text(hjust = 0)) +
  xlim(-25, 5.5)

setwd("C:/Users/nf2497/Desktop/mold-busters-project/figures")
ggsave("main_aggregated.pdf", plot = p_main, device = cairo_pdf, width = 8, height = 4)


# --- LCGA plot -----------------------------------------------------------

data_lcga <- data_overall %>%
  filter(group %in% c("Overall", "A (11%)", "B (20%)", "C (21%)",
                      "D (25%)", "E (13%)", "RAD/PACT (10%)")) %>%
  mutate(`ATT per 1,000 residents (95% CI)` = make_label(estimate, conf.low, conf.high, sig),
         Group = factor(group, levels = c("Overall", "A (11%)", "B (20%)", "C (21%)",
                                          "D (25%)", "E (13%)", "RAD/PACT (10%)")),
         label = `ATT per 1,000 residents (95% CI)`) %>%
  select(Group, `ATT per 1,000 residents (95% CI)`, estimate, conf.low, conf.high, label) %>%
  arrange(Group)

p_lcga <- ggplot(data_lcga, aes(x = estimate, y = Group, color = Group)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 2.2, na.rm = TRUE) +
  geom_text(aes(label = label), hjust = 0.5, vjust = -0.6, size = 5,
            na.rm = TRUE, color = "black") +
  scale_x_continuous(name = "Average Treatment Effect on the Treated per 1,000 residents",
                     expand = expansion(mult = c(0.02, 0.25))) +
  scale_y_discrete(limits = rev(levels(data_lcga$Group)), drop = FALSE) +
  scale_color_manual(name = "LCGA group", values = c(
    "Overall"        = "black",
    "A (11%)"        = "#bc3c29ff",
    "B (20%)"        = "#0072b5ff",
    "C (21%)"        = "#e18727ff",
    "D (25%)"        = "#ee4c97ff",
    "E (13%)"        = "#7876b1ff",
    "RAD/PACT (10%)" = "grey50"
  )) +
  xlim(-20, 5) +
  labs(y = NULL, x = " ") +
  theme_minimal(base_size = 18) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(hjust = 0),
        legend.position = "right")

ggsave("lcga_aggregated-dev.pdf", plot = p_lcga, device = cairo_pdf, width = 6, height = 7)


# --- Sensitivity plot ----------------------------------------------------

data_sensitivities <- data_overall %>%
  filter(group %in% c(
    "   with other ED outcomes", "   with uninsured rate",
    "   with COVID deathrate", "   with traffic patterns",
    "   excluding RAD/PACT", "   stricter control proximity",
    "   stricter control income", "   trimmed (0.95) overall",
    "   10% larger NYCHA denominator", "   40% larger NYCHA denominator"
  )) %>%
  mutate(`ATT per 1,000 residents (95% CI)` = make_label(estimate, conf.low, conf.high, sig),
         label = `ATT per 1,000 residents (95% CI)`, Group = group) %>%
  select(Group, `ATT per 1,000 residents (95% CI)`, estimate, conf.low, conf.high, label, sig)

data_sensitivities <- bind_rows(
  tibble(Group = "Alternative model adjustment", `ATT per 1,000 residents (95% CI)` = "",
         estimate = NA, conf.low = NA, conf.high = NA, label = "", sig = NA),
  data_sensitivities %>% filter(Group %in% c("   with other ED outcomes", "   with uninsured rate",
                                             "   with COVID deathrate", "   with traffic patterns")),
  tibble(Group = "Alternative selection criteria", `ATT per 1,000 residents (95% CI)` = "",
         estimate = NA, conf.low = NA, conf.high = NA, label = "", sig = NA),
  data_sensitivities %>% filter(Group %in% c("   excluding RAD/PACT",
                                             "   stricter control proximity",
                                             "   stricter control income",
                                             "   trimmed (0.95) overall")),
  tibble(Group = "Alternative NYCHA population", `ATT per 1,000 residents (95% CI)` = "",
         estimate = NA, conf.low = NA, conf.high = NA, label = "", sig = NA),
  data_sensitivities %>% filter(Group %in% c("   10% larger NYCHA denominator",
                                             "   40% larger NYCHA denominator"))
) %>%
  mutate(Group = factor(Group, levels = c(
    "Alternative model adjustment",
    "   with other ED outcomes", "   with uninsured rate",
    "   with COVID deathrate", "   with traffic patterns",
    "Alternative selection criteria",
    "   excluding RAD/PACT", "   stricter control proximity",
    "   stricter control income", "   trimmed (0.95) overall",
    "Alternative NYCHA population",
    "   10% larger NYCHA denominator", "   40% larger NYCHA denominator"
  ))) %>%
  arrange(Group) %>%
  mutate(label = `ATT per 1,000 residents (95% CI)`)

p_sens <- ggplot(data_sensitivities, aes(x = estimate, y = Group)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.6, color = "#405A95FF", na.rm = TRUE) +
  geom_point(size = 2.2, color = "#405A95FF", na.rm = TRUE) +
  geom_text(aes(label = label), hjust = 0.5, vjust = -0.6, size = 3.7, na.rm = TRUE) +
  scale_x_continuous(name = "Average Treatment Effect on the Treated per 1,000 residents",
                     expand = expansion(mult = c(0.02, 0.25))) +
  scale_y_discrete(limits = rev(levels(data_sensitivities$Group)), drop = FALSE) +
  labs(y = NULL, x = "Average Treatment Effect on the Treated per 1,000 residents") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(hjust = 0)) +
  xlim(-20, 5)

ggsave("sensitivity_aggregated.pdf", plot = p_sens, device = cairo_pdf, width = 8, height = 6)


# --- Age x sex plot ------------------------------------------------------

data_age_sex <- data_overall %>%
  filter(group %in% c("F 5-17", "M 5-17", "F 18-39", "M 18-39", "F 40+", "M 40+")) %>%
  mutate(`ATT per 1,000 residents (95% CI)` = make_label(estimate, conf.low, conf.high, sig),
         Group = group, label = `ATT per 1,000 residents (95% CI)`,
         sig_marker = make_sig_marker(sig)) %>%
  select(Group, `ATT per 1,000 residents (95% CI)`, estimate, conf.low, conf.high,
         label, sig_marker) %>%
  arrange(Group) %>%
  mutate(Group = factor(Group, levels = c("F 5-17", "M 5-17", "F 18-39",
                                          "M 18-39", "F 40+", "M 40+")))

p_agesex <- ggplot(data_age_sex, aes(x = estimate, y = Group)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.6, color = "#405A95FF", na.rm = TRUE) +
  geom_point(size = 2.2, color = "#405A95FF", na.rm = TRUE) +
  geom_text(aes(x = conf.high, label = sig_marker),
            hjust = -0.3, vjust = 0.3, size = 5, color = "black", na.rm = TRUE) +
  scale_x_continuous(name = "Average Treatment Effect among the Treated per 1,000 residents",
                     expand = expansion(mult = c(0.02, 0.25))) +
  scale_y_discrete(limits = rev(levels(data_age_sex$Group)), drop = FALSE) +
  labs(y = NULL, x = "Average Treatment Effect on the Treated per 1,000 residents") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(hjust = 0))

ggsave("agesex_aggregated.pdf", plot = p_agesex, device = cairo_pdf, width = 8, height = 5)


# --- Season plot ---------------------------------------------------------

data_age_season <- season_data %>%
  separate(group, into = c("age", "season"), sep = "_") %>%
  mutate(
    age        = if_else(age == "all", "Overall", age),
    season     = tools::toTitleCase(season),
    age        = factor(age,    levels = c("Overall", "5-17", "18-39", "40+")),
    season     = factor(season, levels = c("Summer", "Fall", "Winter", "Spring")),
    label      = make_label(estimate, conf.low, conf.high, sig),
    sig_marker = make_sig_marker(sig)
  )

data_fs <- data_age_season %>%
  mutate(season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter")),
         age    = factor(age,    levels = rev(c("Overall", "5-17", "18-39", "40+"))))

p_season <- ggplot(data_fs, aes(x = estimate, y = age)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.6, color = "#405A95FF") +
  geom_point(size = 2.2, color = "#405A95FF") +
  geom_text(aes(x = conf.high, label = sig_marker),
            hjust = -0.3, vjust = 0.3, size = 5, color = "black") +
  scale_x_continuous(name = "Average Treatment Effect on the Treated per 1,000 residents",
                     expand = expansion(mult = c(0.02, 0.25))) +
  labs(y = NULL) +
  facet_grid(season ~ ., scales = "free_y", space = "free_y") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(hjust = 1),
        strip.placement = "outside",
        strip.text.y.left = element_text(face = "bold"),
        strip.background.y = element_blank(),
        panel.spacing.y = unit(10, "pt"))

ggsave("season_aggregated.pdf", plot = p_season, device = cairo_pdf, width = 6, height = 8)


