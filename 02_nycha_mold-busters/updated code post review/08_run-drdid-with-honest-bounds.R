library(ggplot2)
library(patchwork)
library(dplyr)
library(paletteer)
library(here)
library(fst)
library(tidyverse)
library(DRDID)
library(did)
library(HonestDiD)

#' @title honest_did
honest_did <- function(...) UseMethod("honest_did")

honest_did.AGGTEobj <- function(es,
                                e          = 0,
                                type       = c("smoothness", "relative_magnitude"),
                                gridPoints = 100,
                                ...) {
  type <- match.arg(type)
  if (es$type != "dynamic") stop("need to pass in an event study")
  if (es$DIDparams$base_period != "universal") stop("Use a universal base period for honest_did")
  
  es_inf_func <- es$inf.function$dynamic.inf.func.e
  n <- nrow(es_inf_func)
  V <- t(es_inf_func) %*% es_inf_func / n / n
  
  referencePeriod <- -1
  consecutivePre  <- !all(diff(es$egt[es$egt <= referencePeriod]) == 1)
  consecutivePost <- !all(diff(es$egt[es$egt >= referencePeriod]) == 1)
  if (consecutivePre | consecutivePost) {
    stop("honest_did expects a time vector with consecutive time periods;\nplease re-code your event study and interpret the results accordingly.")
  }
  
  hasReference <- any(es$egt == referencePeriod)
  if (hasReference) {
    referencePeriodIndex <- which(es$egt == referencePeriod)
    V    <- V[-referencePeriodIndex, -referencePeriodIndex]
    beta <- es$att.egt[-referencePeriodIndex]
  } else {
    beta <- es$att.egt
  }
  
  nperiods <- nrow(V)
  npre     <- sum(1 * (es$egt < referencePeriod))
  npost    <- nperiods - npre
  
  if (!hasReference & (min(c(npost, npre)) <= 0)) {
    msg <- if (npost <= 0) "not enough post-periods" else "not enough pre-periods"
    stop(paste0(msg, " (check your time vector; note honest_did takes -1 as the reference period)"))
  }
  
  baseVec1 <- basisVector(index = (e + 1), size = npost)
  orig_ci  <- constructOriginalCS(betahat        = beta,
                                  sigma          = V,
                                  numPrePeriods  = npre,
                                  numPostPeriods = npost,
                                  l_vec          = baseVec1)
  
  if (type == "relative_magnitude") {
    robust_ci <- createSensitivityResults_relativeMagnitudes(betahat        = beta,
                                                             sigma          = V,
                                                             numPrePeriods  = npre,
                                                             numPostPeriods = npost,
                                                             l_vec          = baseVec1,
                                                             gridPoints     = gridPoints,
                                                             ...)
  } else if (type == "smoothness") {
    robust_ci <- createSensitivityResults(betahat        = beta,
                                          sigma          = V,
                                          numPrePeriods  = npre,
                                          numPostPeriods = npost,
                                          l_vec          = baseVec1,
                                          ...)
  }
  
  return(list(robust_ci = robust_ci, orig_ci = orig_ci, type = type))
}


setwd(here("data", "did-analytical-datasets"))
# identify units that meet all thresholds in every year
keep_units <- read.csv("data_agegroup_ct.csv")%>%
  group_by(geo_group) %>%
  filter(all(prop_female >= .45),
         all(pop_young_old >= .5),
         all(building_age <= 95)) %>%
  pull(geo_group) %>%
  unique()

# ── Data prep ─────────────────────────────────────────────────────────────────

setwd(here("data", "did-analytical-datasets"))

make_data <- function(include_2021 = FALSE) {
  read.csv("data_agegroup_ct.csv") %>%
    mutate(consolidated_tds_number = geo_group) %>%
    mutate(denominator = denominator_age) %>%
    mutate(
      MB = if_else(group == "nycha", 1, 0),
      visits_per_population     = 1000 * (total_ed / denominator),
      visits_non_asthma_per_pop = 1000 * (total_ed_non_asthma / denominator)
    ) %>%
    filter(!year %in% c(2019, 2020)) %>%
    { if (!include_2021) filter(., year != 2021) else . } %>%
    mutate(time = if (include_2021) {
      case_when(
        year == 2016 ~ 1,
        year == 2017 ~ 2,
        year == 2018 ~ 3,
        year == 2021 ~ 4,
        year == 2022 ~ 5,
        year == 2023 ~ 6
      )
    } else {
      case_when(
        year == 2016 ~ 1,
        year == 2017 ~ 2,
        year == 2018 ~ 3,
        year == 2022 ~ 4,
        year == 2023 ~ 5
      )
    }) %>%
    mutate(first.treat = if_else(group == "nycha", 4, 0)) %>%
    filter(nycha_neighb_250 == 1) %>%
    mutate(
      borough = substr(geoid, 1, 5),
      borough = if_else(borough == "36085", "36047", borough)
    ) %>%
    filter(geo_group %in% keep_units)
}

data_no2021   <- make_data(include_2021 = FALSE)
data_with2021 <- make_data(include_2021 = TRUE)


# ── Helper: fit model + run both sensitivity analyses ─────────────────────────

covars <- c("building_age", "ppt", "pop_young_old", "ndvi", "tdmean", "annual_pm", "borough")

run_sensitivity <- function(data) {
  
  data_overall <- filter(data, analytical_age_group == "all")
  
  fit <- att_gt(
    yname      = "visits_per_population",
    tname      = "time",
    idname     = "consolidated_tds_number",
    gname      = "first.treat",
    xformla    = as.formula(paste("~", paste(covars, collapse = " + "))),
    data       = data_overall,
    panel      = TRUE,
    base_period = "universal"
  )
  
  agg <- aggte(fit, type = "dynamic")
  
  sens_rm <- honest_did(agg, e = 0, type = "relative_magnitude",
                        Mbarvec = seq(from = 0.5, to = 3, by = 0.5))
  
  sens_sd <- honest_did(agg, e = 0, type = "smoothness")
  
  list(
    plot_rm = HonestDiD::createSensitivityPlot_relativeMagnitudes(
      sens_rm$robust_ci, sens_rm$orig_ci),
    plot_sd = HonestDiD::createSensitivityPlot(
      sens_sd$robust_ci, sens_sd$orig_ci)
  )
}


# ── Run both datasets ──────────────────────────────────────────────────────────

results_with2021 <- run_sensitivity(data_with2021)
results_no2021   <- run_sensitivity(data_no2021)

# ── Combine into 2×2 panel ────────────────────────────────────────────────────
# Top row: with 2021 | Bottom row: without 2021
# Left col: relative magnitude | Right col: smoothness

combined_plot <-
  (results_with2021$plot_rm + ggtitle("Relative Magnitude (with 2021)")) +
  (results_with2021$plot_sd + ggtitle("Smoothness (with 2021)")) +
  (results_no2021$plot_rm   + ggtitle("Relative Magnitude (without 2021)")) +
  (results_no2021$plot_sd   + ggtitle("Smoothness (without 2021)")) +
  plot_layout(ncol = 2) +
  plot_annotation(
    title    = "HonestDiD Sensitivity Analysis",
    theme    = theme(plot.title    = element_text(face = "bold", size = 14),
                     plot.subtitle = element_text(size = 11))
  )

combined_plot

setwd("C:/Users/nf2497/Desktop/mold-busters-project/figures")
ggsave("honestdid_sensitivity_2x2.png", combined_plot,
       width = 14, height = 10, dpi = 300)
