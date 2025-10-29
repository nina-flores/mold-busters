require(dplyr)
require(tidyverse)
require(fst)

# read in data and clean --------------------------------------------------

setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/svi")
svi_dat <- read.csv("NewYork.csv") %>%
  filter(COUNTY %in% c("New York", "Kings", "Queens", "Richmond", "Bronx")) %>%
  select(RPL_THEMES, FIPS) %>%
  mutate(svi = if_else(RPL_THEMES ==  -999, NA, RPL_THEMES)) %>%
  select(-RPL_THEMES)
  

# write out the data ------------------------------------------------------

setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")
write.fst(svi_dat, "svi_dat_ct.fst")

