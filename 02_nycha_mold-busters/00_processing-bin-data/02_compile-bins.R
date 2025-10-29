# Purpose is to link rad/pact conversion information at the bin level to the 
# list of NYCHA bins that we have. # the other file did not have them all

library(fst)
library(tidyverse)
library(dplyr)

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/rad-pact")
pact_bins <- read.fst("pact_bin_data.fst") %>%
  mutate(bin = as.numeric(BIN),
         tds_number = as.numeric(tds_number),
         building_number = as.numeric(building_number)) %>%
  select(tds_number, building_number, bin, converted_date) %>%
  distinct()

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/NYCHA addresses")

bins <- read.csv("NYCHA_Residential_Addresses_20231114.csv") %>%
  janitor::clean_names() %>%
  select(tds, building, bin, address) %>%
  mutate(tds_number = tds,
         building_number = building) %>%
  select(-tds, -building) %>%
  mutate(building_number = if_else(building_number == "04E", "4.3", building_number),
         building_number = if_else(building_number == "04W", "4.7", building_number),
         building_number = as.numeric(building_number)) %>%
  distinct()

all_bin <- full_join(bins, pact_bins) %>%
  group_by(tds_number) %>%
  fill(converted_date, .direction = "downup") %>%  # Fill both downward & upward
  ungroup() %>%
  distinct()



all_bin %>%
  na.omit() %>%
  select(tds_number, building_number) %>%
  n_distinct()

e <- all_bin %>%
  na.omit() %>%
  select(tds_number, converted_date) %>%
  unique()

# both block 2010 and 2020 available in dohmh data, same with census tract, but no 
# block group. Will just need to aggregate later

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/bins")
write.fst(all_bin, "NYCHA_bin.fst")
