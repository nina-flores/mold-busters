# Here link NYCHA developments to their respective census tract for the 
# linkage of tract level data.

require(tidyverse)
require(lubridate)
require(dplyr)
require(sf)
require(fst)
require(janitor)

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/prepped-for-doh")
pop_data <- read.fst("nycha_pop_data.fst") %>%
  mutate(tds_number = as.numeric(tds_number)) %>%
  select(-bin_or_ct)

# keep just one census tract for each development

nycha_ct <- pop_data %>%
  filter(group == "nycha") %>%
  select(GEOID, tds_number, building_number) %>%
 # count(GEOID, tds_number, name = "n_buildings") %>%
  # Get ct with most buildings per tds
  group_by(tds_number, GEOID, building_number) %>%
  slice(1) %>%
  ungroup() %>%
  rename(geoid_clean = GEOID) 

setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
write.csv(nycha_ct, "nycha-ct.csv", row.names = FALSE)


