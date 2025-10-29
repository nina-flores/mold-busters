# The purpose of this script is to use tidycensus to pull in the census
# tract level census variables we want including

# median income
# % NHW
# % NHB
# % Hispanic
# poverty


require(tidyverse)
require(tidycensus)
require(fst)
census_api_key("b30c02786d1c9fddff73822ffe0dd6c8dd31b82e")


# pull in data ------------------------------------------------------------

v22 <- load_variables(2023, "acs5", cache = TRUE)

ct_data <- get_acs(geography = "tract", 
        variables = c(ct_median_income = "B06011_001",
                      total_population = "B03001_001",
                      nhw = "B03002_003",
                      nhb = "B03002_004",
                      hispanic = "B03002_012",
                      below_poverty_line = "B17001_002",
                      total_pov = "B01001_001"), # same as total pop
        state = "NY",
        county = c("005", "047", "061", "085", "081"),
        year = 2023)


# clean -------------------------------------------------------------------

ct_data_cleaned <- ct_data %>%
  select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(ct_percent_nhb = (nhb/total_population)*100,
         ct_percent_h = (hispanic/total_population)*100,
         ct_percent_nhw = (nhw/total_population)*100,
         ct_percent_pov = (below_poverty_line/total_pov)*100) %>%
  mutate(ct_pct_pov_quart = ntile(ct_percent_pov, 4))
  

hist(ct_data_cleaned$ct_percent_nhw)

# write out data ----------------------------------------------------------

setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")
write.fst(ct_data_cleaned, "census_data_ct.fst")
