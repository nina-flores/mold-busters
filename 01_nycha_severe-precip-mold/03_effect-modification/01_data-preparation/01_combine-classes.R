# here I need to combine the final time series analytical dataset with
# building characteristics that we will assess for effect modification.

# These include: building age, presence of sheetrock, building archetype,
# % white residents, median income, neighborhood deep flood zones, previous 
# sandy damage, heat, and percent poverty. 


# load packages -----------------------------------------------------------

require(dplyr)
require(fst)
require(tidyverse)

# read in the data --------------------------------------------------------

# Set working directory
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")

lcga_classes <- read.fst("classes_lcga2021.fst") %>%
  select(class5, tds_number) %>%
  distinct() %>%
  mutate(class_letter = case_when(
    class5 == 1 ~ "D",
    class5 == 2 ~ "E",
    class5 == 3 ~ "A",
    class5 == 4 ~ "C",
    class5 == 5 ~ "B"))


# time series
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/time_series")
time_series <- readRDS("spatial_analytical_dat_frame.rds") %>%
  mutate(tds_number = as.factor(tds_number))
# currently there are only 1878 buildings in this dataset due to missing demographic info


# combine the datasets ----------------------------------------------------

analytical_data <- full_join(time_series, lcga_classes) %>%
  drop_na(tds_number)

analytical_data %>% filter(is.na(class_letter)) %>% select(tds_number) %>% n_distinct()
# checks out

setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/time_series")
write.fst(analytical_data, "em_analytical_data.fst")

