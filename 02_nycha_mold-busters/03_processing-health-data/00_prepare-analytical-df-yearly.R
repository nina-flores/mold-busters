# The objective of this script is to aggregate the sparcs data to bin-months

require(dplyr)
require(lubridate)
require(tidyverse)
require(tigris)
require(purrr)
require(readr)
require(fst)

# read in data ------------------------------------------------------------
setwd("~/mold-busters/data/sparcs-data") # sparcs data last pullled 5/1/25


# Define a function to read and process the CSV file
read_and_process <- function(year) {
  file_name <- paste0("ed1723nyc_for_mb_", year, ".csv")
  read.csv(file_name) %>%
    drop_na(F1E.output.bin) %>% # removed 26,303 total observations (4.3%)
    mutate(ADMIT_DT = as.Date(ADMIT_DT, format = "%d%b%Y", tz = "American/New York" ),
           F1E.latitude = as.numeric(str_remove_all(F1E.latitude, '"')),
           year = year(ADMIT_DT))
}

# Loop through 2016-2023 and combine all into one dataframe
years <- 2016:2023
sparcs <- map_dfr(years, read_and_process) %>% # 581116 - 578389 = 2727 same day visits that we aggregate
  group_by(across(-c(ed, inpat))) %>%
  summarize(ed = max(ed),
            inpat = max(inpat)) %>%
  ungroup()

sum(sparcs$ed)
sparcs <- sparcs %>%
  mutate(analytical_age_group = case_when(
    AGE >= 5 & AGE <= 17 ~ "5-17",
    AGE >= 18 & AGE < 40 ~ "18-39",
    AGE >= 40 ~ "40+")) %>%
  mutate(gender = GENDER_CD)

setwd("~/mold-busters/data/bin-data")
link <- read.fst("for_doh.fst") # this is a bin to tds linkage df

setwd("~/mold-busters/data/processed-data")
nycha_additional <- read.csv("zero_visit_buildings.csv")%>%
  mutate(bin = bin_or_ct,
         census_tract = as.numeric(GEOID)) %>%
  select(bin, census_tract) %>%
  unique()


# aggregate blocks to block groups for later joins ------------------------

# now to get the census tract geoid from the block - need to take off the final 4 digits from the block geoid
sparcs <- sparcs %>%
  mutate(census_tract = substr(F1E.USCB_block_20, 1, 11)) %>%
  filter(!is.na(analytical_age_group)) # check if we want to retain those under 5 for any reason


# Calculate the total number of visits for each bin and year and analytical age group and gender
total_visits <- sparcs %>%
  group_by(F1E.output.bin, census_tract, year, analytical_age_group, gender) %>%
  summarise(total_ed = sum(ed),
            total_inpat = sum(inpat)) %>% 
  rename(bin = F1E.output.bin) %>%
  mutate(census_tract = as.numeric(census_tract)) %>%
  ungroup()

nyc_total_visits <- total_visits %>%
  group_by(year, analytical_age_group) %>%
  summarize(total_ed = sum(total_ed),
            total_inpat = sum(total_inpat))

setwd("~/mold-busters/data/processed-data")
write.fst(nyc_total_visits, "nyc_total_visits.fst")

# getting all the bin - census tract combinations in nycha and 
# in the sparcs data. Going to expand this so that we can more cleanly add
# the few tds that dont have 
bin_census_tracts <- total_visits %>%
  full_join(nycha_additional) %>%
  select(bin, census_tract) %>%
  distinct()


expanded_df <- bin_census_tracts %>%
  tidyr::crossing(
    analytical_age_group = unique(total_visits$analytical_age_group),
    gender = unique(total_visits$gender),
    year = unique(total_visits$year)
  )

# Merge with the original dataset to keep existing values and fill missing ones with 0
df_complete <- expanded_df %>%
  left_join(total_visits, by = c("bin", "census_tract", "analytical_age_group", "year", "gender")) %>%
  mutate(total_ed = if_else(is.na(total_ed), 0, total_ed),
         total_inpat = if_else(is.na(total_inpat), 0, total_inpat)) %>%  # Replace NAs with 0
  ungroup()

nycha <- link %>%
  filter(group == "nycha") %>%
  mutate(bin = bin_or_ct) %>%
  mutate(bin = as.integer(bin)) %>%
  mutate(tds_or_ct = tds_number) 

census_tract <- link %>%
  filter(group == "controls") %>%
  mutate(census_tract = bin_or_ct) %>%
  mutate(tds_or_ct = census_tract)

nycha_sparcs <- nycha %>% 
  left_join(df_complete) %>%
  group_by(year, analytical_age_group, gender, tds_or_ct, building_number) %>%
  summarise(total_ed = sum(total_ed, na.rm = T),
            total_inpat = sum(total_inpat, na.rm = T)) %>%
  mutate(nycha = 1) %>%
  ungroup() %>%
  drop_na(analytical_age_group)


sum(nycha_sparcs$total_ed, na.rm = T)
sum(nycha_sparcs$total_inpat, na.rm = T)
# 87290/498181 17.5% of visits during this time

controls_sparcs <- census_tract %>% 
  left_join(df_complete) %>%
  group_by(year, analytical_age_group, gender, tds_or_ct, building_number) %>%
  summarise(total_ed = sum(total_ed, na.rm = T),
            total_inpat = sum(total_inpat, na.rm = T)) %>%
  mutate(nycha = 0)  %>% 
  ungroup() 
  

sum(controls_sparcs$total_ed, na.rm = T)
sum(controls_sparcs$total_inpat, na.rm = T)

#150339/498181
#[1] 30.2% of visits coming from the 525 control ct


data <- rbind(nycha_sparcs, controls_sparcs) %>%
  drop_na(analytical_age_group) %>%
  select(year, tds_or_ct, building_number, analytical_age_group, nycha, gender, total_ed, total_inpat) 

data_all <- data %>%
  group_by(year, tds_or_ct, building_number, gender, nycha) %>%
  summarize(total_ed = sum(total_ed, na.rm = T),
            total_inpat = sum(total_inpat, na.rm = T),
         analytical_age_group = "all") %>%
  ungroup()


data <- data %>% rbind(data_all) %>%
  mutate(analytical_age_group = factor(analytical_age_group, levels = c("all", "5-17",
                                                                        "18-39",
                                                                        "40+"))) %>%
  arrange(year) %>%
  group_by(tds_or_ct, building_number, analytical_age_group, gender) %>%
  mutate(time = row_number()) %>%
  ungroup() 

data_nycha <- data %>%
  filter(nycha == 1 & analytical_age_group == "all")
sum(data_nycha$total_ed)

setwd("~/mold-busters/data/processed-data")
write.fst(data, "processed_data.fst")

data_nycha %>%
  select(tds_or_ct, building_number) %>%
  n_distinct()

 link <- link %>%
   rename(tds_or_ct = tds_number)
# 
#    a <- anti_join(link, data_nycha) %>%
#       filter(group == "nycha") %>%
#       select(tds_or_ct, building_number, bin_or_ct, GEOID) %>%
#       unique()
# #   
#    write.csv(a, "zero_visit_buildings.csv", row.names = FALSE)



