# The purpose of this file is to compile all the rad/pact converted units
# to the development level. 

require(tidyverse)
require(readxl)

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/data")
dat_pact <- read_csv("PACT_Dataset.csv") %>%
  janitor::clean_names() %>%
  select(development_name, conversion_date) %>%
  na.omit() %>%
  filter(conversion_date != "Development Name") %>%
  mutate(conversion_date = as.Date(conversion_date, format = "%m/%d/%Y"))

# add tds information to this

setwd("~/Desktop/projects/Mattlab/F31/data/Tenant data updated files")

# Read in sheet 2 for each of the building characteristic files
build_char_2016 <- read_excel("NYCHA data for Mold Research Columbia University Jan 2016 additional data.xlsx", sheet = 2) %>%
  mutate(year = 2016) %>%
  janitor::clean_names() %>%
  rename("development_name" = "development") %>%
  select(development_name, tds_number) %>%
  unique()

dat_pact <- left_join(dat_pact, build_char_2016 )


# use previous file to clean up the few missing

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/data")

a <- read.csv("rad_pact_information.csv") %>%
  select(development, tds) %>%
  rename("development_name" = "development") %>%
  unique()

dat_pact <- left_join(dat_pact, a) %>%
  mutate(tds_number = if_else(is.na(tds_number), tds, tds_number)) %>%
  select(tds_number, conversion_date)

write.csv(dat_pact, "rad_pact_clean.csv")

c <- dat_pact %>% filter(year(conversion_date) == 2023 | year(conversion_date) == 2021| year(conversion_date) == 2022) 
# same as other file
