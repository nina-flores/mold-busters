### The objective of this file is to link the work order data to the building shapfiles
### by first linking it to the BIN

### I plan to write out a shapefile that I can use to pull the meterological data at the 
### bin level



### load required packages
require(tidyverse)
require(lubridate)
require(dplyr)
require(sf)

setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
dta <- read.fst("building_characteristics.fst") %>%
  mutate(tds_number = as.numeric(tds_number)) %>%
  select(tds_number, building_number) %>%
  unique()


### read in the BIN data from the NYCHA residential addresses datafile

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/NYCHA addresses")

bin <- read.csv("NYCHA_Residential_Addresses_20231114.csv")%>%
  janitor::clean_names() %>%
  select(development, tds, building, bin, address) %>%
  unique()

n_distinct(bin$tds) ### 285 ### this is mostly missing the rad/pact data
n_distinct(dta$tds_number) ### 243

### how many total tds building combinations?
tds_build_pair <- read.csv("NYCHA_Residential_Addresses_20231114.csv")%>%
  janitor::clean_names() %>%
  select(tds, building) %>%
  unique()
### there are 2,212 unique tds building combinations, some have multiple bins - we will be getting precip
### data for each bin, so I will later average the precip data for each tds building combination

dta_bin <- left_join(dta, bin, by = c("tds_number" = "tds", "building_number" = "building"))

dta_bin %>%
  select(tds_number, building_number) %>%
  n_distinct() ### all 2166 joined

bins_for_shape <- dta_bin %>%
  select(development, tds_number, address, building_number, bin)%>%
  unique() %>%
  mutate(bin = if_else(building_number == "4E",  3323299, bin)) %>%
  mutate(bin = if_else(building_number == "4W", 3323302, bin)) %>%
  mutate(development = if_else(building_number == "4W" | building_number == "4E", "GOWANUS", development)) %>%
  select(development, tds_number, building_number, bin)%>%
  unique() 

bins_for_shape %>%
  select(tds_number, building_number) %>%
  n_distinct()

### now link this to the nyc bin shapefile
setwd("~/Desktop/projects/Mattlab/F31/data/NYC Building Footprints")
shape <- st_read("geo_export_de70d948-26b6-4497-9cef-dc2bad06d1c1.shp") %>%
  select(bin, geometry) %>%
  st_transform(4326) ### want all data in wgs for future use in GEE

### merge the data
dta_bin_shape <- left_join(bins_for_shape, shape, by = "bin") %>% 
  st_as_sf() %>%
  unique()

dta_bin_shape %>%
  select(tds_number, building_number) %>%
  n_distinct()

### figure out which ones could not be matched by bin 
### and give them the geometry of another building in the same development
empty_geometries <- st_is_empty(dta_bin_shape)

### add this info to the original dataset
dta_bin_shape$empty_geometries <- empty_geometries # only 2 now

dta_bin_shape_gow <- dta_bin_shape %>%
  filter(building_number == "4E" | building_number == "4W")

dta_bin_shape <- dta_bin_shape %>%
  filter(!(building_number == "4E" | building_number == "4W")) %>%
  group_by(development) %>%
  mutate(
    building_number = as.numeric(building_number),
    building_number_old = building_number,
    nearest_row = if_else(empty_geometries, map2_int(empty_geometries, building_number, ~ {
      if (.x) {
        non_empty_indices <- which(!empty_geometries)
        non_empty_indices[which.min(abs(building_number[non_empty_indices] - .y))]
      } else {
        NA_integer_
      }
    }), row_number())
  ) %>%
  ungroup() %>%
  group_by(development) %>%
  mutate(
    bin = if_else(empty_geometries, bin[nearest_row], bin),
    geometry = if_else(empty_geometries, geometry[nearest_row], geometry),
    building_number_match = if_else(empty_geometries, building_number[nearest_row], building_number)
  ) %>%
  ungroup()

final_sf <- dta_bin_shape %>%
  select(-nearest_row, -building_number_old, -building_number_match) %>%
  rbind(dta_bin_shape_gow) 

### write out this shapefile
setwd("~/Desktop/projects/Mattlab/F31/data/NYCHA_buildings_shapefile")
st_write(final_sf, "nycha_buildings_with_bins2.shp", append=FALSE)


