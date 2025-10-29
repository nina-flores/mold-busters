# the objective here is to create an indicator for the tracts that within
# 1 km of a nycha tract

library(sf)
library(dplyr)
library(readr)

setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
nycha_ct <- read.csv("nycha-ct.csv") %>%
  dplyr::select(geoid_clean) %>%
  mutate(geoid_clean = as.character(geoid_clean)) %>%
  unique()

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/tl_2020_36_tract")
all_ct <-  st_read("tl_2020_36_tract.shp") %>%
  filter(COUNTYFP %in% c("005", "047", "061", "085", "081")) %>%
  dplyr::select(GEOID) %>%
  st_transform(crs = 32618) # uses meters!

# Subset tracts for NYCHA locations
nycha_geom <- all_ct %>%
  filter(GEOID %in% nycha_ct$geoid_clean) 

# Buffer NYCHA tracts by 1km (1000 meters)
nycha_buffer <- st_buffer(nycha_geom, dist = 1000)

# Identify all tracts within 1km of any NYCHA tract

tracts_within_1km <- st_join(all_ct, nycha_buffer, join = st_intersects, left = FALSE)

a <- tracts_within_1km$GEOID.x %>%
  unique() %>%
  as.data.frame() %>%
  rename("geoid" = ".")

setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
write.csv(a, "nycha-neighbors-1000.csv", row.names = FALSE)  



                
                
