# The purpose of this file is to create the 3 flooding indicators for NYCHA
# buildings using the bin shapefile. We buffer each of the stormwater flood
# maps by 100 feet and also created an indicator of overlap with the 
# coastal flood zone shapefile.

require(sf)
require(dplyr)
require(tigris)
require(purrr)
require(fst)

# read in stormwater data -------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/floods/NYC_Stormwater_Flood_Map_-_Moderate_Flood_with_Current_Sea_Levels")
stormwater_flood <- st_read("NYC Stormwater Flood Map - Moderate Flood with Current Sea Levels.gdb")

stormwater_layers <- st_layers("NYC Stormwater Flood Map - Moderate Flood with Current Sea Levels.gdb")


nuisance_flood_shape <- st_geometry(stormwater_flood$Shape[1]) %>%
  st_buffer(dist = 100)
deep_flood_shape <- st_geometry(stormwater_flood$Shape[2])%>%
  st_buffer(dist = 100) # distance in FEET



# join stormwater flooding with census tract boundaries -------------------

# read in nycha building shapefile
setwd("~/Desktop/projects/Mattlab/F31/data/NYCHA_buildings_shapefile")
nycha <- st_read("nycha_buildings_with_bins2.shp") %>%
  st_transform(2263) %>%
  mutate(geometry = st_centroid(geometry),
         tds_number = as.numeric(tds_nmb),
         building_number = as.character(bldng_n))

# merge nuisance flooding
nuisance_flood <- nycha %>% 
  st_intersects(nuisance_flood_shape) 

# Convert the list into a binary vector
intersection_indicator <- map_int(nuisance_flood, ~ ifelse(length(.x) > 0, 1, 0))


# Add the binary vector as a new column in nyc_ct
nycha$intersects_nuisance_flood <- intersection_indicator

# merge deep flooding

deep_flood <- nycha %>% 
  st_intersects(deep_flood_shape) 

# Convert the list into a binary vector
intersection_indicator <- map_int(deep_flood, ~ ifelse(length(.x) > 0, 1, 0))

# Add the binary vector as a new column in nyc_ct
nycha$intersects_deep_flood <- intersection_indicator


# read in sea level data --------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/floods/Sea Level Rise Maps (2020s 100-year Floodplain)")
coastal_flood <- st_read("geo_export_49dfcb2c-d930-4c7f-8e60-6144b652f88b.shp") %>%
  st_transform(2263)

# intersect
coastal_flood <- nycha %>% 
  st_intersects(coastal_flood) 

# Convert the list into a binary vector
intersection_indicator <- map_int(coastal_flood, ~ ifelse(length(.x) > 0, 1, 0))

# Add the binary vector as a new column in nyc_ct
nycha$intersects_coastal_flood <- intersection_indicator



# write out dataset -------------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")

nycha <- nycha %>%
  as.data.frame() %>%
  select(-geometry)

write.fst(nycha, "flooding_nycha.fst")
