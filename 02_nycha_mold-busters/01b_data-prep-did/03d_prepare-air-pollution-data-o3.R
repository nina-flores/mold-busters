library(sf)
library(ggplot2)
library(dplyr)
library(raster)
library(tigris) # for tract shapefiles

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/tl_2020_36_tract")
tracts_NY <- st_read("tl_2020_36_tract.shp") %>%
  filter(COUNTYFP %in% c("005", "047", "061", "085", "081")) %>%
  dplyr::select(GEOID) %>%
  st_transform(crs = 2263)

# Set working directory
setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/AnnAvg_1_15_300m")

# Set target CRS for rasters
target_crs <- st_crs(tracts_NY)$epsg

# Create an empty list to store yearly data
o3_list <- list()

# Loop over years 2016 (aa8) through 2023 (aa15)
for (i in 8:15) {
  year <- 2008 + i
  folder <- paste0("s", i, "_o3300m")
  raster_path <- file.path(folder, "prj.adf")
  
  if (file.exists(raster_path)) {
    cat("Processing:", year, "\n")
    
    o3_raster <- raster(raster_path)
    crs(o3_raster) <- target_crs # Set CRS to match tracts
    
    # Extract weighted mean o3 for each tract
    o3_df <- raster::extract(
      x = o3_raster,
      y = tracts_NY,
      weights = TRUE,
      normalizeWeights = TRUE,
      fun = mean,
      df = TRUE,
      na.rm = TRUE
    ) %>%
      rename(annual_o3 = 2) %>% # second column is the extracted value
      mutate(GEOID = tracts_NY$GEOID,
             year = year)
    
    o3_list[[as.character(year)]] <- o3_df
  } else {
    warning("Missing raster for year ", year, ": ", raster_path)
  }
}

# Combine all years into one data frame
nyccas_all_years <- bind_rows(o3_list) 

nyccas_all_years_dat <- nyccas_all_years %>%
  dplyr::select(-ID)

# Preview
head(nyccas_all_years)

# save to file
setwd("~/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/processed_air_pollution")
write.csv(nyccas_all_years, "nyccas_o3_by_tract_2016_2023.csv", row.names = FALSE)

plot(o3_raster)
