library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(janitor)
library(terra)
library(exactextractr)
library(fst)

#-------------------------
# 1. Read shapefile
#-------------------------
tracts_NY <- st_read("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/tl_2020_36_tract/tl_2020_36_tract.shp") %>%
  filter(COUNTYFP %in% c("005", "047", "061", "085", "081")) %>%
  dplyr::select(GEOID) %>%
  st_transform(2263)

#-------------------------
# 2. Read and clean traffic data
#-------------------------
tf_df <- read_excel("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/traffic-data/aadt_and_truckpct.xlsx") %>%
  clean_names() %>%
  drop_na(latitude, longitude) %>%
  filter(latitude != 0, longitude != 0) %>%
  pivot_longer(
    cols = starts_with("aadt_"),
    names_to = "year",
    names_prefix = "aadt_",
    values_to = "aadt"
  )

#-------------------------
# 3. Convert points to sf and filter to NYC tracts
#-------------------------
tf_sf <- st_as_sf(tf_df, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(2263) %>%
  st_filter(tracts_NY)

#-------------------------
# 4. Prepare raster grid template
#-------------------------
r_template <- rast(terra::ext(tracts_NY), resolution = 500, crs = st_crs(tracts_NY)$wkt)

#-------------------------
# 5. Loop over years using terra::interpIDW (moving window method)
#-------------------------

all_years <- unique(tf_sf$year)
results <- list()

for (yr in all_years) {
  
  cat("Processing year:", yr, "\n")
  
  pts_year <- tf_sf %>%
    filter(year == yr, !is.na(aadt))
  
  if (nrow(pts_year) == 0) {
    cat("No data for year", yr, "- skipping.\n")
    next
  }
  
  #-----------------------------------
  # 5a. Convert to SpatVector
  #-----------------------------------
  pts_vect <- vect(pts_year)
  
  # Double-check that "aadt" is present
  if (!"aadt" %in% names(pts_vect)) {
    stop("No 'aadt' column found in pts_vect for year ", yr)
  }
  
  #-----------------------------------
  # 5b. IDW interpolation using moving window
  #-----------------------------------
  idw_raster <- terra::interpIDW(
    x = r_template,
    y = pts_vect,
    field = "aadt",
    radius = 2000,   # Search radius in feet (adjust if needed)
    power = 2
  )
  
  #-----------------------------------
  # 5c. Aggregate interpolated raster to tracts
  #-----------------------------------
  tracts_NY$mean_aadt <- exactextractr::exact_extract(idw_raster, tracts_NY, 'mean')
  
  out <- tracts_NY %>%
    dplyr::select(GEOID, mean_aadt) %>%
    mutate(year = yr)
  
  results[[yr]] <- out
  
}

#-------------------------
# 6. Combine results across years and plot
#-------------------------

final_results <- bind_rows(results)

ggplot() +
  geom_sf(data = final_results %>% filter(year == "2022"), aes(fill = mean_aadt)) +
  theme_minimal() +
  labs(title = "Interpolated Mean AADT by Tract (2019)")

final_results <- final_results %>%
  as.data.frame() %>%
  select(-geometry)


# save to file
setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/traffic-data")
write.fst(final_results, "processed_traffic_2016_2023.fst")

# old code below

# setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/tl_2020_36_tract")
# tracts_NY <- st_read("tl_2020_36_tract.shp") %>%
#   filter(COUNTYFP %in% c("005", "047", "061", "085", "081")) %>%
#   dplyr::select(GEOID) %>%
#   st_transform(crs = 2263)
# 
# setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 3 - diff-in-diff/data/traffic-data")
# tf_df <- read_excel("aadt_and_truckpct.xlsx") %>%
#   janitor::clean_names() %>%
#   drop_na(latitude) %>%
#   drop_na(longitude) %>%
#   filter(latitude != 0) %>%
#   filter(longitude != 0)
# 
# 
# 
# tf_sf <- st_as_sf(tf_df, coords = c("longitude", "latitude"), crs = 4326)  # WGS84
# 
# pt_2263 <- st_transform(tf_sf, 2263) 
# 
# joined <- st_join(tracts_NY, pt_2263)
# 
# long_df <- joined %>%
#   pivot_longer(
#     cols = starts_with("aadt_"),
#     names_to = "year",
#     names_prefix = "aadt_",
#     values_to = "aadt"
#   ) %>%
#   dplyr:: select(GEOID, year, aadt) %>%
#   group_by(GEOID, year) %>%
#   summarize(ct_mean_aadt = mean(aadt, na.rm = T))
# 
# 
# ggplot() +
#   geom_sf(data = long_df, aes(fill = ct_mean_aadt)) +
#   theme_minimal()


# a bit of missingness with this method - lets interpolate using idw

