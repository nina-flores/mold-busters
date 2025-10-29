# The purpose here is to take the city's heat raster file and aggregate to 
# census tracts and find quartiles across the entire city.

require(raster)
require(tigris)
require(fst)
require(sf)
require(dplyr)


# read in heat data -------------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/heat")
temp_raster <- raster("f_deviation_smooth.tif")

plot(temp_raster)

crs(temp_raster)

# read in census tract tiger files ----------------------------------------

# Specify the state and county codes for New York City's five boroughs
state_code <- "NY"
county_codes <- c("005", "047", "061", "081", "085")

nyc_ct <- tracts(state = state_code, county = county_codes)

# Convert the CRS to WGS84
nyc_ct_wgs84 <- st_transform(nyc_ct, 4326) # same as raster layer


# perform extraction over nyc census tracts
heat_ct  =  raster::extract(temp_raster, # raster (x) to extract from
                                  y=nyc_ct_wgs84,  # shapefile (y) to overlay and take values forward 
                                  weights = TRUE, # calculate weights used for averaging (area-weighted mean)
                                  normalizeWeights=TRUE, # normalize weights to always add up to 1 if, say, some of the shapefile is not covered by a raster 
                                  fun=mean, # take the mean of the values
                                  df=TRUE, # return as a dataframe object
                                  na.rm=TRUE # remove NAs before function (mean) applied so that NAs aren't returned
)

heat_ct_df <- nyc_ct_wgs84 %>%
  mutate(f_deviation_smooth = heat_ct$f_deviation_smooth) %>%
  as.data.frame() %>%
  dplyr::select(GEOID, f_deviation_smooth) 

plot(temp_raster)
  
  
# Step 1: Calculate the quartiles
quartiles <- quantile(heat_ct_df$f_deviation_smooth, probs = c(0, 0.25, 0.5, 0.75, 1))

# Step 2: Use cut to create a new column
heat_ct_df <- heat_ct_df %>%
  mutate(heat_quartile = cut(f_deviation_smooth, breaks = quartiles, include.lowest = TRUE, labels = FALSE))
  

# write out data  ---------------------------------------------------------
setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/hierarchical_processed_data")
write.fst(heat_ct_df, "heat_ct.fst")

