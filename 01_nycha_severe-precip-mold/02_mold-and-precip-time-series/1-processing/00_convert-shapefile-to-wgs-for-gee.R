require(sf)

setwd("~/Desktop/projects/Mattlab/F31/data/NYCHA_buildings_shapefile")
dta <- st_read("nycha_buildings_with_bins2.shp")
dta_tf <- dta  %>% st_transform(4326)
st_write(dta, "nycha_buildings_with_bins-wgs2.shp", append =  FALSE)

