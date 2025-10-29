###load packages

require(dplyr)
require(sf)
require(tidyverse)
require(fst)
require(tigris)
require(cowplot)
require(patchwork)

setwd("~/Desktop/projects/Mattlab/F31/data/processed data")
data_post <- read.fst("mold-met-data-post-mb.fst") %>% drop_na(year) %>%
  filter(MMWRyear > 2020) %>% 
  drop_na(intervention_date)


setwd("~/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/time_series")
nycha_building_shape_unique <- readRDS(file = "nycha_building_shape_unique.rds")


# Aggregate values by year
### to get standardized sum of exposure weeks per building
annual_data_post  <- data_post %>%
  group_by(tds_number, building_number) %>%
  summarise(AnnualSumAverage = (sum(week_95th_ind_precip)/3)) %>%
  ungroup() 



### get building-specific thresholds week_95th_precip = week_95th_precip * 0.0393701 in inches
building_thresholds <- data_post %>%
  select(tds_number, building_number,week_95th_precip) %>%
  unique() %>%
  mutate(week_95th_precip = week_95th_precip * 0.0393701) 



### join to dataset with building shapefile

data_map_post <- annual_data_post %>%
  left_join(nycha_building_shape_unique, by = c("tds_number" = "tds_number", "building_number" = "building_number"))  %>% 
  st_as_sf()

data_map_thresholds <- building_thresholds %>%
  left_join(nycha_building_shape_unique, by = c("tds_number" = "tds_number", "building_number" = "building_number"))  %>% 
  st_as_sf()


### read in new york city county file
nyc_county <- counties(state = "NY", cb = TRUE) %>%
  filter(NAME %in% c("Kings", "New York", "Queens", "Bronx", "Richmond"))  
custom_palette<- c("#fee6ce" , "#9ecae1","#3182bd",  "#59234E")
  

### map the 95th percentile precipitation by building
data_map_thresholds <- data_map_thresholds[order(data_map_thresholds$week_95th_precip, decreasing = FALSE), ]


thresh<- ggplot()+
  geom_sf(data = nyc_county, color = "black", fill = "white") +
  geom_sf(data = data_map_thresholds, aes(color = week_95th_precip), size = 2)+
  ggtitle("a. 95th percentile precipitation")+
  labs(color = "Weekly sum (in.)") +
  scale_color_gradientn(colors = custom_palette,
                        na.value = "grey50")  +
  theme_void()+
  theme(plot.title = element_text(size =15,margin = margin(b = -200)),
        legend.position = c(.15, .8), legend.text=element_text( size = 11),  
        legend.title = element_text( hjust = .8,size =12))


data_map_post <- data_map_post[order(data_map_post$AnnualSumAverage, decreasing = FALSE), ]

post <- ggplot()+
  geom_sf(data = nyc_county, color = "black", fill = "white") +
  geom_sf(data = data_map_post, aes(color = AnnualSumAverage), size = 2) +
  ggtitle("b. Annual total (Jan 2021 - Dec 2023)")+
  labs(color = "n per year") +
  scale_color_gradientn(colors = custom_palette,
                        limits = c(2, 7),
                        na.value = "grey50")  +
  theme_void()+
  theme(plot.title = element_text(size =15,margin = margin(b = -200)),
        legend.position = c(.15, .8), legend.text=element_text( size = 11), 
        legend.title = element_text(size =12))



### plot the maps together
thresh +  post +  plot_layout(ncol = 2, widths = c(1, 1))



