library(sf)
library(tidyverse)
library(mapview)
library(lubridate)
library(janitor)
library(ggspatial)
library(systemfonts)
source("make-hex-bins.R")

nyc_traffic_accidents_2020 <- read_csv("data/nyc-traffic-accidents_2020.csv")

nyc_zips_sf <- read_sf("data/ny-zipcodes") %>% 
  clean_names() %>% 
  mutate(area = st_area(geometry),
         area = as.numeric(area))


gg_nyc_borough_areas <- ggplot() +
  geom_sf(data = nyc_zips_sf,
          aes(fill = area)) +
  scale_fill_viridis_c(labels = scales::number_format(scale = 1E-6),
                       name = expression(paste("Area (", km^2, ")"))) +
  theme_void(base_size = 20) +
  theme(legend.position = "top",
        legend.key.width = unit(3, "cm"))

gg_nyc_borough_areas %>% 
  ggsave("gg_nyc_borough_areas.png",
         .,
         width = 6.71 * 2,
         height = 4.82 * 2)

nyc_traffic_accidents_2020_sf <- nyc_traffic_accidents_2020 %>% 
  filter(year(crash_date) == 2020) %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(crs = st_crs(nyc_zips_sf))

nyc_zips_with_accidents_2020_sf <- nyc_zips_sf %>% 
  mutate(accidents_in_region = lengths(st_covers(nyc_zips_sf, nyc_traffic_accidents_2020_sf)))
  
nyc_zips_with_accidents_2020_sf %>% 
  mapview(zcol = "accidents_in_region")

lengths(st_covers(nyc_zips_sf, nyc_traffic_accidents_2020_sf))

lengths(st_within(nyc_traffic_accidents_2020_sf, nyc_zips_sf))

nyc_accidents_within_zips <- st_join(nyc_traffic_accidents_2020_sf, nyc_zips_sf, join = st_within, left = FALSE)
 