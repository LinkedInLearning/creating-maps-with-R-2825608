library(maps)
library(tidyverse)
library(sf)
library(rnaturalearthdata)
library(rmapshaper)

new_zealand_sf <- countries50 %>% 
  st_as_sf() %>% 
  filter(name == "New Zealand")

new_zealand_top_10_cities <- world.cities %>% 
  filter(country.etc == "New Zealand") %>% 
  slice_max(pop, n = 10) %>% 
  arrange(desc(pop))

new_zealand_top_10_cities_sf <- new_zealand_top_10_cities %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = ms_filter_islands(new_zealand_sf, 1E8)) +
  geom_sf(data = new_zealand_top_10_cities_sf,
          aes(size = pop),
          shape = 21,
          alpha = 0.6,
          fill = "purple") +
  coord_sf(crs = 2193)
