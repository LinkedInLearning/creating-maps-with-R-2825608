library(sf)
library(mapview)
library(tidyverse)

world_sf <- read_sf("data/world-shapefiles")

country_population <- read_csv("data/country-population.csv")

world_sf %>% 
  mapview(zcol = "continent")

world_sf %>% 
  left_join(country_population) %>% 
  filter(name != "Antarctica") %>% 
  mapview(zcol = "pop_est")
