library(sf)
library(mapview)
library(tidyverse)

world_sf <- read_sf("data/world-shapefiles")

country_population <- read_csv("data/country-population.csv")

world_pop <- world_sf %>% 
  left_join(country_population) %>% 
  filter(name != "Antarctica") %>% 
  group_by(continent) %>% 
  summarise(continent_population = sum(pop_est, na.rm = TRUE))




world_pop %>% 
  mapview(zcol = "continent_population")
