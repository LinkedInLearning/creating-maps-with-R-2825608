library(sf)
library(mapview)
library(tidyverse)

world_sf <- read_sf("data/world-shapefiles")

country_population <- read_csv("data/country-population.csv")






world_pop %>% 
  mapview(zcol = "continent")
