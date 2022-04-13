library(sf)
library(rnaturalearthdata)
library(gapminder)
library(tidyverse)


countries_sf <- countries110 %>% 
  st_as_sf() %>% 
  st_make_valid(oriented = FALSE) %>% 
  st_wrap_dateline()

dir.create("data/world-shapefiles")

countries_sf %>% 
  select(name, continent) %>% 
  write_sf("data/world-shapefiles/world-shapefiles.shp")

countries_sf %>% 
  st_drop_geometry() %>% 
  select(name, pop_est) %>% 
  drop_na() %>% 
  write_csv("data/country-population.csv")
