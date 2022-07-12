library(tidyverse)
library(sf)
library(maps)
library(rnaturalearthdata)
library(leaflet)

germany_sf <- countries50 %>% 
  st_as_sf() %>% 
  filter(name == "Germany")

germany_cities <- world.cities %>% 
  filter(country.etc == "Germany") %>% 
  slice_max(pop, n = 5)

leaflet() %>% 
  addPolygons(data = germany_sf,
              weight = 1,
              color = "black",
              fillColor = "darkolivegreen",
              fillOpacity = 1) %>% 
  addCircleMarkers(data = germany_cities)
