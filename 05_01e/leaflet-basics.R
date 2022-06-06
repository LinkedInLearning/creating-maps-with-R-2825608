library(tidyverse)
library(rnaturalearthdata)
library(leaflet)
library(sf)

world_sf <- countries110 %>% 
  st_as_sf()



leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addPolygons(data = world_sf,
              label = ~name)
