library(tidyverse)
library(rnaturalearthdata)
library(leaflet)
library(sf)

world_sf <- countries110 %>% 
  st_as_sf()

