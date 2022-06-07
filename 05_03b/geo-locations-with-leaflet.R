library(tidyverse)
library(sf)
library(maps)
library(leaflet)
library(rnaturalearthdata)

brazil_sf <- countries110 %>% 
  st_as_sf() %>% 
  filter(name == "Brazil")

brazil_cities <- world.cities %>% 
  filter(country.etc == "Brazil",
         pop >= 1E6) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  arrange(desc(pop))

leaflet() %>% 
  addPolygons(data = brazil_sf,
              fillColor = "darkolivegreen",
              fillOpacity = 1,
              color = "black",
              weight = 1
              ) %>% 
  addCircleMarkers(data = brazil_cities,
                   label = ~name)
