library(sf)
library(rnaturalearthdata)
library(maps)
library(leaflet)
library(leaflet.extras)
library(tidyverse)

germany_cities <- world.cities %>% 
  filter(country.etc %in% c("Germany")) %>% 
  group_by(country.etc) %>% 
  slice_max(pop, n = 5) %>% 
  ungroup() %>% 
  mutate(capital = as.logical(capital)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

germany_sf <- countries50 %>% 
  st_as_sf() %>% 
  filter(name %in% c("Germany"))

popup_city <- function(city, pop){
  
  str_glue("{city}",
           "<br>",
           "Population: {scales::number(pop, big.mark = ',')}")
  
}

leaflet() %>% 
  addPolygons(data = germany_sf,
              fillColor = "#92C548",
              fillOpacity = 1,
              weight = 1,
              color = "black") %>% 
  addMarkers(data = germany_cities,
             popup = ~popup_city(name, pop)) %>% 
  setMapWidgetStyle(style = list(background = "antiquewhite"))
