library(tidyverse)
library(sf)
library(maps)
library(rnaturalearthdata)
library(leaflet)
library(leaflet.extras)

germany_sf <- countries50 %>% 
  st_as_sf() %>% 
  filter(name == "Germany")

germany_cities <- world.cities %>% 
  filter(country.etc == "Germany") %>% 
  slice_max(pop, n = 5)

popup_city_pop <- function(city, population){
  
  paste(
    "<b>City:</b>", city,
    "<br>",
    "<b>Population</b>", scales::number(population, big.mark = ",")
  )
  
}

leaflet() %>% 
  addPolygons(data = germany_sf,
              weight = 1,
              color = "black",
              fillColor = "darkolivegreen",
              fillOpacity = 1) %>% 
  addCircleMarkers(data = germany_cities,
                   weight = 1,
                   color = "black",
                   fillColor = "purple",
                   fillOpacity = 1,
                   radius = ~scales::rescale(sqrt(pop), c(5, 20)),
                   popup = ~popup_city_pop(name, pop)) %>% 
  setMapWidgetStyle(style = list(background = "white"))
