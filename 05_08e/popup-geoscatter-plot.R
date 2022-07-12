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
  slice_max(pop, n = 5) %>% 
  st_as_sf(coord = c("long", "lat"),
           crs = 4326)

popup_city_pop <- function(city, population){
  
  paste(
    "<b>City:</b>", city,
    "<br>",
    "<b>Population</b>", scales::number(population, big.mark = ",")
  )
  
}

germany_cities <- germany_cities %>% 
  mutate(city_type = ifelse(capital == 1, "Capital City", "City"))

pal_city_type <- colorFactor(c("gold", "purple"), c("Capital City", "City"))

leaflet() %>% 
  addPolygons(data = germany_sf,
              weight = 1,
              color = "black",
              fillColor = "darkolivegreen",
              fillOpacity = 1) %>% 
  addCircleMarkers(data = germany_cities,
                   weight = 1,
                   color = "black",
                   fillColor = ~pal_city_type(city_type),
                   fillOpacity = 1,
                   popup = ~popup_city_pop(name, pop)) %>% 
  addLegend(data = germany_cities,
            pal = pal_city_type,
            values = ~city_type,
            opacity = 1) %>% 
  setMapWidgetStyle(style = list(background = "white"))
