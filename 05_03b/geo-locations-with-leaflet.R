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
  arrange(desc(pop)) %>% 
  mutate(city_type = ifelse(capital == 1, "Capital City", "City"))

city_type_pal <- colorFactor(c("Gold", "Purple"), brazil_cities$city_type)

leaflet() %>% 
  addPolygons(data = brazil_sf,
              fillColor = "darkolivegreen",
              fillOpacity = 1,
              color = "black",
              weight = 1
  ) %>% 
  addCircleMarkers(data = brazil_cities,
                   weight = 1,
                   color = "black",
                   fillColor = ~city_type_pal(city_type),
                   fillOpacity = 1) %>% 
  addLegend(pal = city_type_pal,
            data = brazil_cities,
            values = ~city_type,
            title = NA,
            opacity = 1)


