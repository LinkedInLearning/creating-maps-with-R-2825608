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

brazil_cities <- brazil_cities %>% 
  mutate(city_type = ifelse(capital == 1, "Capital City", "City"))

pal_city_type <- colorFactor(c("gold", "purple"), c("Capital City", "City"))

leaflet() %>% 
  addPolygons(data = brazil_sf,
              fillColor = "darkolivegreen",
              fillOpacity = 1,
              color = "black",
              weight = 1
  ) %>% 
  addCircleMarkers(data = brazil_cities,
                   weight = 1,
                   fillColor = ~pal_city_type(city_type),
                   color = "black",
                   fillOpacity = 1,
                   radius = ~scales::rescale(pop, c(1, 10)),
                   label = ~name) %>% 
  addLegend(data = brazil_cities,
            pal = pal_city_type,
            values = ~city_type,
            opacity = 1)
