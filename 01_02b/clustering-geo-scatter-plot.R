library(sf)
library(tigris)
library(maps)
library(janitor)
library(leaflet)
library(leaflet.extras)
library(tigris)
library(tidyverse)

us_contiguous <- states(resolution = "20m") %>% 
  clean_names() %>% 
  mutate(statefp = as.numeric(statefp)) %>% 
  filter(statefp < 60,
         !statefp %in% c(2, 15))

quakes_us <- quakes_kaggle %>% 
  slice(unlist(st_covers(st_transform(us_contiguous, crs = 4326), quakes_kaggle)))

us_cities <- world.cities %>% 
  filter(country.etc == "USA") %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) %>% 
  st_transform(crs = st_crs(us_contiguous)) %>% 
  mutate(capital = ifelse(capital == 1, "Capital", "City"))

us_contiguous_cities <- us_cities %>% 
  slice(unlist(st_covers(us_contiguous, us_cities)))

pal_capital_city <- colorFactor(c("#7570b3", "#e7298a"), us_contiguous_cities$capital)


leaflet() %>% 
  addTiles(options = tileOptions(opacity = 0)) %>%
  addPolygons(data = us_contiguous,
              fillColor = "#92C548",
              fillOpacity = 1,
              weight = 1,
              color = "black") %>% 
  addCircleMarkers(data = us_contiguous_cities,
                   clusterOptions = markerClusterOptions(),
                   fillColor = ~pal_capital_city(capital),
                   fillOpacity = 1,
                   weight = 1) %>% 
  addLegend(pal = pal_capital_city,
            values = ~capital,
            data = us_contiguous_cities,
            opacity = 1, 
            title = "City type")

