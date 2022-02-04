library(sf)
library(janitor)
library(rnaturalearthdata)
library(maps)
library(tigris)
library(rmapshaper)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(mapview)
library(tigris)
library(ggrepel)
library(tidyverse)

germany_cities_sf <- world.cities %>% 
  filter(country.etc %in% c("Germany")) %>% 
  group_by(country.etc) %>% 
  slice_max(pop, n = 5) %>% 
  ungroup() %>% 
  mutate(capital = as.logical(capital)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

germany_sf <- countries50 %>% 
  st_as_sf() %>% 
  filter(name %in% c("Germany"))

gg_germany_geo_scatter <- ggplot() +
  geom_sf(data = germany_sf,
          fill = "darkolivegreen3") +
  geom_sf(data = germany_cities_sf,
          aes(colour = capital),
          size = 5) +
  scale_colour_manual(labels = c("TRUE" = "Capital City", "FALSE" = "City"),
                      values = c("TRUE" = "orange", "FALSE" = "purple"),
                      name = "") +
  labs(title = "Top 5 largest cities in Germany") +
  theme_void(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 20))

ggsave("gg_germany_geo_scatter.png",
       gg_germany_geo_scatter,
       width = 8.19,
       height = 9)

germany_cities_tib <- germany_cities_sf %>% 
  st_drop_geometry() %>% 
  bind_cols(as_tibble(st_coordinates(germany_cities_sf)))


leaflet() %>% 
  addPolygons(data = germany_sf,
              fillColor = "#92C548",
              fillOpacity = 1,
              weight = 1,
              color = "black") %>% 
  addMarkers(data = germany_cities_sf) %>% 
  setMapWidgetStyle(style = list(background = "antiquewhite"))

quakes_kaggle <- read_csv("data/kaggle-earthquakes.csv") %>% 
  janitor::clean_names() %>% 
  mutate(date = mdy(date)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

countries_sf %>% 
  filter(name == "United States")

us_contiguous <- states(resolution = "20m") %>% 
  clean_names() %>% 
  ms_simplify() %>% 
  mutate(statefp = as.numeric(statefp)) %>% 
  filter(statefp < 60,
         !statefp %in% c(2, 15))

quakes_us <- quakes_kaggle %>% 
  slice(unlist(st_covers(st_transform(us_contiguous, crs = 4326), quakes_kaggle)))


gg_earthquakes_us <- ggplot() +
  geom_sf(data = us_contiguous,
          fill = "darkolivegreen3",
          color = "white") +
  geom_sf(data = st_as_sf(st_union(us_contiguous)),
          fill = "transparent",
          color = "black") +
  geom_sf(data = quakes_us,
          shape = 21,
          fill = "purple") +
  labs(title = "Earthquakes in the contiguous US") +
  theme_void(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 20))

ggsave("gg_earthquakes_us.png",
       gg_earthquakes_us,
       width = 8,
       height = 4)

