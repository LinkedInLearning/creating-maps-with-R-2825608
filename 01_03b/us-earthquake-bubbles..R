library(tidyverse)
library(sf)
library(rnaturalearthdata)
library(maps)
library(here)
library(ggspatial)
library(janitor)
library(readxl)
library(tidygeocoder)
library(ggspatial)
library(leaflet)
library(ggrepel)
library(scales)
library(lubridate)
library(rmapshaper)
library(tigris)

quakes_kaggle <- read_csv("data/kaggle-earthquakes.csv") %>% 
  janitor::clean_names() %>% 
  mutate(date = mdy(date)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

us_contiguous <- states(resolution = "20m") %>% 
  clean_names() %>% 
  ms_simplify() %>% 
  mutate(statefp = as.numeric(statefp)) %>% 
  filter(statefp < 60,
         !statefp %in% c(2, 15))

quakes_us <- quakes_kaggle %>% 
  slice(unlist(st_covers(st_transform(us_contiguous, crs = 4326), quakes_kaggle)))

ggplot() +
  geom_sf(data = us_contiguous,
          fill = "darkolivegreen3",
          color = "white") +
  geom_sf(data = st_as_sf(st_union(us_contiguous)),
          fill = "transparent",
          color = "black") +
  geom_sf(data = quakes_us,
          aes(size = magnitude),
          shape = 21,
          fill = "purple") +
  # scale_size(range = c(1, 10)) +
  scale_radius() +
  labs(title = "Earthquakes in the contiguous US") +
  theme_void(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 20))
