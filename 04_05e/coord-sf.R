library(tidyverse)
library(sf)
library(janitor)
library(tigris)

oregon_sf <- counties(state = "OR") %>% 
  clean_names()

oregon_hospitals <- read_csv("data/oregon-hospital-locations.csv")

oregon_hospitals <- oregon_hospitals %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)


ggplot() +
  geom_sf(data = oregon_sf,
          fill = "darkolivegreen3",
          color = "white") +
  geom_sf(data = oregon_hospitals,
          color = "darkblue") +
  labs(title = "Hospital locations in Oregon") +
  theme_void()


bbox_multnomah <- oregon_sf %>% 
  filter(name == "Multnomah") %>% 
  st_bbox() %>% 
  as.list()

ggplot() +
  geom_sf(data = oregon_sf,
          fill = "darkolivegreen3",
          color = "white") +
  geom_sf(data = oregon_hospitals,
          color = "darkblue") +
  labs(title = "Hospital locations in Oregon") +
  coord_sf(xlim = c(bbox_multnomah$xmin, bbox_multnomah$xmax),
           ylim = c(bbox_multnomah$ymin, bbox_multnomah$ymax)) +
  theme_void()
