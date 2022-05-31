library(tidyverse)
library(sf)
library(janitor)
library(tigris)

oregon_sf <- counties(state = "OR") %>% 
  clean_names()

oregon_hospitals <- read_csv("oregon-hospital-locations.csv")

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

multnomah_bbox <- oregon_sf %>% 
  filter(name == "Multnomah") %>% 
  st_bbox() %>% 
  as.list()


ggplot() +
  geom_sf(data = oregon_sf,
          fill = "darkolivegreen3",
          color = "white") +
  geom_sf(data = filter(oregon_hospitals, county == "Multnomah"),
          color = "darkblue") +
  coord_sf(xlim = c(multnomah_bbox$xmin, multnomah_bbox$xmax),
           ylim = c(multnomah_bbox$ymin, multnomah_bbox$ymax)) +
  labs(title = "Hospital locations in Multnomah") +
  theme_void()