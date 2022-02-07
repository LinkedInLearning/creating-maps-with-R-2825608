library(tidyverse)
library(sf)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(maps)
library(janitor)
library(readxl)
library(ggspatial)
library(leaflet)
library(ggrepel)
library(scales)
library(rmapshaper)

uk_110m <- countries110 %>% 
  st_as_sf() %>% 
  filter(name == "United Kingdom")

gg_uk_110m <- ggplot() + 
  geom_sf(data = uk_110m) +
  theme_void()

ggsave("gg_uk_110m.png",
       gg_uk_110m)

bbox_uk <- as.list(st_bbox(uk_110m))

uk_10m <- countries10 %>% 
  st_as_sf() %>% 
  filter(NAME == "United Kingdom")

gg_uk_10m <- ggplot() +
  geom_sf(data = uk_10m) +
  coord_sf(xlim = c(bbox_uk$xmin, bbox_uk$xmax),
           ylim = c(bbox_uk$ymin, bbox_uk$ymax)) +
  theme_void()

ggsave("gg_uk_10m.png",
       gg_uk_10m)


uk_counties_only <- read_sf("data/Counties_(December_2020)_EN_BFC/") %>% 
  st_transform(crs = 4326) %>% 
  ms_simplify()

gg_uk_counties_only <- ggplot() +
  geom_sf(data = uk_10m,
          fill = "white") +
  geom_sf(data = uk_counties_only,
          fill = "darkolivegreen3") +
  coord_sf(xlim = c(bbox_uk$xmin, bbox_uk$xmax),
           ylim = c(bbox_uk$ymin, bbox_uk$ymax)) +
  theme_void()

ggsave("gg_uk_counties_only.png",
       gg_uk_counties_only)


uk_counties_and_ua <- read_sf("data/Counties_and_Unitary_Authorities_(December_2020)_UK_BFC/") %>% 
  st_transform(crs = 4326) %>% 
  ms_simplify()

gg_uk_counties_and_ua <- ggplot() +
  geom_sf(data = uk_10m,
          fill = "white") +
  geom_sf(data = uk_counties_and_ua,
          fill = "darkolivegreen3") +
  coord_sf(xlim = c(bbox_uk$xmin, bbox_uk$xmax),
           ylim = c(bbox_uk$ymin, bbox_uk$ymax)) +
  theme_void()

ggsave("gg_uk_counties_and_ua.png",
       gg_uk_counties_and_ua)




