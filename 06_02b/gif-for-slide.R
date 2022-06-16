library(tidyverse)
library(sf)
library(rnaturalearth)  
library(gganimate)
# remotes::install_github("thomasp85/transformr")
library(transformr)

# Code modified from https://jakubnowosad.com/posts/2018-10-23-map-distortion/

world_orig <- ne_countries(returnclass = "sf")

world_poly <- world_orig %>% 
  select(geometry) %>% 
  st_cast("POLYGON")

world <- world_poly %>% 
  st_crop(xmin = -180, ymin = -86, xmax = 180, ymax = 84) %>% 
  st_transform(crs = 3857)

world_areas <- world %>% 
  st_transform(crs = "+proj=moll") %>% 
  st_area()

map_areas <- world %>% 
  st_set_crs(NA) %>% 
  st_area()

world_scaled <- world %>% 
  mutate(scale = 1 / (sqrt(map_areas / world_areas))) %>% 
  mutate(scale = as.numeric(scale / max(scale)))

scaler <- function(x, y, z) {
  (x - z) * y + z
}

world_geom <- st_geometry(world) 
world_center = st_centroid(world_geom)

world_transf <- pmap(list(world_geom, world_scaled$scale, world_center), scaler) %>% 
  st_sfc(crs = st_crs(world)) %>% 
  st_sf()

world$state = 1
world_transf$state = 2
worlds = rbind(world, world_transf)

worlds_anim <- ggplot() +
  geom_sf(data = worlds, fill = "#D2B48C") +
  coord_sf(ylim = c(-pi * 6378137, pi * 6378137)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white"), panel.border = element_blank()) +
  transition_states(state, transition_length = 5, state_length = 2) + 
  ease_aes("cubic-in-out")

worlds_animate <- animate(worlds_anim, nframes = 50)

anim_save("worlds_animate.gif", worlds_animate)



