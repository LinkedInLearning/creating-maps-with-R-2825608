library(tidyverse)
library(rnaturalearthdata)
library(maps)
library(sf)

sf_use_s2(FALSE)
world_cities <- world.cities %>% 
  as_tibble() %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326)

city_circles_4326 <- world_cities %>% 
  top_n(10, pop) %>% 
  st_buffer(1000E3 / 111320)

coastline_sf <- coastline50 %>% 
  st_as_sf() %>% 
  st_wrap_dateline()



gg_crs_4326_circles <- ggplot() +
  geom_sf(data = coastline_sf) +
  geom_sf(data = city_circles_4326,
          alpha = 0.4,
          fill = "red",
          colour = "red") +
  coord_sf(crs = 3395) +
  theme_void()

gg_crs_4326_circles %>% 
  ggsave("gg_crs_4326_circles.png",
         .,
         width = 12,
         height = 8,
         bg = "white")
