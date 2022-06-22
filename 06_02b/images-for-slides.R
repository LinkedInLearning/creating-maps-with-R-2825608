library(tidyverse)
library(rnaturalearthdata)
library(sf)

countries_sf <- countries110 %>%
  st_as_sf()

theme_cjh_map <- function(){
  theme_void() +
    theme(panel.background = element_rect(fill = "#98d7ef"), panel.border = element_blank())
}

# sf::sf_use_s2(FALSE)

gg_web_mercator <- countries_sf %>%
  st_transform(crs = 3857) %>% 
  ggplot() +
  geom_sf(fill = "#D2B48C",
          color = "transparent") +
  coord_sf(crs = 3857, expand = TRUE) +
  theme_cjh_map() +
  coord_sf(ylim = c(-pi * 6378137, pi * 6378137))

gg_robinson <- countries_sf %>%
  ggplot() +
  geom_sf(fill = "#D2B48C",
          color = "transparent") +
  coord_sf(crs = "+proj=robin") +
  theme_cjh_map()

gg_pall_peters <- countries_sf %>%
  ggplot() +
  geom_sf(fill = "#D2B48C",
          color = "transparent") +
  coord_sf(crs = "+proj=cea", expand = FALSE) +
  theme_cjh_map()

gg_web_mercator %>% 
  ggsave("gg_web_mercator.png",
         .,
         width = 10,
         height = 10)


gg_robinson %>% 
  ggsave("gg_robinson.png",
         .,
         width = 20,
         height = 8)

gg_pall_peters %>% 
  ggsave("gg_pall_peters.png",
         .,
         width = 20,
         height = 8)


