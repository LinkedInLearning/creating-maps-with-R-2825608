library(cartogram)
library(leaflet)
library(janitor)
library(sf)
library(rnaturalearthdata)
library(tidyverse)

theme_map_cjh <- function(base_size = 20, ...){
  
  theme_ipsum(base_size = base_size) +
    theme(panel.grid.major = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          ...)
  
}
world_geo_crs_compromise <- countries110 %>% 
  st_as_sf() %>% 
  clean_names() %>% 
  select(name, continent, pop_est) %>% 
  filter(name != "Antarctica") %>% 
  st_transform(crs = 'PROJCS["ProjWiz_Custom_Robinson",
 GEOGCS["GCS_WGS_1984",
  DATUM["D_WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Robinson"],
 PARAMETER["False_Easting",0.0],
 PARAMETER["False_Northing",0.0],
 PARAMETER["Central_Meridian",0],
 UNIT["Meter",1.0]]')


world_cartogram_compromise <- cartogram_cont(world_geo_crs_compromise, "pop_est", itermax = 20)

world_cartogram_compromise$continent

gg_cartogram_whole_world <- ggplot() +
  geom_sf(data = st_transform(world_cartogram_compromise, crs = 4326),
          aes(fill = continent),
          color = "white",
          size = 0.1) +
  theme_map_cjh(base_size = 8) +
  theme(legend.position = "top")

gg_cartogram_whole_world %>% 
  ggsave("gg_cartogram_whole_world.png",
         .,
         width = 7,
         height = 4)
