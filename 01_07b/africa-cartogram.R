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

# Shapefiles downloaded from https://github.com/nasa/World-Wind-Java/tree/master/WorldWind/testData/shapefiles
nasa_shapefiles <- read_sf("data/nasa-shapefiles", layer = "TM_WORLD_BORDERS_SIMPL-0.2") %>% 
  clean_names()

africa_sf <- nasa_shapefiles %>% 
  filter(region == 2L) %>% 
  st_transform(crs = 32733)

suggest_crs(africa_sf)

africa_pop_cartogram_contig <- cartogram_cont(africa_sf, "pop2005")
africa_pop_cartogram_non_contig <- cartogram_ncont(africa_sf, "pop2005")


gg_cartogram_contig_africa <- ggplot() +
  geom_sf(data = africa_pop_cartogram_contig,
          aes(fill = pop2005),
          color = "white") +
  scale_fill_viridis_c(labels = scales::number_format(scale = 1E-6, suffix = " Million"),
                       name = NULL) +
  theme_map_cjh() +
  theme(legend.position = "top",
        legend.key.width = unit(2, "cm"))

gg_cartogram_contig_africa_no_legend <- ggplot() +
  geom_sf(data = africa_pop_cartogram_contig,
          color = "white",
          fill = "darkolivegreen") +
  scale_fill_viridis_c(labels = scales::number_format(scale = 1E-6, suffix = " Million"),
                       name = NULL) +
  theme_map_cjh() +
  theme(legend.position = "top",
        legend.key.width = unit(2, "cm"))


gg_cartogram_non_contig_africa_no_legend <- ggplot() +
  geom_sf(data = africa_pop_cartogram_non_contig,
          fill = "darkolivegreen",
          color = "white") +
  geom_sf(data = africa_sf,
          fill = "transparent",
          color = "black") +
  scale_fill_viridis_c(labels = scales::number_format(scale = 1E-6, suffix = " Million"),
                       name = NULL) +
  theme_map_cjh() +
  theme(legend.position = "top",
        legend.key.width = unit(2, "cm"))


gg_cartogram_contig_africa %>% 
  ggsave("gg_cartogram_contig_africa.png",
         .,
         width = 7,
         height = 7)

gg_cartogram_contig_africa_no_legend %>% 
  ggsave("gg_cartogram_contig_africa_no_legend.png",
         .,
         width = 7,
         height = 7)

gg_cartogram_non_contig_africa_no_legend %>% 
  ggsave("gg_gg_cartogram_non_contig_africa_no_legend.png",
         .,
         width = 7,
         height = 7)




gg_choropleth_africa <- ggplot() +
  geom_sf(data = africa_sf,
          aes(fill = pop2005),
          color = "white") +
  scale_fill_viridis_c(labels = scales::number_format(scale = 1E-6, suffix = " Million"),
                       name = NULL) +
  theme_map_cjh() +
  theme(legend.position = "top",
        legend.key.width = unit(2, "cm"))

gg_choropleth_africa %>% 
  ggsave("gg_choropleth_africa.png",
         .,
         width = 7,
         height = 7)
