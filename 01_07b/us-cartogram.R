library(cartogram)
library(leaflet)
library(janitor)
library(sf)
library(tidycensus)
library(crsuggest)
library(hrbrthemes)
library(tidyverse)

theme_map_cjh <- function(base_size = 20, ...){
  
  theme_ipsum(base_size = base_size) +
    theme(panel.grid.major = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          ...)
  
}


vars_acs5_2019 <- load_variables(2019, "acs5")

us_state_pop_sf <- get_acs("state",
        year = 2019,
        geometry = TRUE,
        variables = c("total_population" = "B01001_001")) %>% 
  clean_names() %>% 
  mutate(geoid = as.numeric(geoid))

us_contiguous_pop_sf <- us_state_pop_sf %>% 
  filter(geoid < 60,
         !geoid %in% c(2L, 15L)) %>% 
  st_transform(crs = 'PROJCS["ProjWiz_Custom_Albers",
 GEOGCS["GCS_WGS_1984",
  DATUM["D_WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Albers"],
 PARAMETER["False_Easting",0.0],
 PARAMETER["False_Northing",0.0],
 PARAMETER["Central_Meridian",-100.2832031],
 PARAMETER["Standard_Parallel_1",28.8581834],
 PARAMETER["Standard_Parallel_2",46.1823779],
 PARAMETER["Latitude_Of_Origin",37.5202807],
 UNIT["Meter",1.0]]')

us_contiguous_cartogram_pop <- cartogram(us_contiguous_pop_sf, "estimate")

gg_cartogram_us_contiguous <- ggplot() +
  geom_sf(data = us_contiguous_cartogram_pop,
          aes(fill = estimate),
          color = "white") +
  scale_fill_viridis_c(labels = scales::number_format(scale = 1E-6, suffix = " Million"),
                       name = "State Population") +
  theme_map_cjh() +
  theme(legend.position = "top",
        legend.key.width = unit(3, "cm"))

gg_cartogram_us_contiguous %>% 
  ggsave("gg_cartogram_us_contiguous.png",
         .,
         width = 2 * 6.56,
         height = 2 * 4.09)




gg_choropleth_us_contiguous <- ggplot() +
  geom_sf(data = us_contiguous_pop_sf,
          aes(fill = estimate),
          color = "white") +
  scale_fill_viridis_c(labels = scales::number_format(scale = 1E-6, suffix = " Million"),
                       name = "State Population") +
  theme_map_cjh() +
  theme(legend.position = "top",
        legend.key.width = unit(3, "cm"))


gg_choropleth_us_contiguous %>% 
  ggsave("gg_choropleth_us_contiguous.png",
         .,
         width = 2 * 6.56,
         height = 2 * 4.09)
