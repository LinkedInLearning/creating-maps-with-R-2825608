library(sf)
library(tidyverse)
library(mapview)
library(lubridate)
library(janitor)
# devtools::install_github("ropensci/bikedata")
library(bikedata)
source("make-hex-bins.R")


london_sf <- read_sf("london_boroughs") %>% 
  mutate(area = st_area(geometry),
         area = as.numeric(area))

london_hex_sf <- london_sf %>% 
  st_union() %>% 
  st_as_sf() %>% 
  make_hex_bins("honeycomb")

london_fishnet_sf <- london_sf %>% 
  st_union() %>% 
  st_as_sf() %>% 
  make_hex_bins("fishnet")

london_pixel_sf <- london_sf %>% 
  st_union() %>% 
  st_as_sf() %>% 
  make_hex_bins("pixel")

plot_binned_choropleth <- function(bin_data, region_sf){
  
ggplot() +
  geom_sf(data = bin_data,
          color = "grey80") +
  geom_sf(data = st_as_sf(st_union(region_sf)),
          fill = "transparent") +
  theme_void()

}

london_hex_sf %>% 
  plot_binned_choropleth(london_sf)


london_fishnet_sf %>% 
  plot_binned_choropleth(london_sf)

nyc_hexbin_sf %>% 
  plot_binned_choropleth(nyc_borders_sf)


nyc_zips_sf %>% 
  st_union() %>% 
  st_as_sf() %>% 
  make_hex_bins("fishnet") %>% 
  plot_binned_choropleth(nyc_zips_sf)

nyc_zips_sf %>% 
  st_union() %>% 
  st_as_sf() %>% 
  make_hex_bins("pixel") %>% 
  plot_binned_choropleth(nyc_zips_sf) %>% 
  ggsave("gg_nyc_pixel_binning.png",
         .,
         width = 6.71 * 2,
         height = 4.82 * 2)

nyc_zips_sf %>% 
  st_union() %>% 
  st_as_sf() %>% 
  make_hex_bins("fishnet") %>% 
  plot_binned_choropleth(nyc_zips_sf) %>% 
  ggsave("gg_nyc_fishnet_binning.png",
         .,
         width = 6.71 * 2,
         height = 4.82 * 2)

nyc_zips_sf %>% 
  st_union() %>% 
  st_as_sf() %>% 
  make_hex_bins("honeycomb") %>% 
  plot_binned_choropleth(nyc_zips_sf) %>% 
  ggsave("gg_nyc_honeycomb_binning.png",
         .,
         width = 6.71 * 2,
         height = 4.82 * 2)





