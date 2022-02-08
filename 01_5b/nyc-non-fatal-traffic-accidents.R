library(sf)
library(tidyverse)
library(mapview)
library(lubridate)
library(janitor)
library(systemfonts)
source("make-hex-bins.R")

nyc_traffic_accidents_2020 <- read_csv("data/nyc-traffic-accidents_2020.csv")

# Source: https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u/data?no_mobile=true
nyc_zips_sf <- read_sf("data/ny-zipcodes") %>% 
  clean_names() %>% 
  mutate(area = st_area(geometry),
         area = as.numeric(area))


gg_nyc_borough_areas <- ggplot() +
  geom_sf(data = nyc_zips_sf,
          aes(fill = area)) +
  scale_fill_viridis_c(labels = scales::number_format(scale = 1E-6),
                       name = expression(paste("Area (", km^2, ")"))) +
  theme_void(base_size = 20) +
  theme(legend.position = "top",
        legend.key.width = unit(3, "cm"))

gg_nyc_borough_areas %>% 
  ggsave("gg_nyc_borough_areas.png",
         .,
         width = 6.71 * 2,
         height = 4.82 * 2)

nyc_traffic_accidents_2020_sf <- nyc_traffic_accidents_2020 %>% 
  filter(year(crash_date) == 2020) %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(crs = st_crs(nyc_zips_sf))

nyc_zips_with_accidents_2020_sf <- nyc_zips_sf %>% 
  mutate(accidents_in_region = lengths(st_covers(nyc_zips_sf, nyc_traffic_accidents_2020_sf)))
  
nyc_zips_with_accidents_2020_sf %>% 
  mapview(zcol = "accidents_in_region")


nyc_outline_sf <- nyc_zips_sf %>% 
  st_union() %>% 
  st_as_sf()

nyc_hexbin_sf <- nyc_outline_sf  %>% 
  make_hex_bins("honeycomb",
                3600) %>% 
  st_cast("POLYGON")

nyc_hexbin_with_accidents_2020_sf <- nyc_hexbin_sf %>% 
  mutate(accidents_in_region = lengths(st_covers(nyc_hexbin_sf, nyc_traffic_accidents_2020_sf))) 
  
nyc_hexbin_with_accidents_2020_sf %>% 
  mapview(zcol = "accidents_in_region")

ggplot() +
  geom_sf(data = nyc_outline_sf,
          fill = "grey80",
          color = "black") +
  geom_sf(data = nyc_hexbin_with_accidents_2020_sf,
          aes(fill = accidents_in_region),
          # alpha = 0.5,
          color = "grey30",
          size = 0.1) +
  geom_sf(data = nyc_zips_sf,
          fill = "transparent",
          color = "white",
          size = 0.1) +
  scale_fill_viridis_c(name = "Traffic incidents\nin 2020") +
  theme_void()


gg_nyc_zip_choropleth <- ggplot() +
  geom_sf(data = nyc_outline_sf,
          fill = "grey80",
          color = "black") +
  # geom_sf(data = nyc_hexbin_with_accidents_2020_sf,
  #         aes(fill = accidents_in_region),
  #         # alpha = 0.5,
  #         color = "grey30",
  #         size = 0.1) +
  geom_sf(data = nyc_zips_with_accidents_2020_sf,
          aes(fill = accidents_in_region),
          color = "white",
          size = 0.1) +
  scale_fill_viridis_c(name = NULL,
                       labels = scales::number_format(big.mark = ",")) +
  annotate("text",
           x = 913129 + 2E4,
           y = 272710.9 - 3E4,
           size = 8,
           family = "Helvetica",
           label = "Traffic accidents in\nNew York during 2020") +
  theme_void(base_size = 20,
             base_family = "Helvetica") +
  theme(plot.title = element_text(vjust = -25,
                                  family = "Helvetica"),
        legend.position = c(0.15, 0.67),
        legend.direction = "horizontal",
        legend.key.width=unit(2,"cm"))

gg_nyc_zip_choropleth

gg_nyc_zip_choropleth %>% 
  ggsave("gg_nyc_zip_choropleth.png",
         .,
         width = 6.71 * 2,
         height = 4.82 * 2)





gg_nyc_hexbin_detailed_choropleth <- ggplot() +
  geom_sf(data = nyc_outline_sf,
          fill = "grey90",
          color = "transparent") +
  geom_sf(data = nyc_hexbin_with_accidents_2020_sf,
          aes(fill = accidents_in_region),
          # alpha = 0.5,
          color = "grey30",
          size = 0.1) +
  geom_sf(data = nyc_zips_sf,
          fill = "transparent",
          color = "white",
          size = 0.1) +
  geom_sf(data = nyc_outline_sf,
          fill = "transparent",
          color = "grey30",
          size = 0.5) +
  scale_fill_viridis_c(name = NULL,
                       labels = scales::number_format(big.mark = ",")) +
  annotate("text",
           x = 913129 + 2E4,
           y = 272710.9 - 3E4,
           size = 8,
           family = "Helvetica",
           label = "Traffic accidents in\nNew York during 2020") +
  theme_void(base_size = 20,
             base_family = "Helvetica") +
  theme(plot.title = element_text(vjust = -25,
                                  family = "Helvetica"),
        legend.position = c(0.15, 0.67),
        legend.direction = "horizontal",
        legend.key.width=unit(2,"cm"))

gg_nyc_hexbin_detailed_choropleth %>% 
  ggsave("gg_nyc_hexbin_detailed_choropleth.png",
         .,
         width = 6.71 * 2,
         height = 4.82 * 2)



gg_nyc_hexbin_simple_choropleth <- ggplot() +
  geom_sf(data = nyc_outline_sf,
          fill = "grey90",
          color = "transparent") +
  geom_sf(data = nyc_hexbin_with_accidents_2020_sf,
          aes(fill = accidents_in_region),
          # alpha = 0.5,
          color = "grey30",
          size = 0.1) +
  geom_sf(data = nyc_outline_sf,
          fill = "transparent",
          color = "grey20") +
  scale_fill_viridis_c(name = NULL,
                       labels = scales::number_format(big.mark = ",")) +
  annotate("text",
           x = 913129 + 2E4,
           y = 272710.9 - 3E4,
           size = 8,
           family = "Helvetica",
           label = "Traffic accidents in\nNew York during 2020") +
  theme_void(base_size = 20,
             base_family = "Helvetica") +
  theme(plot.title = element_text(vjust = -25,
                                  family = "Helvetica"),
        legend.position = c(0.15, 0.67),
        legend.direction = "horizontal",
        legend.key.width=unit(2,"cm"))

gg_nyc_hexbin_simple_choropleth %>% 
  ggsave("gg_nyc_hexbin_simple_choropleth.png",
         .,
         width = 6.71 * 2,
         height = 4.82 * 2)





