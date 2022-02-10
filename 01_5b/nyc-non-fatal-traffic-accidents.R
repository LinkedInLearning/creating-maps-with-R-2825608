library(sf)
library(tidyverse)
library(mapview)
library(lubridate)
library(janitor)
library(ggspatial)
library(systemfonts)
source("make-hex-bins.R")

nyc_traffic_accidents_2020 <- read_csv("data/nyc-traffic-accidents_2020.csv")

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

## ==== Make scatter ====

bbox_nyc_zip <- as.list(st_bbox(nyc_zips_sf))

gg_accidents_geo_scatter <- ggplot() +
  geom_sf(data = nyc_zips_sf,
          fill = "gray90",
          color = "transparent") +
  geom_sf(
    data = nyc_traffic_accidents_2020_sf,
    color = "blue",
    size = 0.1
  ) +
  geom_sf(data = nyc_zips_sf,
          fill = "transparent",
          color = "gray80",
          size = 0.01) +
  geom_sf(data = st_as_sf(st_union(nyc_zips_sf)),
          fill = "transparent",
          color = "gray30") +
  coord_sf(xlim = c(bbox_nyc_zip$xmin, bbox_nyc_zip$xmax),
           ylim = c(bbox_nyc_zip$ymin, bbox_nyc_zip$ymax)) +
  theme_void()


gg_accidents_geo_scatter %>% 
  ggsave("gg_accidents_geo_scatter.png",
         .,
         width = 4.46 * 2,
         height = 4.09 * 2)



## ==== Make choropleth ====

nyc_zips_with_accidents_choropleth <- nyc_zips_sf %>% 
  mutate(accidents_in_zip = lengths(st_covers(nyc_zips_sf, nyc_traffic_accidents_2020_sf)))

gg_nyc_zip_choropleth <- ggplot() +
  geom_sf(data = nyc_zips_sf,
          fill = "gray90",
          color = "transparent") +
  geom_sf(data = nyc_zips_with_accidents_choropleth,
          fill = "grey80",
          color = "white") +
  geom_sf(
    data = nyc_zips_with_accidents_choropleth,
    aes(fill = accidents_in_zip),
    color = "grey80",
    size = 0.1
  ) +
  geom_sf(data = nyc_zips_sf,
          fill = "transparent",
          color = "white",
          size = 0.1) +
  geom_sf(data = st_as_sf(st_union(nyc_zips_sf)),
          fill = "transparent",
          color = "gray30") +
  scale_fill_viridis_c(name = NULL,
                       labels = scales::number_format(big.mark = ",")) +
  annotate(
    "text",
    x = bbox_nyc_zip$xmin + 3E4,
    y = bbox_nyc_zip$ymin + 1.2E5,
    size = 8,
    family = "Helvetica",
    label = "Traffic accidents in\nNew York during 2020"
  ) +
  theme_void(base_size = 20,
             base_family = "Helvetica") +
  theme(
    plot.title = element_text(),
    legend.position = c(0.2, 0.65),
    legend.direction = "horizontal",
    legend.key.width = unit(1.5, "cm")
  )

gg_nyc_zip_choropleth %>% 
  ggsave("gg_nyc_hexbin_detailed_choropleth.png",
         .,
         width = 4.46 * 2,
         height = 4.09 * 2)

## ==== Make hexbins ====

nyc_zips_hex_sf <- nyc_zips_sf %>% 
  make_hex_bins("honeycomb",
                3800) %>% 
  st_cast("POLYGON") 

nyc_zips_with_accidents_hex_sf <- nyc_zips_hex_sf %>% 
  mutate(accidents_in_zip = lengths(st_covers(nyc_zips_hex_sf, nyc_traffic_accidents_2020_sf)))

gg_nyc_hexbin_detailed_choropleth <- ggplot() +
  geom_sf(data = nyc_zips_sf,
          fill = "gray90",
          color = "transparent") +
  geom_sf(data = nyc_zips_with_accidents_hex_sf,
          fill = "grey80",
          color = "white") +
  geom_sf(
    data = nyc_zips_with_accidents_hex_sf,
    aes(fill = accidents_in_zip),
    color = "grey80",
    size = 0.1
  ) +
  geom_sf(data = nyc_zips_sf,
          fill = "transparent",
          color = "white",
          size = 0.1) +
  geom_sf(data = st_as_sf(st_union(nyc_zips_sf)),
          fill = "transparent",
          color = "gray30") +
  scale_fill_viridis_c(name = NULL,
                       labels = scales::number_format(big.mark = ",")) +
  annotate(
    "text",
    x = bbox_nyc_zip$xmin + 3E4,
    y = bbox_nyc_zip$ymin + 1.2E5,
    size = 8,
    family = "Helvetica",
    label = "Traffic accidents in\nNew York during 2020"
  ) +
  theme_void(base_size = 20,
             base_family = "Helvetica") +
  theme(
    plot.title = element_text(),
    legend.position = c(0.2, 0.65),
    legend.direction = "horizontal",
    legend.key.width = unit(1.5, "cm")
  )

gg_nyc_hexbin_detailed_choropleth %>% 
  ggsave("gg_nyc_hexbin_detailed_choropleth.png",
         .,
         width = 4.46 * 2,
         height = 4.09 * 2)
 

