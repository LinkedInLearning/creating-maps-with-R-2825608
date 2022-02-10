library(sf)
library(tidyverse)
library(mapview)
library(lubridate)
library(janitor)
# devtools::install_github("ropensci/bikedata")
library(bikedata)
source("make-hex-bins.R")


london_sf <- read_sf("data/london_boroughs") %>% 
  mutate(area = st_area(geometry),
         area = as.numeric(area))

london_hex_sf <- london_sf %>% 
  st_union() %>% 
  st_as_sf() %>% 
  make_hex_bins("honeycomb")

london_bike_stations <- lo_stns %>% 
  as_tibble() %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326) %>% 
  st_transform(crs = st_crs(london_sf))


london_bikes_by_borough <- london_sf %>% 
  mutate(bike_stations_in_zip = lengths(st_covers(london_sf, london_bike_stations))) %>% 
  mutate(bike_stations_in_zip = ifelse(bike_stations_in_zip == 0, NA_integer_, bike_stations_in_zip))
  
london_bikes_by_borough %>% 
  filter(bike_stations_in_zip > 0) %>% 
  mapview(zcol = "bike_stations_in_zip")

london_boroughs_hex_sf <- london_bikes_by_borough %>% 
  filter(!is.na(bike_stations_in_zip)) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  make_hex_bins("honeycomb") %>% 
  st_cast("POLYGON")



london_boroughs_with_bikes_hex_sf <- london_boroughs_hex_sf %>% 
  mutate(bike_stations_in_zip = lengths(st_covers(london_boroughs_hex_sf, london_bike_stations))) %>% 
  mutate(bike_stations_in_zip = ifelse(bike_stations_in_zip == 0, NA_integer_, bike_stations_in_zip))

bbox_boroughs_with_bikes <- as.list(st_bbox(london_boroughs_with_bikes_hex_sf))


gg_london_borough_areas <- ggplot() +
  geom_sf(data = london_sf,
          aes(fill = area)) +
  scale_fill_viridis_c(labels = scales::number_format(scale = 1E-6),
                       name = expression(paste("Area (", km^2, ")"))) +
  theme_void(base_size = 20) +
  theme(legend.position = "top",
        legend.key.width = unit(3, "cm"))

gg_london_borough_areas %>% 
  ggsave("gg_london_borough_areas.png",
         .,
         width = 6.71 * 2,
         height = 4.82 * 2)


gg_lonbikes_hex_zoomed_detailed <- ggplot() +
  geom_sf(data = london_sf,
          fill = "gray90",
          color = "transparent") +
  geom_sf(data = london_boroughs_with_bikes_hex_sf,
          fill = "grey80",
          color = "black") +
  geom_sf(data = london_boroughs_with_bikes_hex_sf,
          aes(fill = bike_stations_in_zip),
          color = "white",
          size = 0.1) +
  geom_sf(data = london_sf,
          fill = "transparent",
          color = "black") +
  scale_fill_viridis_c(name = NULL,
                       labels = scales::number_format(big.mark = ","),
                       breaks = c(0, 2, 4, 6, 8)
                       ) +
  annotate("rect",
           xmin = bbox_boroughs_with_bikes$xmin + 1.59E4,
           xmax = bbox_boroughs_with_bikes$xmin + 2.5E4,
           ymin = bbox_boroughs_with_bikes$ymin + 2.8E3,
           ymax = bbox_boroughs_with_bikes$ymin + 7E3,
           fill = "white") +
  annotate("label",
           x = bbox_boroughs_with_bikes$xmin + 2E4,
           y = bbox_boroughs_with_bikes$ymin + 6E3,
           size = 8,
           family = "Helvetica",
           label = "Hireable bike\nlocations in London") +
  coord_sf(xlim = c(bbox_boroughs_with_bikes$xmin, bbox_boroughs_with_bikes$xmax),
           ylim = c(bbox_boroughs_with_bikes$ymin, bbox_boroughs_with_bikes$ymax)) +
  theme_void(base_size = 20,
             base_family = "Helvetica") +
  theme(plot.title = element_text(vjust = -25,
                                  family = "Helvetica"),
        legend.position = c(0.8, 0.23),
        legend.direction = "horizontal",
        legend.key.width=unit(2,"cm"))

gg_lonbikes_hex_zoomed_detailed

gg_lonbikes_hex_zoomed_detailed %>% 
  ggsave("gg_lonbikes_hex_zoomed_detailed.png",
         .,
         width = 6.71 * 2,
         height = 4.82 * 2)


gg_lonbikes_hex_all_london_detailed <- ggplot() +
  geom_sf(data = london_sf,
          fill = "gray90",
          color = "transparent") +
  geom_sf(data = london_boroughs_with_bikes_hex_sf,
          fill = "grey80",
          color = "black") +
  geom_sf(data = london_boroughs_with_bikes_hex_sf,
          aes(fill = bike_stations_in_zip),
          color = "white",
          size = 0.1) +
  geom_sf(data = london_sf,
          fill = "transparent",
          color = "black") +
  scale_fill_viridis_b(name = NULL,
                       labels = scales::number_format(big.mark = ","),
                       breaks = c(0, 2, 4, 6, 8)
  ) +
  labs(title = "Hireable bike locations in London") +
  theme_void(base_size = 20,
             base_family = "Helvetica") +
  theme(plot.title = element_text(),
        legend.position = "right",
        # legend.position = c(0.8, 0.23),
        # legend.direction = "vertical"
        legend.key.height=unit(2,"cm")
        )

gg_lonbikes_hex_all_london_detailed %>% 
  ggsave("gg_lonbikes_hex_all_london_detailed.png",
         .,
         width = 6.71 * 2,
         height = 4.82 * 2)


gg_lonbikes_borough_zoomed_detailed <- ggplot() +
  geom_sf(data = filter(london_bikes_by_borough, !is.na(bike_stations_in_zip)),
          aes(fill = bike_stations_in_zip),
          color = "white",
          size = 0.1) +
  geom_sf(data = filter(london_bikes_by_borough, is.na(bike_stations_in_zip)),
          fill = "transparent",
          color = "black") +
  # geom_sf(data = london_sf,
  #         fill = "transparent",
  #         color = "black") +
  scale_fill_viridis_c(name = NULL,
                       labels = scales::number_format(big.mark = ","),
                       na.value="white"
  ) +
  annotate("rect",
           xmin = bbox_boroughs_with_bikes$xmin + 1.59E4,
           xmax = bbox_boroughs_with_bikes$xmin + 2.5E4,
           ymin = bbox_boroughs_with_bikes$ymin + 2.8E3,
           ymax = bbox_boroughs_with_bikes$ymin + 7E3,
           fill = "gray90") +
  annotate("label",
           x = bbox_boroughs_with_bikes$xmin + 2E4,
           y = bbox_boroughs_with_bikes$ymin + 6E3,
           size = 8,
           family = "Helvetica",
           label = "Hireable bike\nlocations in London") +
  coord_sf(xlim = c(bbox_boroughs_with_bikes$xmin, bbox_boroughs_with_bikes$xmax),
           ylim = c(bbox_boroughs_with_bikes$ymin, bbox_boroughs_with_bikes$ymax)) +
  theme_void(base_size = 20,
             base_family = "Helvetica") +
  theme(plot.title = element_text(vjust = -25,
                                  family = "Helvetica"),
        legend.position = c(0.8, 0.23),
        legend.direction = "horizontal",
        legend.key.width=unit(2,"cm"))

gg_lonbikes_borough_zoomed_detailed %>% 
  ggsave("gg_lonbikes_borough_zoomed_detailed.png",
         .,
         width = 6.71 * 2,
         height = 4.82 * 2)

st_as_sf(st_union(london_bikes_by_borough)) %>% 
  ggplot() +
  geom_sf()


gg_lonbikes_borough_everything_detailed <- ggplot() +
  geom_sf(data = filter(london_bikes_by_borough, is.na(bike_stations_in_zip)),
          fill = "gray90",
          color = "white") +
  # geom_sf(data = london_bikes_by_borough,
  #         fill = "grey80",
  #         color = "black") +
  geom_sf(data = filter(london_bikes_by_borough, !is.na(bike_stations_in_zip)),
          aes(fill = bike_stations_in_zip),
          color = "white") +
  geom_sf(data = st_as_sf(st_union(london_bikes_by_borough)),
          fill = "transparent",
          color = "black",
          weight = 0.3) +
  scale_fill_viridis_c(name = NULL,
                       labels = scales::number_format(big.mark = ",")
  ) +
  labs(title = "Hireable bike locations in London") +
  theme_void(base_size = 20,
             base_family = "Helvetica") +
  theme(plot.title = element_text(),
        legend.position = "right",
        # legend.position = c(0.8, 0.23),
        # legend.direction = "vertical"
        legend.key.height=unit(2,"cm")
  )


gg_lonbikes_borough_everything_detailed %>% 
  ggsave("gg_lonbikes_borough_everything_detailed.png",
         .,
         width = 6.71 * 2,
         height = 4.82 * 2)
