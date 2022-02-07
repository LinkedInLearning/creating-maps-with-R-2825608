library(tidyverse)
library(sf)
library(rnaturalearthdata)
library(maps)
library(janitor)
library(readxl)
library(ggspatial)
library(leaflet)
library(ggrepel)
library(scales)
library(rmapshaper)
library(tigris)
library(colorblindr)

# Data from https://www.electricaldirect.co.uk/blog/most-popular-streaming-services
most_popular_streaming_service <- read_csv("data/most-popular-streaming-service.csv") %>% 
  clean_names()

us_contiguous <- states() %>% 
  clean_names() %>% 
  mutate(statefp = as.numeric(statefp)) %>% 
  filter(statefp < 60,
         !statefp %in% c(2, 15)) %>% 
  ms_simplify()

us_most_popular_streaming_sf <- us_contiguous %>% 
  left_join(most_popular_streaming_service,
            by = c("name" = "state"))

colors_services <- list(
  "Amazon Prime" = "#2A96D9",
  "ESPN" = "#cc5445",
  "Hulu" = "#35B12E",
  "Netflix" = "grey30"
)

st_as_sf(us_most_popular_streaming_sf %>% st_union())

gg_us_streaming_services <- us_most_popular_streaming_sf %>% 
  filter(!is.na(streaming_service)) %>% 
  ggplot() +
  geom_sf(aes(fill = streaming_service),
          color = "white",
          size = 0.1) +
  geom_sf(data = st_as_sf(us_most_popular_streaming_sf %>% st_union()),
          fill = "transparent",
          color = "black") +
  scale_fill_manual(values = colors_services,
                    name = NULL) +
  labs(title = "Most popular streaming service by state") +
  theme_void(base_size = 24) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top", legend.margin=margin(t = 10, b = -10, unit = "pt"))

ggsave("gg_us_streaming_services.png",
       gg_us_streaming_services,
       width = 8,
       height = 4)


gg_for_cvd <- us_most_popular_streaming_sf %>% 
  filter(!is.na(streaming_service)) %>% 
  ggplot() +
  geom_sf(aes(fill = streaming_service),
          color = "white",
          size = 0.1) +
  geom_sf(data = st_as_sf(us_most_popular_streaming_sf %>% st_union()),
          fill = "transparent",
          color = "black") +
  scale_fill_manual(values = colors_services,
                    name = NULL) +
  theme_void(base_size = 24)
  
gg_cvd_grid <- cvd_grid(gg_for_cvd)

ggsave("gg_cvd_grid.png",
       gg_cvd_grid)




