library(tidyverse)
library(sf)
library(janitor)
library(rmapshaper)
library(tigris)
library(leaflet)

most_popular_streaming_service <- read_csv("data/most-popular-streaming-service.csv") %>% 
  clean_names()

us_contiguous <- states() %>% 
  clean_names() %>% 
  mutate(statefp = as.numeric(statefp)) %>% 
  filter(statefp < 60,
         !statefp %in% c(2, 15)) %>% 
  ms_simplify()

us_most_popular_streaming_service <- us_contiguous %>% 
  left_join(most_popular_streaming_service,
            by = c("name" = "state"))

colors_services <- c(
  "Amazon Prime" = "#2A96D9",
  "ESPN" = "#cc5445",
  "Hulu" = "#35B12E",
  "Netflix" = "grey30"
)

