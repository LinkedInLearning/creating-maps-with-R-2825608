library(tidyverse)
library(sf)
library(janitor)
library(rmapshaper)
library(tigris)
library(leaflet)
library(leaflet.extras)

most_popular_streaming_service <- read_csv("data/most-popular-streaming-service.csv") %>% 
  clean_names()

order_streaming_service <- most_popular_streaming_service %>% 
  count(streaming_service, sort = TRUE) %>% 
  pull(streaming_service)


us_contiguous <- states() %>% 
  clean_names() %>% 
  mutate(statefp = as.numeric(statefp)) %>% 
  filter(statefp < 60,
         !statefp %in% c(2, 15)) %>% 
  ms_simplify()

us_most_popular_streaming_service <- us_contiguous %>% 
  left_join(most_popular_streaming_service,
            by = c("name" = "state")) %>% 
  mutate(streaming_service = fct_relevel(streaming_service, order_streaming_service))

colors_services <- c(
  "Amazon Prime" = "#2A96D9",
  "ESPN" = "#cc5445",
  "Hulu" = "#35B12E",
  "Netflix" = "grey30"
)

pal_streaming_service <- colorFactor(colors_services[order_streaming_service], us_most_popular_streaming_service$streaming_service, na.color = "pink")


leaflet() %>% 
  addPolygons(data = us_most_popular_streaming_service,
              weight = 1,
              color = "black",
              label = ~name,
              fillColor = ~pal_streaming_service(streaming_service),
              fillOpacity = 1) %>% 
  addLegend(data = us_most_popular_streaming_service,
            pal = pal_streaming_service,
            values = ~streaming_service,
            opacity = 1,
            na.label = "(No data)") %>% 
  setMapWidgetStyle(style = list(background = "lightblue"))
