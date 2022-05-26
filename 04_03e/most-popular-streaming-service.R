library(tidyverse)
library(sf)
library(janitor)
library(rmapshaper)
library(tigris)

most_popular_streaming_service <- read_csv("data/most-popular-streaming-service.csv") %>% 
  clean_names()

order_service_popularity <- most_popular_streaming_service %>% 
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
  filter(!is.na(streaming_service))


colors_services <- list(
  "Amazon Prime" = "#2A96D9",
  "ESPN" = "#cc5445",
  "Hulu" = "#35B12E",
  "Netflix" = "grey30"
)

colors_services <- colors_services[order_service_popularity]

ggplot() +
  geom_sf(data = us_most_popular_streaming_service,
          aes(fill = streaming_service),
          color = "white",
          size = 0.2) +
  scale_fill_manual(values = colors_services,
                    name = "") +
  labs(title = "Most popular streaming service by state") +
  theme_void() +
  theme(legend.position = "top")
