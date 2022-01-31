library(sf)
library(tigris)
library(rmapshaper)
library(janitor)
library(tidyverse)

us_contiguous <- states(resolution = "20m") %>% 
  clean_names() %>% 
  ms_simplify() %>% 
  mutate(statefp = as.numeric(statefp)) %>% 
  filter(statefp < 60,
         !statefp %in% c(2, 15))




gg_us_regions <- us_contiguous %>% 
  ggplot() +
  geom_sf(aes(fill = region),
          color = "white",
          size = 0.5) +
  scale_fill_viridis_d(labels = c("1" = "Northeast",
                                  "2" = "Central",
                                  "3" = "Southern",
                                  "4" = "Western"),
                       name = NULL) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_void() +
  theme(legend.position = "top")

ggsave("gg_us_regions.png",
       gg_us_regions,
       width = 8,
       height = 4)
