library(tigris)
library(sf)
library(rmapshaper)
library(tidyverse)
library(janitor)

us_states <- states() %>% 
  clean_names() %>% 
  mutate(statefp = as.numeric(statefp)) %>% 
  filter(statefp < 60,
         !statefp %in% c(2, 15)) %>% 
  ms_simplify()


state_coastline <- read_csv("data/km-of-coastline.csv")

us_coastline <- us_states %>% 
  left_join(state_coastline,
            by = c("name" = "state"))


ggplot() +
  geom_sf(data = us_coastline) +
  theme_void()

