library(sf)
library(tidyverse)
library(mapview)

us_states <- read_sf("data/us-states")

state_population <- read_csv("data/state-population.csv")


us_population_sf <- us_states %>% 
  left_join(state_population,
            by = c("NAME" = "state"))

us_population_sf %>% 
  filter(population < 1E6)

us_region_pop <- us_population_sf %>% 
  group_by(REGION) %>% 
  summarise(region_population = sum(population))

us_region_pop %>% 
  mapview()
