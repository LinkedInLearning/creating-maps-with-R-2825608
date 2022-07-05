library(sf)
library(tidyverse)
library(mapview)

us_states <- read_sf("data/us-states")

state_population <- read_csv("data/state-population.csv")

us_state_pop <- us_states %>% 
  left_join(state_population,
            by = c("NAME" = "state"))

us_state_pop %>% 
  select(NAME, population) %>% 
  filter(population < 1E6)


us_region_population <- us_state_pop %>% 
  group_by(REGION) %>% 
  summarise(region_population = sum(population))

us_region_population %>% 
  mapview(zcol = "region_population")
