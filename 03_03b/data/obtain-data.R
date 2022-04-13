library(tidycensus)
library(tigris)
library(sf)
library(tidyverse)

us_states <- states()

dir.create("data/us-states")

us_states %>% 
  write_sf("data/us-states/us-states.shp")

load_variables(2019, "acs5")


state_population <- get_acs("state",
        variable = "B01001_001") %>% 
  select(NAME, estimate) %>%
  rename(state = NAME,
         population = estimate)

state_population %>% 
  write_csv("data/state-population.csv")
