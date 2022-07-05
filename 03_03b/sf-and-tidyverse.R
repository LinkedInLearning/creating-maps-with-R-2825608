library(sf)
library(tidyverse)
library(mapview)

us_states <- read_sf("data/us-states")

state_population <- read_csv("data/state-population.csv")

