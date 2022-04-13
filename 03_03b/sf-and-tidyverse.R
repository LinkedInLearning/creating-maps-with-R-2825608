library(sf)
library(tidyverse)

us_states <- read_sf("data/us-states")

state_population <- read_csv("data/state-population.csv")