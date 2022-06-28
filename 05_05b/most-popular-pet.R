library(tidyverse)
library(sf)
library(janitor)
library(rmapshaper)
library(tigris)
library(leaflet)

most_popular_pets <- read_csv("data/pet-searches-by-state.csv") %>% 
  clean_names()

us_contiguous <- states() %>% 
  clean_names() %>% 
  mutate(statefp = as.numeric(statefp)) %>% 
  filter(statefp < 60,
         !statefp %in% c(2, 15)) %>% 
  ms_simplify()

us_most_popular_pets <- us_contiguous %>% 
  left_join(most_popular_pets,
            by = c("name" = "state"))

colors_pets <-
  c(
    "Hamster" = "darkorchid1",
    "Guinea pig" = "darkcyan",
    "Chinchilla" = "chocolate",
    "Sugar glider" = "darkred",
    "Bearded dragon" = "gold"
  )

leaflet() %>% 
  addPolygons(data = us_most_popular_pets,
              label = ~name)
