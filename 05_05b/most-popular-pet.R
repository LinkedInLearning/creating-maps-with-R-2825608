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
  list(
    "Hamster" = "#AE76A6",
    "Guinea pig" = "#92BFB1",
    "Chinchilla" = "#694A38",
    "Sugar glider" = "#A61C3C",
    "Bearded dragon" = "#FFAD05"
  )

leaflet() %>% 
  addPolygons(data = us_most_popular_pets,
              label = ~name)
