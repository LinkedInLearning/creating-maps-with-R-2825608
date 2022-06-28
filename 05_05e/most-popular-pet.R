library(tidyverse)
library(sf)
library(janitor)
library(rmapshaper)
library(tigris)
library(leaflet)

most_popular_pets <- read_csv("data/pet-searches-by-state.csv") %>% 
  clean_names()

order_pets <- most_popular_pets %>% 
  count(pet, sort = TRUE) %>% 
  pull(pet)


us_contiguous <- states() %>% 
  clean_names() %>% 
  mutate(statefp = as.numeric(statefp)) %>% 
  filter(statefp < 60,
         !statefp %in% c(2, 15)) %>% 
  ms_simplify()

us_most_popular_pets <- us_contiguous %>% 
  left_join(most_popular_pets,
            by = c("name" = "state")) %>% 
  mutate(pet = fct_relevel(pet, order_pets))

colors_pets <-
  c(
    "Hamster" = "darkorchid1",
    "Guinea pig" = "darkcyan",
    "Chinchilla" = "chocolate",
    "Sugar glider" = "darkred",
    "Bearded dragon" = "gold"
  )

pal_pets <- colorFactor(colors_pets[order_pets], us_most_popular_pets$pet,
                        na.color = "pink")

leaflet() %>% 
  addPolygons(data = us_most_popular_pets,
              label = ~name,
              weight = 1,
              color = "black",
              fillColor = ~pal_pets(pet),
              fillOpacity = 1
  ) %>% 
  addLegend(data = us_most_popular_pets,
            pal = pal_pets,
            values = ~pet,
            na.label = "Washington DC")

