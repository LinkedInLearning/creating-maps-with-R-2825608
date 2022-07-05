library(sf)
library(readxl)
library(tidygeocoder)
library(mapview)
library(tidyverse)

capitol_buildings <- read_excel("data/us-capitol-buildings.xlsx")

capitol_buildings_geocoded <- capitol_buildings %>% 
  geocode(
    street = address_capitol,
    state = state,
    method = "iq"
  )

capitol_buildings_geocoded %>% 
  filter(is.na(long))

capitol_buildings_geocoded %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) %>% 
  mapview()
