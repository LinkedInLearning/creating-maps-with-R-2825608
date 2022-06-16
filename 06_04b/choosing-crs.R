library(tidyverse)
library(rnaturalearthdata)
library(sf)

oceania_sf <- countries110 %>% 
  st_as_sf() %>% 
  filter(name %in% c("Australia", "New Zealand"))
