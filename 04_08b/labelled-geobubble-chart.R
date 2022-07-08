library(tidyverse)
library(sf)
library(maps)
library(rnaturalearthdata)
library(ggrepel)

germany_sf <- countries50 %>% 
  st_as_sf() %>% 
  filter(name == "Germany")

germany_cities <- world.cities %>% 
  filter(country.etc == "Germany") %>% 
  slice_max(pop, n = 5)


ggplot() +
  geom_sf(data = germany_sf,
          fill = "darkolivegreen3") +
  theme_void()
