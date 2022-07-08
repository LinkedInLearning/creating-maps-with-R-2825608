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
  slice_max(pop, n = 5) %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326)

germany_cities_df <- germany_cities %>% 
  st_drop_geometry() %>% 
  bind_cols(st_coordinates(germany_cities)) %>% 
  slice(c(1, n()))

ggplot() +
  geom_sf(data = germany_sf,
          fill = "darkolivegreen3") +
  geom_sf(data = germany_cities,
          aes(size = pop,
              fill = as.logical(capital)),
          shape = 21) +
  geom_label_repel(data = germany_cities_df,
                   aes(x = X,
                       y = Y,
                       label = name)) +
  scale_size_area(labels = scales::number_format(scale = 1E-6,
                                                 suffix = " Million")) +
  scale_fill_manual(values = c("TRUE" = "gold",
                               "FALSE" = "purple")) +
  theme_void()




