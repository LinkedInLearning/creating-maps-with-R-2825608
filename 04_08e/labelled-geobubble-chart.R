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

germany_cities_sf <- germany_cities %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326)

germany_cities_to_label <- germany_cities %>% 
  arrange(pop) %>% 
  slice(1, n())

ggplot() +
  geom_sf(data = germany_sf,
          fill = "darkolivegreen3") +
  geom_sf(data = germany_cities_sf,
          aes(size = pop,
              fill = as.logical(capital)),
          shape = 21) +
  scale_fill_manual(values = c("TRUE" = "gold",
                               "FALSE" = "purple"),
                    labels = c("TRUE" = "Capital City",
                               "FALSE" = "City"),
                    name = "") +
  geom_label_repel(data = germany_cities_to_label,
                   aes(x = long,
                       y = lat,
                       label = name,
                       point.size = scales::rescale(pop, c(1, 10)))) +
  scale_size_area(max_size = 10,
                  labels = scales::number_format(scale = 1E-6,
                                                 suffix = " Million"),
                  name = "") +
  guides(fill = guide_legend(override.aes = list(size = 5)),
         size = guide_legend(override.aes = list(fill = "lightgrey"))) +
  theme_void()

