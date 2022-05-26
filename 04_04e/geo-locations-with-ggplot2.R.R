library(tidyverse)
library(sf)
library(maps)
library(rnaturalearthdata)
library(ggrepel)

brazil_sf <- countries110 %>% 
  st_as_sf() %>% 
  filter(name == "Brazil")

brazil_cities <- world.cities %>% 
  filter(country.etc == "Brazil",
         pop >= 1E6) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  arrange(desc(pop))


brazil_cities_tib <- brazil_cities %>% 
  st_drop_geometry() %>% 
  bind_cols(st_coordinates(brazil_cities)) %>% 
  slice(1, n())

ggplot() +
  geom_sf(data = brazil_sf,
          fill = "darkolivegreen3") +
  geom_sf(data = brazil_cities,
          shape = 21,
          aes(fill = as.logical(capital),
              size = pop)) +
  geom_label_repel(data = brazil_cities_tib,
                   aes(x = X,
                       y = Y,
                       label = name,
                       point.size = scales::rescale(pop, c(1, 10))),
                   nudge_x = c(-5, 5)) +
  scale_size_area(max_size = 10,
                  name = "City population",
                  labels = scales::number_format(scale = 1E-6,
                                                 suffix = " Million")) +
  scale_fill_manual(name = "",
                    values = c("TRUE" = "gold",
                               "FALSE" = "purple"),
                    labels = c("TRUE" = "Capital City",
                               "FALSE" = "City")) +
  guides(fill = guide_legend(override.aes = list(size = 5)),
         size = guide_legend(override.aes = list(fill = "gray70"))) +
  theme_void()

