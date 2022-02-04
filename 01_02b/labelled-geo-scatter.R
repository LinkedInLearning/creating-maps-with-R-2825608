library(sf)
library(rnaturalearthdata)
library(maps)
library(tigris)
library(rmapshaper)
library(tidyverse)

capitals <- world.cities %>% 
  filter(country.etc %in% c("UK", "Ireland")) %>% 
  filter(capital == 1)

large_cities <- world.cities %>% 
  filter(country.etc %in% c("UK", "Ireland"),
         capital != 1) %>% 
  group_by(country.etc) %>% 
  slice_max(pop, n = 5)

cities_sf <- large_cities %>%
  bind_rows(capitals) %>% 
  mutate(capital = as.logical(capital)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

cities_tib <- cities_sf %>% 
  st_drop_geometry() %>% 
  bind_cols(as_tibble(st_coordinates(cities_sf)))

ireland_and_uk <- countries50 %>% 
  st_as_sf() %>% 
  filter(name %in% c("United Kingdom", "Ireland"))


gg_uk_labeled <- ggplot() +
  geom_sf(data = ireland_and_uk,
          fill = "darkolivegreen3") +
  geom_sf(data = cities_sf,
          aes(colour = capital),
          size = 5) +
  geom_label_repel(data = cities_tib,
                   aes(x = X,
                       y = Y,
                       label = name),
                   # min.segment.length = unit(0.1, 'lines'),
                   # point.padding = unit(10, 'lines'),
                   nudge_y = c(0.5, -0.5)) +
  scale_colour_manual(labels = c("TRUE" = "Capital City", "FALSE" = "City"),
                      values = c("TRUE" = "orange", "FALSE" = "purple"),
                      name = "") +
  theme_void() +
  theme(legend.text = element_text(size = 24),
        legend.position = "top")

ggsave("gg_uk_labeled.png",
       gg_uk_labeled,
       width = 7,
       height = 8)
