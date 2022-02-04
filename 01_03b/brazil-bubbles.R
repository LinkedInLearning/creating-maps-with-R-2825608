library(tidyverse)
library(sf)
library(rnaturalearthdata)
library(maps)
library(janitor)
library(leaflet)
library(ggrepel)
library(scales)

brazil_cities <- world.cities %>% 
  filter(country.etc == "Brazil",
         pop > 1e6) %>% 
  mutate(capital = as.logical(capital)) %>% 
  arrange(desc(pop))

brazil_cities_sf <- brazil_cities %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

brazil_sf <- countries50 %>% 
  st_as_sf() %>% 
  filter(name == "Brazil")

gg_brazil_bubble_basic <- ggplot() +
  geom_sf(data = brazil_sf,
          fill = "darkolivegreen3") +
  geom_sf(data = brazil_cities_sf,
          aes(size = pop),
          fill = "#bc5090",
          shape = 21) +
  scale_size_area(
    max_size = 20,
    labels = scales::number_format(suffix = " Million",
                                   scale = 1e-6),
    name = "City size"
  ) +
  theme_void(base_size = 18) +
  theme(legend.text = element_text(size = 28),
        legend.title = element_text(size = 28),
        legend.spacing.y = unit(1, "cm"), title = element_text(size = 28)) +
  guides(size = guide_legend(override.aes = list(fill = "#bc5090")),
         fill = guide_legend(override.aes = list(size = 10))) +
  labs(title = "Cities in Brazil with more than 1 Million residents")

ggsave("gg_brazil_bubble_basic.png",
       gg_brazil_bubble_basic)




brazil_cities_tib <- brazil_cities_sf %>% 
  st_drop_geometry() %>% 
  bind_cols(as_tibble(st_coordinates(brazil_cities_sf)))




gg_brazil_bubble_and_fill <- ggplot() +
  geom_sf(data = brazil_sf,
          fill = "darkolivegreen3") +
  geom_sf(data = brazil_cities_sf,
          aes(fill = capital,
              size = pop),
          shape = 21) +
  scale_size_area(
    max_size = 20,
    labels = scales::number_format(suffix = " Million",
                                   scale = 1e-6),
    name = "City size"
  ) +
  scale_fill_manual(
    labels = c("TRUE" = "Capital City",
               "FALSE" = "City"),
    values = c("TRUE" = "#ffa600",
               "FALSE" = "#bc5090"),
    name = ""
  ) +
  theme_void(base_size = 18) +
  theme(legend.text = element_text(size = 28),
        legend.title = element_text(size = 28),
        legend.spacing.y = unit(1, "cm"), title = element_text(size = 28)) +
  guides(size = guide_legend(override.aes = list(fill = "#bc5090")),
         fill = guide_legend(override.aes = list(size = 10))) +
  labs(title = "Cities in Brazil with more than 1 Million residents")

ggsave("gg_brazil_bubble_and_fill.png",
       gg_brazil_bubble_and_fill)

biggest_smallest_city <-  brazil_cities_tib %>% 
  arrange(pop) %>% 
  slice(c(1, nrow(.)))

gg_brazil_everything <- ggplot() +
  geom_sf(data = brazil_sf,
          fill = "darkolivegreen3") +
  geom_sf(data = brazil_cities_sf,
          aes(fill = capital,
              size = pop),
          shape = 21) +
  geom_label_repel(
    data = biggest_smallest_city,
    aes(x = X,
        y = Y,
        label = name),
    point.size = scales::rescale(biggest_smallest_city$pop, c(1, 20)),
    size = 6
  ) +
  scale_size_area(
    max_size = 20,
    labels = scales::number_format(suffix = " Million",
                                   scale = 1e-6),
    name = "City size"
  ) +
  scale_fill_manual(
    labels = c("TRUE" = "Capital City",
               "FALSE" = "City"),
    values = c("TRUE" = "#ffa600",
               "FALSE" = "#bc5090"),
    name = ""
  ) +
  theme_void(base_size = 18) +
  theme(legend.text = element_text(size = 28),
        legend.title = element_text(size = 28),
        legend.spacing.y = unit(1, "cm"), title = element_text(size = 28)) +
  guides(size = guide_legend(override.aes = list(fill = "#bc5090")),
         fill = guide_legend(override.aes = list(size = 10))) +
  labs(title = "Cities in Brazil with more than 1 Million residents")

ggsave("gg_brazil_everything.png",
       gg_brazil_everything)








