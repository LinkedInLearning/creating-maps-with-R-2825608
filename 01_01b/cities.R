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

city_data <- large_cities %>%
  bind_rows(capitals) %>% 
  mutate(capital = as.logical(capital)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

ireland_and_uk <- countries50 %>% 
  st_as_sf() %>% 
  filter(name %in% c("United Kingdom", "Ireland"))

gg_cities_shape <- ggplot() +
  geom_sf(data = ireland_and_uk,
          fill = "darkolivegreen3") +
  geom_sf(data = city_data,
          aes(shape = capital),
          size = 5) +
  scale_shape_manual(values = c("FALSE" = 8, "TRUE" = 16),
                     labels = c("TRUE" = "Capital City", "FALSE" = "City"),
                     name = "") +
  theme_void() +
  theme(legend.text = element_text(size = 24),
        legend.position = "top")

ggsave("gg_cities_shape.png",
       gg_cities_shape,
       width = 7,
       height = 8)

gg_cities_color <- ggplot() +
  geom_sf(data = ireland_and_uk,
          fill = "darkolivegreen3") +
  geom_sf(data = city_data,
          aes(colour = capital),
          size = 5) +
  scale_colour_manual(labels = c("TRUE" = "Capital City", "FALSE" = "City"),
                      values = c("TRUE" = "orange", "FALSE" = "purple"),
                      name = "") +
  theme_void() +
  theme(legend.text = element_text(size = 24),
        legend.position = "top")

ggsave("gg_cities_color.png",
       gg_cities_color,
       width = 7,
       height = 8)

brazil_cities <- world.cities %>% 
  filter(country.etc == "Brazil") %>% 
  slice_max(pop, n = 10) %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326)

brazil_sf <- countries50 %>% 
  st_as_sf() %>% 
  filter(name == "Brazil")

gg_brazil_cities_size <- ggplot() +
  geom_sf(data = brazil_sf,
          fill = "darkolivegreen3") +
  geom_sf(data = brazil_cities,
          aes(size = pop),
          color = "#2C2863") +
  scale_size_area(labels = scales::number_format(scale = 1E-6,
                                                 suffix = " Million"),
                  name = "Population:",
                  max_size = 10) +
  theme_void() +
  theme(legend.position = "top",
        legend.text = element_text(size = 15))

ggsave("gg_brazil_cities_size.png",
       gg_brazil_cities_size,
       width = 7,
       height = 8)

gg_brazil_cities_fill <- ggplot() +
  geom_sf(data = brazil_sf,
          fill = "darkolivegreen3") +
  geom_sf(data = brazil_cities,
          aes(fill = pop),
          size = 10,
          shape = 21) +
  scale_fill_viridis_c(labels = scales::number_format(scale = 1E-6,
                                                 suffix = " Million"),
                  name = "Population:") +
  theme_void() +
  theme(legend.position = "top",
        legend.text = element_text(size = 15),
        legend.key.width = unit(2.5, "cm"))

ggsave("gg_brazil_cities_fill.png",
       gg_brazil_cities_fill,
       width = 7,
       height = 8)




us_contiguous <- states(resolution = "20m") %>% 
  clean_names() %>% 
  ms_simplify() %>% 
  mutate(statefp = as.numeric(statefp)) %>% 
  filter(statefp < 60,
         !statefp %in% c(2, 15))

us_border <- us_contiguous %>% 
  st_union()

us_cities <- world.cities %>% 
  filter(country.etc == "USA") %>% 
  slice_max(pop, n = 10) %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326)

gg_us_cities_size <- ggplot() +
  geom_sf(data = us_contiguous,
          color = "white",
          fill = "darkolivegreen3") +
  geom_sf(data = us_border,
         fill = "transparent") +
  geom_sf(data = us_cities %>% 
            arrange(desc(pop)),
          aes(size = pop),
          shape = 21,
          fill = "#238EA1",
          color = "black") +
  scale_size_area(labels = scales::number_format(scale = 1E-6,
                                                 suffix = " Million"),
                  name = "Population:",
                  max_size = 10) +
  theme_void() +
  theme(legend.position = "top",
        legend.text = element_text(size = 15))

ggsave("gg_us_cities_size.png",
       gg_us_cities_size,
       width = 8,
       height = 4)  

gg_us_cities_fill <- ggplot() +
  geom_sf(data = us_contiguous,
          color = "white",
          fill = "darkolivegreen3") +
  geom_sf(data = us_border,
          fill = "transparent") +
  geom_sf(data = us_cities %>% 
            arrange(desc(pop)),
          aes(fill = pop),
          shape = 21,
          color = "black",
          size = 8) +
  scale_fill_viridis_c(labels = scales::number_format(scale = 1E-6,
                                                      suffix = " Million"),
                       name = "Population:") +
  theme_void() +
  theme(legend.position = "top",
        legend.text = element_text(size = 15),
        legend.key.width = unit(2.5, "cm"))

ggsave("gg_us_cities_fill.png",
       gg_us_cities_fill,
       width = 8,
       height = 4)




