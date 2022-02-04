library(maps)
library(rnaturalearthdata)
library(sf)
library(scales)
library(ggrepel)
library(tidyverse)


brazil_top_10 <- world.cities %>% 
  as_tibble() %>% 
  filter(country.etc == "Brazil") %>%
  slice_max(pop, n = 10) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  mutate(capital = as.logical(capital)) %>% 
  arrange(desc(pop)) %>% 
  mutate(city = name)


label_city_shape <- function(city, pop = NA){
  
  paste0(city, "\n", number(pop, suffix = " M", scale = 1e-6, accuracy = 0.1))
  
}

oblong_horiz_city_positions <- brazil_top_10 %>%
  mutate(x = c(1, 3, 5, 7, 9, 1, 3, 5, 7, 9),
         y = c(5, 5, 5, 5, 5, 3, 3, 3, 3, 3))

labelled_cities <- oblong_horiz_city_positions %>% 
  st_drop_geometry() %>% 
  filter(city %in% c("Sao Paulo", "Salvador", "Brasilia", "Belem"))


gg_bubble_radius <- oblong_horiz_city_positions %>%
  ggplot(aes(x, y, size = pop)) +
  coord_fixed(xlim = c(0, 10),
              ylim = c(2, 6),
              expand = FALSE) +
  geom_point(color = "#af8dc3")  +
  scale_radius(range = c(1, 30),
               name = "City population\n(vary by RADIUS)",
               labels = number_format(suffix = " Million",
                                      scale = 1e-6)) +
  geom_label_repel(data = labelled_cities,
                   aes(x, y, label = label_city_shape(city, pop)),
                   nudge_y = 0.8,
                   nudge_x = c(0.9, rep(0, 3)),
                   size = 4,
                   segment.color = '#af8dc3',
                   show.legend = F) +
  guides(size = guide_legend(reverse=TRUE)) +
  theme_void(base_size = 10) +
  theme(legend.position = "bottom", legend.direction = "horizontal", 
        plot.title = element_text(size = 15,
                                  hjust = 0.5),
        legend.text=element_text(size=12))

ggsave("gg_bubble_radius.png",
       gg_bubble_radius,
       width = 9,
       height = 5)

gg_bubble_area <- oblong_horiz_city_positions %>%
  ggplot(aes(x, y, size = pop)) +
  coord_fixed(xlim = c(0, 10),
              ylim = c(2, 6),
              expand = FALSE) +
  geom_point(color = "#af8dc3")  +
  scale_size_area(max_size = 20,
                  name = "City population\n(vary by AREA)",
                  position = "bottom",
                  labels = number_format(suffix = " Million",
                                         scale = 1e-6)) +
  guides(size = guide_legend(reverse = TRUE)) +
  geom_label_repel(data = labelled_cities,
                   aes(x, y, label = label_city_shape(city, pop)),
                   nudge_y = 0.8,
                   nudge_x = 0,
                   size = 4,
                   segment.color = '#af8dc3',
                   show.legend = F) +
  theme_void(base_size = 10) +
  theme(legend.position = "bottom", legend.direction = "horizontal", 
        plot.title = element_text(size = 15,
                                  hjust = 0.5),
        legend.text=element_text(size=12))

ggsave("gg_bubble_area.png",
       gg_bubble_area,
       width = 9,
       height = 5)

