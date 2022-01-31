library(sf)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(rmapshaper)
library(janitor)
library(maps)
library(ggspatial)
library(mapview)
library(leaflet)
library(tidyverse)

montenegro_sf <- countries10 %>% 
  st_as_sf() %>% 
  clean_names() %>% 
  filter(name == "Montenegro") 

montenegro_50_sf <- countries50 %>% 
  st_as_sf() %>% 
  clean_names() %>% 
  filter(name == "Montenegro") 

countries_10_sf <- countries10 %>% 
  st_as_sf() %>% 
  clean_names()

montenegro_border_countries <- countries_10_sf[st_touches(montenegro_sf, countries_10_sf, sparse = FALSE), ]


montenegro_cities <- world.cities %>% 
  filter(country.etc == "Montenegro") %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326)

countries_50_sf <- countries50 %>% 
  st_as_sf()


gg_montenegro_no_context <- ggplot() +
  geom_sf(data = montenegro_sf,
          fill = "darkolivegreen3") +
  geom_sf(data = montenegro_cities,
          aes(size = pop)) + 
  scale_size_area(labels = scales::number_format(big.mark = ","),
                  name = "Population",
                  max_size = 10) +
  theme_void() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))

ggsave("gg_montenegro_no_context.png",
       gg_montenegro_no_context,
       width = 8,
       height = 9)

gg_montenegro_with_title <- gg_montenegro_no_context +
  labs(title = "Montenegro") +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 20))

ggsave("gg_montenegro_with_title.png",
       gg_montenegro_with_title,
       width = 8,
       height = 9)

gg_montenegro_border_countries <- ggplot() +
  geom_sf(data = montenegro_border_countries,
          fill = "antiquewhite",
          color = "grey50") +
  geom_sf(data = montenegro_sf,
          fill = "darkolivegreen3") +
  theme_void() +
  theme(panel.border = element_rect(fill = "transparent"))

ggsave("gg_montenegro_border_countries.png",
       gg_montenegro_border_countries,
       width = 8.42,
       height = 8.42)


gg_montenegro_continent_context <- ggplot() +
  geom_sf(data = countries_50_sf,
          fill = "antiquewhite",
          color = "grey50") +
  geom_sf(data = montenegro_50_sf,
          fill = "darkolivegreen3") +
  coord_sf(xlim = c(-10 , 30),
           ylim = c(35, 60)) +
  theme_void() +
  theme(panel.border = element_rect(fill = "transparent"))

ggsave("gg_montenegro_continent_context.png",
       gg_montenegro_continent_context,
       width = 8,
       height = 7)

label_cities <- function(city_name, population){
  
  fmt_pop <- scales::number(population, big.mark = ",")
  
  str_glue("{city_name}", "<br>", "Population: {fmt_pop}")
  
}

leaflet() %>% 
  addProviderTiles(providers$Stamen.Terrain) %>% 
  addPolygons(data = montenegro_50_sf,
              fillColor = "transparent",
              color = "black",
              weight = 1) %>% 
  addCircleMarkers(data = montenegro_cities,
                   radius = ~sqrt(pop) * 1E-1,
                   fillColor = "purple",
                   fillOpacity = 0.8,
                   weight = 1,
                   color = "black",
                   popup = ~label_cities(name, pop))
