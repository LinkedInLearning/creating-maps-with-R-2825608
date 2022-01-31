library(sf)
library(tigris)
library(tidycensus)
library(rmapshaper)
library(janitor)
library(tidyverse)

us_contiguous_pop <- get_acs(geography = "state",
        variables = "B01003_001",
        geometry = TRUE) %>% 
  clean_names() %>% 
  mutate(geoid = as.numeric(geoid)) %>% 
  filter(geoid < 60,
         !geoid %in% c(2, 15)) %>% 
  ms_simplify()

gg_us_contiguous_population <- us_contiguous_pop %>% 
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c(labels = scales::number_format(scale = 1E-6,
                                                      suffix = " Million"),
                       name = "Population") +
  theme_void() +
  theme(legend.position = "top",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.key.width = unit(2.5, "cm"))

ggsave("gg_us_contiguous_population.png",
       gg_us_contiguous_population,
       width = 8,
       height = 4)
