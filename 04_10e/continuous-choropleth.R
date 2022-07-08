library(tigris)
library(sf)
library(rmapshaper)
library(tidyverse)
library(janitor)

us_states <- states() %>% 
  clean_names() %>% 
  mutate(statefp = as.numeric(statefp)) %>% 
  filter(statefp < 60,
         !statefp %in% c(2, 15)) %>% 
  ms_simplify()


state_coastline <- read_csv("data/km-of-coastline.csv")

us_coastline <- us_states %>% 
  left_join(state_coastline,
            by = c("name" = "state"))


ggplot() +
  geom_sf(data = us_coastline,
          aes(fill = km_of_coastline,
              shape = "Zero km\nof coastline")) +
  scale_fill_viridis_c(labels = scales::number_format(big.mark = ","),
                       na.value = "pink") +
  guides(shape = guide_legend(override.aes = list(fill = "pink"),
                              title = NULL,
                              order = 2),
         fill = guide_colorbar(order = 1)) +
  theme_void()

