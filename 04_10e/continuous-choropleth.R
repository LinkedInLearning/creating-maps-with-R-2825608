library(tigris)
library(sf)
library(rmapshaper)
library(tidyverse)
library(janitor)
library(tidycensus)

us_states <- states() %>% 
  clean_names() %>% 
  mutate(statefp = as.numeric(statefp))

south_atlantic_states <- us_states %>% 
  filter(division == 5) %>% 
  ms_simplify()

prisoners_per_state <- get_decennial(geography = "state",
              variables = c("state_prison_population" = "PCT020006")) %>% 
  clean_names() %>% 
  mutate(state_prison_population = if_else(name == "District of Columbia",
                         NA_real_,
                         value))

south_atlantic_prisons <- south_atlantic_states %>% 
  left_join(prisoners_per_state)



ggplot() +
  geom_sf(data = south_atlantic_prisons,
          aes(fill = state_prison_population,
              shape = "Washington DC"),
          colour = "white",
          size = 0.2) +
  scale_fill_viridis_c(name = "State prison population",
                       labels = scales::number_format(big.mark = ","),
                       na.value = "pink") +
  guides(shape = guide_legend(override.aes = list(fill = "pink"),
                              title = NULL)) +
  theme_void()

