library(tigris)
library(sf)
library(rmapshaper)
library(tidyverse)
library(janitor)
library(leaflet)

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


leaflet() %>% 
  addPolygons(data = us_coastline)

# legend placement fix ----------------------------------------------------

css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix) 

lf_coastline %>% 
  htmlwidgets::prependContent(html_fix)
