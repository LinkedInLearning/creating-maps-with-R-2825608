library(tigris)
library(sf)
library(rmapshaper)
library(tidyverse)
library(janitor)
library(tidycensus)
library(leaflet)
library(leaflet.extras)

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


pal_prison_pop <- colorNumeric("viridis", south_atlantic_prisons$state_prison_population)

popup_state_prisons <- function(state, prison_population){
  
  paste(
    "<b>State:</b>", state,
    "<br>",
    "<b>State prison population:</b>", scales::number(prison_population, big.mark = ",")
  )
  
}

lf_map <- leaflet() %>% 
  addPolygons(data = south_atlantic_prisons,
              color = "black",
              weight = 1,
              fillColor = ~pal_prison_pop(state_prison_population),
              fillOpacity = 1) %>% 
  addLegend(data = south_atlantic_prisons,
            pal = pal_prison_pop,
            values = ~state_prison_population,
            opacity = 1,
            na.label = "Washington DC",
            title = "State prison population") %>% 
  setMapWidgetStyle(style = list(background = "white"))




# legend fix --------------------------------------------------------------

html_fix <- htmltools::tags$style(type = "text/css", "div.info.legend.leaflet-control br {clear: both;}") 

lf_map %>% 
  htmlwidgets::prependContent(html_fix)

