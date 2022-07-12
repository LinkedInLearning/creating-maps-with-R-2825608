library(tigris)
library(sf)
library(rmapshaper)
library(tidyverse)
library(janitor)
library(leaflet)
library(leaflet.extras)

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


pal_coastline_km <- colorNumeric("viridis", us_coastline$km_of_coastline,
                                 na.color = "pink")


popup_state_coastline <- function(state_name, coastline_km){
  
  paste(
    "<b>State:</b>", state_name,
    "<br>",
    "<b>Coastline length:</b>", scales::number(coastline_km, suffix = "km", accuracy = 1)
  )
  
}

lf_coastline <- leaflet() %>% 
  addPolygons(data = us_coastline,
              weight = 1,
              color = "white",
              fillColor = ~pal_coastline_km(km_of_coastline),
              fillOpacity = 1,
              popup = ~popup_state_coastline(name, km_of_coastline)) %>% 
  addLegend(data = us_coastline,
            pal = pal_coastline_km,
            values = ~km_of_coastline,
            opacity = 1,
            na.label = "Zero coastline") %>% 
  setMapWidgetStyle(style = list(background = "white"))

# legend placement fix ----------------------------------------------------

css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix) 

lf_coastline %>% 
  htmlwidgets::prependContent(html_fix)
