library(tidyverse)
library(sf)
library(leaflet)

london_sf <- read_sf("data/london_boroughs")

education_data <- read_csv("data/age-when-completed-education.csv")

london_school_leavers_sf <- london_sf %>% 
  left_join(education_data,
            by = c("lad11nm" = "area")) %>% 
  filter(age_group == "16 or under") %>% 
  st_transform(crs = 4326)


pal_school_leavers <- colorBin("viridis", london_school_leavers_sf$value, 
                               na.color = "pink")


lf_london_school_leavers <- leaflet() %>% 
  addPolygons(data = london_school_leavers_sf,
              weight = 1,
              color = "black",
              fillColor = ~pal_school_leavers(value),
              fillOpacity = 1) %>% 
  addLegend(data = london_school_leavers_sf,
            pal = pal_school_leavers,
            values = ~value,
            opacity = 1,
            title = "School leavers 16 or under",
            na.label = "City of London")



# legend fix --------------------------------------------------------------

html_fix <- htmltools::tags$style(type = "text/css", "div.info.legend.leaflet-control br {clear: both;}") 

lf_london_school_leavers %>% 
  htmlwidgets::prependContent(html_fix)