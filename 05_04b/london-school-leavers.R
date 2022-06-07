library(tidyverse)
library(sf)
library(leaflet)

london_sf <- read_sf("data/london_boroughs")

education_data <- read_csv("data/age-when-completed-education.csv")

london_school_leavers_sf <- london_sf %>% 
  left_join(education_data,
            by = c("lad11nm" = "area")) %>% 
  filter(age_group == "16 or under")






# legend fix --------------------------------------------------------------

html_fix <- htmltools::tags$style(type = "text/css", "div.info.legend.leaflet-control br {clear: both;}") 

lf_london_school_leavers %>% 
  htmlwidgets::prependContent(html_fix)