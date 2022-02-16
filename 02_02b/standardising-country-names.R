library(sf)
library(readxl)
library(leaflet)
library(countrycode)
library(tidyverse)

# ==== Import ====

world_sf <- read_sf("data/world-shapefiles")

country_population <- read_excel("data/country-gdp.xlsx")

# ==== Join ====





# ==== Standardise country names ====






# ==== Data visualisation Code ====

pal_gdp <- colorNumeric("viridis", world_gdp$gdp_md_est)

world_gdp %>% 
  leaflet() %>% 
  addPolygons(fillColor = ~pal_gdp(gdp_md_est),
              color = "white",
              fillOpacity = 1,
              weight = 1) %>% 
  addLegend(pal = pal_gdp,
            values = ~gdp_md_est,
            opacity = 1)
