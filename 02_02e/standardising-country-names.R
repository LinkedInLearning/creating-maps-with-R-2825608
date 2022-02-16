library(sf)
library(readxl)
library(leaflet)
library(countrycode)
library(tidyverse)

# ==== Import ====

world_sf <- read_sf("data/world-shapefiles")

country_population <- read_excel("data/country-gdp.xlsx")

# ==== Join ====

world_gdp <- world_sf %>% 
  left_join(country_population,
            by = c("name" = "country_name"))



# ==== Standardise country names ====

countrycode("Commonwealth of Australia", origin = "country.name", destination = "iso3c")

country_population_tidy <- country_population %>% 
  mutate(iso_a3 = countrycode(country_name, origin = "country.name", destination = "iso3c"))

world_gdp <- world_sf %>% 
  left_join(country_population_tidy)

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
