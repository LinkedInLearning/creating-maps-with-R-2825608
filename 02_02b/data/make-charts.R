library(sf)
library(readxl)
library(leaflet)
library(hrbrthemes)
library(tidyverse)

# ==== Import ====

world_sf <- read_sf("data/world-shapefiles")

country_population <- read_excel("data/country-gdp.xlsx")

theme_map_cjh <- function(base_size = 20, ...){
  
  theme_ipsum(base_size = base_size) +
    theme(panel.grid.major = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          ...)
  
}

# ==== Join ====





# ==== Standardise country names ====






# ==== Data visualisation Code ====

gg_empty_choropleth <- world_sf %>%
  ggplot() +
  geom_sf(color = "white",
          size = 0.1) +
  theme_map_cjh()

ggsave("data/gg_empty_choropleth.png",
       gg_empty_choropleth,
       width = 10,
       height = 6)


pal_gdp <- colorNumeric("viridis", world_population_sf$gdp_md_est)

world_population_sf %>% 
  leaflet() %>% 
  addPolygons(fillColor = ~pal_gdp(gdp_md_est),
              color = "white",
              fillOpacity = 1,
              weight = 1) %>% 
  addLegend(pal = pal_gdp,
            values = ~gdp_md_est,
            opacity = 1)
