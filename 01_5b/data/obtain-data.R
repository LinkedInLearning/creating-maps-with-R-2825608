library("tidyverse")
library("readxl")
library("janitor")
library("sf")
library(lubridate)

## ==== London shapefiles

download.file("https://data.london.gov.uk/download/2011-boundary-files/9e1dd6bc-4a41-45a6-8b8a-015cc8dd25a8/2011_london_boundaries.zip",
              "data/2011_london_boundaries.zip")

unzip("data/2011_london_boundaries.zip",
      exdir = "data/2011_london_boundaries")

bgc_layers <- st_layers("data/2011_london_boundaries/OA_2011_BGC_London") 

london_output_areas_sf <- map(bgc_layers$name, function(layer_id){read_sf("data/2011_london_boundaries/OA_2011_BGC_London/", layer = layer_id)}) %>%
  bind_rows()

london_boroughs_sf <- london_output_areas_sf %>%
  select(LAD11CD, LAD11NM) %>%
  st_buffer(dist = 0) %>%
  group_by(LAD11NM) %>%
  summarise(geometry = st_union(geometry)) %>%
  clean_names()

dir.create("data/london_boroughs")

london_boroughs_sf %>% 
  write_sf("data/london_boroughs/london_boroughs.shp")

unlink("data/2011_london_boundaries", recursive = TRUE)
unlink("data/2011_london_boundaries.zip")

## ==== NYC Traffic Collisions =====

# https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u/data?no_mobile=true

download.file("https://data.cityofnewyork.us/download/i8iw-xf4u/application%2Fzip",
              "data/nyc_zips.zip")

unzip("data/nyc_zips.zip",
      exdir = "data/ny-zipcodes")

options(timeout=1000)
download.file("https://data.cityofnewyork.us/api/views/h9gi-nx95/rows.csv?accessType=DOWNLOAD",
              "data/Motor_Vehicle_Collisions_-_Crashes.csv")

nyc_traffic_accidents_raw <- read_csv("data/Motor_Vehicle_Collisions_-_Crashes.csv")

nyc_traffic_accidents_2020 <- nyc_traffic_accidents_raw %>% 
  clean_names() %>% 
  mutate(crash_date = mdy(crash_date)) %>% 
  select(crash_date, zip_code, latitude, longitude, everything()) %>% 
  filter(!is.na(latitude)) %>% 
  filter(year(crash_date) == 2020)

nyc_traffic_accidents_2020 %>% 
  write_csv("data/nyc-traffic-accidents_2020.csv")

unlink("data/Motor_Vehicle_Collisions_-_Crashes.csv")
