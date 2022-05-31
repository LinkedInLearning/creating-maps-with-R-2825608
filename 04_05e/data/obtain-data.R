library(tidyverse)
library(sf)
library(janitor)
library(tigris)

# Data from https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/anag-cw7u
oregon_hospitals <- read_csv(here::here("data", "COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility_OR.csv"),
                             na = "-999999") %>%
  distinct(hospital_name, geocoded_hospital_address) %>%
  mutate(geocoded_hospital_address = na_if(geocoded_hospital_address, ""))

oregon_hospitals_auto_geocoded <- oregon_hospitals %>%
  drop_na(geocoded_hospital_address) %>%
  mutate(geocoded_hospital_address = str_remove(geocoded_hospital_address, "POINT ")) %>%
  mutate(geocoded_hospital_address = str_remove(geocoded_hospital_address, "\\(")) %>%
  mutate(geocoded_hospital_address = str_remove(geocoded_hospital_address, "\\)")) %>%
  separate(geocoded_hospital_address,
           into = c("long", "lat"),
           sep = " ") %>%
  mutate(long = parse_number(long)) %>%
  mutate(lat = parse_number(lat))

oregon_sf <- counties(state = "OR") %>% 
  clean_names()

oregon_sf %>% 
  write_sf("data/oregon-counties/oregon-counties.shp")

oregon_hospitals_sf <- oregon_hospitals_auto_geocoded %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs = st_crs(oregon_sf))

oregon_hospital_locations <- oregon_hospitals_sf %>% 
  mutate(county = oregon_sf$name[as.numeric(st_within(oregon_hospitals_sf, oregon_sf))]) %>% 
  st_drop_geometry() %>% 
  bind_cols(st_coordinates(st_transform(oregon_hospitals_sf, crs = 4326))) %>% 
  rename(long = X,
         lat = Y)

oregon_hospital_locations %>% 
  write_csv("data/oregon-hospital-locations.csv")
