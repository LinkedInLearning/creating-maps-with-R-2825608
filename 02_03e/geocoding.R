library(tidygeocoder)
library(sf)
library(mapview)
library(readxl)
library(janitor)
library(tidyverse)

international_addresses <- read_excel("data/street-addresses.xlsx",
                           sheet = "International Addresses") %>%
  clean_names()

geo("58 West St #33A, NY")

geo("58 West St #33A, NY", method = "iq")


international_addresses_geocoded <- international_addresses %>% 
  geocode(street = street_address,
          city = city,
          postalcode = post_code,
          country = country,
          method = "iq")


international_addresses_geocoded %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) %>% 
  mapview()