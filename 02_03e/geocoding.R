library(tidygeocoder)
library(sf)
library(mapview)
library(readxl)
library(janitor)
library(tidyverse)

geo("58 W St #33A, New York", method = "iq")


international_addresses <- read_excel("data/street-addresses.xlsx",
                           sheet = "International Addresses") %>%
  clean_names()


addresses_geocoded <- international_addresses %>% 
  geocode(street = street_address,
          city = city,
          postalcode = post_code,
          country = country,
          method = "iq")

addresses_geocoded %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) %>% 
  mapview()
