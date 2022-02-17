library(tidygeocoder)
library(sf)
library(mapview)
library(readxl)
library(janitor)
library(tidyverse)

international_addresses <- read_excel("data/street-addresses.xlsx",
                           sheet = "International Addresses") %>%
  clean_names()
