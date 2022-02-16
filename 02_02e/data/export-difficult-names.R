library(countrycode)
library(rnaturalearthdata)
library(sf)
library(janitor)
library(writexl)
library(tidyverse)

world_sf_export <- countries110 %>% 
  st_as_sf() %>% 
  clean_names() %>% 
  select(name, iso_a3, gdp_md_est)

deliberately_difficult_country_names <- c("USA", "Canada", "United Kingdom", "Kingdom of the Netherlands", "Commonwealth of Australia", "Fed. Rep. of Brazil", "People's Republic of China", "Hellenic Republic", "S. Korea", "Russian Fed.", "Republic of South Africa")

difficult_country_names <- tibble(
  export_name = deliberately_difficult_country_names,
  iso_a3 = countrycode(deliberately_difficult_country_names, origin = "country.name", destination = "iso3c")
)

world_sf_export %>% 
  left_join(difficult_country_names) %>% 
  mutate(export_name = ifelse(is.na(export_name), name, export_name)) %>% 
  st_drop_geometry() %>% 
  select(export_name, gdp_md_est) %>% 
  rename(country_name = export_name) %>% 
  write_xlsx("data/country-gdp.xlsx")

# ==== Export sf ====

dir.create("data/world-shapefiles")

world_sf_export %>% 
  select(-gdp_md_est) %>% 
  rename(name = name) %>% 
  write_sf("data/world-shapefiles/world-shapefiels.shp")
