library(tidyverse)
library(readxl)
library(sf)

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

# ==== School age leaving ====

download.file("https://data.london.gov.uk/download/age-when-completed-continuous-full-time-education-borough/02bd7908-d260-4dba-982e-4eaec216e4e0/age-when-completed-continuous-full-time-education.xls",
              "data/age-when-completed-continuous-full-time-education.xls")

raw_london_age_finished_edu <- read_xls("data/age-when-completed-continuous-full-time-education.xls",
                                        sheet = "2018", skip = 1)

raw_london_age_finished_edu <- raw_london_age_finished_edu %>%
  select(Code:`Never had education...8`)

lafe_age_group <- tribble(
  ~messy_age_group, ~age_group,
  "16 or under...3", "16 or under",
  "17-19...4", "17-19",
  "20-23...5", "20-23",
  "24+...6", "24+",
  "Still in education...7", "Still in education",
  "Never had education...8", "Never had an education"
)

raw_london_age_finished_edu <- raw_london_age_finished_edu %>%
  pivot_longer(cols = 3:8) %>%
  left_join(lafe_age_group,
            by = c("name" = "messy_age_group")) %>%
  select(Code, Area, age_group, value) %>%
  janitor::clean_names()

london_age_finished_edu <- raw_london_age_finished_edu %>%
  filter(str_starts(code, "E09")) %>%
  mutate(value = as.numeric(value))

london_age_finished_edu %>% 
  write_csv("data/london_age_finished_edu.csv")

