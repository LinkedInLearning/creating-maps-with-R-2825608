library(tidyverse)
library(sf)
library(rnaturalearthdata)
library(maps)
library(janitor)
library(readxl)
library(ggspatial)
library(leaflet)
library(ggrepel)
library(scales)
library(rmapshaper)

london_sf <- read_sf("data/london_boroughs")

education_data <- read_csv("data/age-when-completed-education.csv")

london_school_leavers_sf <- london_sf %>% 
  left_join(education_data,
            by = c("lad11nm" = "area")) %>% 
  filter(age_group == "16 or under")

gg_london_school_leavers <- london_school_leavers_sf %>% 
  ggplot() +
  geom_sf(aes(fill = value,
              shape = "City of\nLondon")) +
  scale_fill_viridis_c(na.value = "pink",
                       labels = scales::number_format(scale = 1, big.mark = ","),
                       name = "School leavers\n") +
  guides(shape = guide_legend(override.aes = list(fill = "pink"),
                              order = 2,
                              title = ""),
         fill = guide_colorbar(order = 1)) +
  labs(title = "Central London has fewer residents that left school\nat 16 (or under) than Outer London") +
  theme_void(base_size = 24) +
  theme(legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"))

ggsave("gg_london_school_leavers.png",
       gg_london_school_leavers,
       width = 10.9,
       height = 8)

gg_shapefiles <- ggplot() +
  geom_sf(data = london_sf) +
  theme_void()

ggsave("gg_shapefiles.png",
       gg_shapefiles,
       width = 10.9,
       height = 8)

