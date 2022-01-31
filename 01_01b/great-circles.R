library(tidyverse)
library(sf)
library(rnaturalearthdata)
library(leaflet)
library(readxl)
library(mapview)
library(janitor)
library(rnaturalearthdata)
library(scales)

air_routes_passengers <- read_excel("data/air-routes.xlsx",
                                    sheet = "international-passenger-volume") %>%
  clean_names()

air_routes_passengers <- air_routes_passengers %>%
  mutate(across(contains("long"), ~as.numeric(str_trim(.x))))

journeys_to_sf <- function(journeys_data,
                           start_long = start.long,
                           start_lat = start.lat,
                           end_long = end.long,
                           end_lat = end.lat) {
  quo_start_long <- enquo(start_long)
  quo_start_lat <- enquo(start_lat)
  quo_end_long <- enquo(end_long)
  quo_end_lat <- enquo(end_lat)
  
  journeys_data %>%
    select(
      !! quo_start_long,
      !! quo_start_lat,
      !! quo_end_long,
      !! quo_end_lat
    ) %>%
    transpose() %>%
    map(~ matrix(flatten_dbl(.), nrow = 2, byrow = TRUE)) %>%
    map(st_linestring) %>%
    st_sfc(crs = 4326) %>%
    st_sf(geometry = .) %>%
    bind_cols(journeys_data) %>%
    select(everything(), geometry)
}

air_routes_seat_kms <- read_excel("data/air-routes.xlsx",
                                  sheet = "seat-kilometers") %>%
  clean_names()

air_routes_seat_kms <- air_routes_seat_kms %>%
  mutate(across(contains("long"), ~as.numeric(str_trim(.x))))

segmentize_air_routes <- function(.data, segment_length_km = 400){
  
  .data %>%
    filter(rank <= 5) %>%
    journeys_to_sf(airport_1_long,
                   airport_1_lat,
                   airport_2_long,
                   airport_2_lat) %>%
    st_segmentize(units::set_units(segment_length_km, km)) %>% 
    st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
  
}


sf_lines_air_routes_seat_kms <- air_routes_seat_kms %>%
  # filter(rank <= 5) %>%
  journeys_to_sf(airport_1_long,
                 airport_1_lat,
                 airport_2_long,
                 airport_2_lat) %>%
  st_segmentize(units::set_units(400, km)) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

sf_lines_air_routes_seat_kms <- air_routes_seat_kms %>%
  filter(rank <= 5) %>%
  journeys_to_sf(airport_1_long,
                 airport_1_lat,
                 airport_2_long,
                 airport_2_lat) %>%
  st_segmentize(units::set_units(400, km)) %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))


part_1 <- air_routes_seat_kms %>%
  filter(rank <= 5) %>%
  select(starts_with("airport_1")) %>%
  rename_with(~str_remove(.x, "airport_1_"))

part_2 <- air_routes_seat_kms %>%
  filter(rank <= 5) %>%
  select(starts_with("airport_2")) %>%
  rename_with(~str_remove(.x, "airport_2_"))

sf_points_air_routes_seat_kms <- part_1 %>%
  bind_rows(part_2) %>%
  unique()

gg_great_circles_curvy <- ggplot() +
  geom_sf(data = st_as_sf(countries110) %>%
            filter(!name == "Antarctica"),
          fill = "darkolivegreen3",
          color = "white",
          size = 0.5) +
  geom_sf(data = segmentize_air_routes(air_routes_seat_kms, 400),
          color = "red") +
  geom_sf(data = sf_points_air_routes_seat_kms %>%
            st_as_sf(coords = c("long", "lat")) %>%
            st_set_crs(4326),
          size = 3,
          color = "darkblue") +
  theme_void() +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  coord_sf(crs = "+init=epsg:3857")

ggsave("gg_great_circles_curvy.png",
       gg_great_circles_curvy)

gg_great_circles_straight <- ggplot() +
  geom_sf(data = st_as_sf(countries110) %>%
            filter(!name == "Antarctica"),
          fill = "darkolivegreen3",
          color = "white",
          size = 0.5) +
  geom_sf(data = segmentize_air_routes(air_routes_seat_kms, 10000),
          color = "red") +
  geom_sf(data = sf_points_air_routes_seat_kms %>%
            st_as_sf(coords = c("long", "lat")) %>%
            st_set_crs(4326),
          size = 3,
          color = "darkblue") +
  theme_void() +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  coord_sf(crs = "+init=epsg:3857")

ggsave("gg_great_circles_straight.png",
       gg_great_circles_straight)
