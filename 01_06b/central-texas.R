library(janitor)
library(sf)
library(tidycensus)
library(tigris)
library(rmapshaper)
library(readxl)
library(tictoc)
library(future)
library(ggspatial)
library(hrbrthemes)
library(tidyverse)


# ==== Texas shapefiles ====
tx_counties_sf <- counties(state = "TX",
                           cb = TRUE) %>% 
  ms_simplify() %>% 
  clean_names()


tx_county_names <- tx_counties_sf %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(name, countyfp) %>% 
  rename(county = name)

tx_outer_central_counties <- c("Bandera County", "Bexar County", 
                               "Bosque County", "Brazos County", 
                               "Burleson County", "Caldwell County", 
                               "Comal County", "Comanche County", 
                               "Fayette County", "Freestone County", 
                               "Gonzales County", "Guadalupe County", 
                               "Hamilton County", "Hill County", 
                               "Kerr County", "Kendall County", 
                               "Kimble County", "Leon County", 
                               "Limestone County", "Madison County", 
                               "Mason County", "Mills County", 
                               "Robertson County", "San Saba County", 
                               "Washington County", "Wilson County") %>% str_remove(., " County")


tx_inner_central_counties <- c("Bastrop County", "Bell County", 
                               "Blanco County", "Burnet County", 
                               "Coryell County", "Gillespie County", 
                               "Hays County", "Lampasas County", 
                               "Lee County", "Llano County", 
                               "Falls County", "McLennan County", 
                               "Milam County", "Travis County", 
                               "Williamson County") %>% str_remove(., " County")

tx_central_counties <- c(tx_inner_central_counties, tx_outer_central_counties)

tx_central_counties_sf <- tx_counties_sf %>% 
  filter(name %in% c(tx_central_counties))

# ==== Age distribution ====

acs_age_groupings <- read_excel("data/acs-age-groupings.xlsx")

vars_acs_2019 <- load_variables(2019, "acs5")

vars_acs_age_by_sex <- vars_acs_2019 %>% 
  filter(concept == "SEX BY AGE") %>% 
  mutate(label = str_remove(label, "Estimate!!Total:!!")) %>% 
  separate(col = label,
           into = c("gender", "age"),
           sep = ":!!") %>% 
  filter(str_detect(age, "[0-9]")) %>% 
  separate(col = age,
           into = c("age_low", "age_high"),
           sep = "[a-z]++") %>% 
  mutate(age_low = str_replace(age_low, "U", "0")) %>% 
  select(-concept) %>% 
  mutate(across(c(age_low, age_high), ~parse_number(.x))) %>% 
  mutate(age_range = str_glue("{age_low}_to_{ifelse(is.na(age_high), age_low, age_high)}"),
         variable_label = tolower(str_glue("{gender}_{age_range}")))

list_acs_age_by_sex <- vars_acs_age_by_sex %>% 
  select(variable_label, name) %>% 
  deframe()

all_counties_sex_by_age <- get_acs("county",
                                   variables = list_acs_age_by_sex,
                                   year = 2019) %>% 
  clean_names()

all_counties_sex_by_age_tidy <- all_counties_sex_by_age %>% 
  separate(name,
           into = c("county", "state"),
           sep = ", ")

tx_counties_age_ranges_all <- all_counties_sex_by_age_tidy %>% 
  filter(state == "Texas") %>% 
  left_join(vars_acs_age_by_sex,
            by = c("variable" = "variable_label")) %>% 
  group_by(geoid, age_range) %>% 
  summarise(total_pop = sum(estimate)) %>% 
  ungroup()

tx_counties_age_ranges_grouped <- tx_counties_age_ranges_all %>% 
  left_join(select(acs_age_groupings, age_range, age_grouping_c)) %>% 
  group_by(geoid, age_grouping_c) %>% 
  summarise(total_pop = sum(total_pop))



## ==== dot density map ====

source("sf_dot_density.R")

tic()
tx_dot_density_central_counties_200 <- tx_counties_sf %>% 
  filter(name %in% tx_central_counties) %>% 
  left_join(tx_counties_age_ranges_grouped) %>% 
  mutate(value = total_pop) %>% 
  arrange(value) %>% 
  mutate(value = ifelse(value < 1, NA, value)) %>% 
  sf_dot_density(group_var = age_grouping_c, parallel = TRUE, scale = 200)
toc("tx_dot_density_central_counties_200")


fct_ordering_age_group_c <- acs_age_groupings %>% 
  arrange(ordering) %>% 
  distinct(age_grouping_c) %>% 
  pull(age_grouping_c)

tx_dot_density_central_counties_200 <- tx_dot_density_central_counties_200 %>% 
  mutate(age_grouping_c = fct_relevel(age_grouping_c, rev(fct_ordering_age_group_c)))

tx_dot_density_central_counties_200_fiddle <- tx_dot_density_central_counties_200 %>% 
  left_join(select(acs_age_groupings, ordering, age_grouping_c))

tx_dot_density_central_counties_200_fiddle_sampled <- tx_dot_density_central_counties_200_fiddle %>% 
  sample_frac(1)

tx_dot_density_central_counties_200 %>% 
  pull(age_grouping_c)

ggplot() +
  geom_sf(data = tx_dot_density_central_counties_200[1:60000,],
          aes(fill = age_grouping_c))

gg_dot_density_central_texas <- ggplot() +
  geom_sf(data = tx_central_counties_sf,
          fill = "gray90",
          color = "transparent") +
  geom_sf(data = tx_dot_density_central_counties_200,
          aes(fill = age_grouping_c,
              color = age_grouping_c),
          size = 0.05,
          shape = 21,
          alpha = 0.8) +
  geom_sf(data = tx_central_counties_sf,
          fill = "transparent") +
  scale_color_viridis_d(option = "magma") +
  scale_fill_viridis_d(option = "magma") +
  guides(fill = guide_legend(override.aes = list(size = 4,
                                                   color = "black",
                                                   shape = 21),
                               title = "", reverse = TRUE),
         color = guide_none()) +
  labs(title = "Age distribution of residents in Central Texas",
       subtitle = "1 dot represents 200 people") +
  theme_void() +
  theme_ipsum(base_size = 20) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

gg_dot_density_central_texas

gg_dot_density_central_texas %>% 
  ggsave("gg_dot_density_central_texas.png",
         .,
         width = 4.88 * 2,
         height = 3.5 *2)

## ==== Choropleth ====

tx_counties_age_ranges_choropleth <- tx_counties_age_ranges_grouped %>% 
  group_by(geoid) %>% 
  mutate(perc = total_pop / sum(total_pop))



tx_choropleth_central_counties <- tx_counties_sf %>% 
  filter(name %in% tx_central_counties) %>% 
  left_join(tx_counties_age_ranges_choropleth)



fct_ordering_age_group_c <- acs_age_groupings %>% 
  arrange(ordering) %>% 
  distinct(age_grouping_c) %>% 
  pull(age_grouping_c)

tx_choropleth_central_counties <- tx_choropleth_central_counties %>% 
  mutate(age_grouping_c = fct_relevel(age_grouping_c, fct_ordering_age_group_c))

gg_central_texas_choropleth <- ggplot() +
  geom_sf(data = tx_choropleth_central_counties,
          aes(fill = perc)) +
  scale_fill_viridis_c(labels = scales::percent_format(),
                       name = NULL,
                       option = "magma") +
  facet_wrap(~age_grouping_c) +
  labs(title = "Age distribution of residents in Central Texas") +
  theme_void() +
  theme_ipsum(base_size = 20) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "top", legend.key.width = unit(3, "cm"))

gg_central_texas_choropleth %>% 
  ggsave("gg_central_texas_choropleth.png",
         .,
         width = 4.88 * 2,
         height = 3.5 *2)

## ==== Careful ====

kerr_tracts <- tracts(state = "TX",
       county = "265") %>% 
  clean_names()

kerr_age_data <- get_acs("tract",
        variables = list_acs_age_by_sex,
        state = "TX",
        year = 2019) %>% 
  clean_names()%>% 
  filter(geoid %in% kerr_tracts$geoid)

kerr_age_data %>% 
  left_join()

kerr_age_data_grouped <- kerr_age_data %>% 
  left_join(vars_acs_age_by_sex,
            by = c("variable" = "variable_label")) %>% 
  group_by(geoid, age_range) %>% 
  summarise(total_pop = sum(estimate)) %>% 
  ungroup()%>% 
  left_join(select(acs_age_groupings, age_range, age_grouping_c)) %>% 
  group_by(geoid, age_grouping_c) %>% 
  summarise(total_pop = sum(total_pop)) %>% 
  ungroup()

kerr_dot_dot_density_tracts <- kerr_tracts %>% 
  left_join(kerr_age_data_grouped) %>% 
  mutate(value = total_pop) %>% 
  sf_dot_density(group_var = age_grouping_c, parallel = TRUE, scale = 200)


kerr_dot_dot_density_tracts %>% 
  mapview::mapview()

filter(tx_central_counties_sf, name == "Kerr")

tx_dot_density_central_counties_200[lengths(st_within(tx_dot_density_central_counties_200, filter(tx_central_counties_sf, name == "Kerr"))), ]

tx_dot_density_central_counties_200[st_within(tx_dot_density_central_counties_200, filter(tx_central_counties_sf, name == "Kerr")), ]



gg_kerr_dot_density <- ggplot() +
  annotation_map_tile(zoom = 10) +
  geom_sf(data = kerr_dot_dot_density_tracts) +
  theme_void()

gg_kerr_dot_density %>% 
  ggsave("gg_kerr_dot_density.png",
         .)

## ==== Hexbins ====

source("data/make-hexbins.R")


as.integer(
  min(
    diff(st_bbox(tx_counties_sf)[c(1, 3)]),
    diff(st_bbox(tx_counties_sf)[c(2, 4)])
  ) / 40
)

tx_central_counties_hex_sf <- tx_central_counties_sf %>% 
  st_union() %>% 
  st_as_sf() %>% 
  make_hex_bins("honeycomb",
                0.1) %>% 
  st_cast("POLYGON") 

tx_central_counties_sf


nyc_zips_with_accidents_hex_sf <- tx_central_counties_hex_sf %>% 
  mutate(accidents_in_zip = lengths(st_covers(nyc_zips_hex_sf, nyc_traffic_accidents_2020_sf)))




