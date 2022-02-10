library(janitor)
library(sf)
library(tidycensus)
library(tigris)
library(rmapshaper)
library(readxl)
library(tictoc)
library(future)
library(hrbrthemes)
library(tidyverse)

source("sf_dot_density.R")

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

tx_tracts_sex_by_age <- get_acs("tract",
                                   variables = list_acs_age_by_sex,
                                 state = "TX",
                                 year = 2019) %>% 
  clean_names()


all_tracts_sex_by_age %>% 
  mapview()

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

tx_counties_age_ranges_all <- tx_counties_age_ranges_all %>% 
  left_join(select(acs_age_groupings, age_range, age_grouping_c)) %>% 
  group_by(geoid, age_grouping_c) %>% 
  summarise(total_pop = sum(total_pop))


tx_tracts_age_ranges_all <- tx_tracts_sex_by_age %>% 
  left_join(vars_acs_age_by_sex,
            by = c("variable" = "variable_label")) %>% 
  group_by(geoid, age_range) %>% 
  summarise(total_pop = sum(estimate)) %>% 
  ungroup()

tx_tracts_age_ranges_all <- tx_tracts_age_ranges_all %>% 
  left_join(select(acs_age_groupings, age_range, age_grouping_c)) %>% 
  group_by(geoid, age_grouping_c) %>% 
  summarise(total_pop = sum(total_pop)) %>% 
  ungroup()


tx_counties_sf <- counties(state = "TX",
                           cb = TRUE) %>% 
  ms_simplify() %>% 
  clean_names()

tx_tracts_sf <- tracts(state = "TX",
                       cb = TRUE) %>% 
  ms_simplify() %>% 
  clean_names()

tx_tracts_sf





tx_counties_dot_density_age_grouping_c <- tx_counties_sf %>% 
  left_join(tx_counties_age_ranges_all) %>% 
  mutate(value = total_pop) %>% 
  arrange(value) %>% 
  mutate(value = ifelse(value < 200, NA, value)) %>% 
  sf_dot_density(group_var = age_grouping_c, parallel = TRUE, scale = 200)


tx_tracts_dot_density_age_grouping_c <- tx_tracts_sf %>% 
  left_join(tx_tracts_age_ranges_all) %>% 
  mutate(value = total_pop) %>% 
  arrange(value) %>% 
  select(age_grouping_c, value) %>% 
  sf_dot_density(group_var = age_grouping_c, parallel = TRUE, scale = 50)



fct_ordering_age_group_c <- acs_age_groupings %>% 
  arrange(ordering) %>% 
  distinct(age_grouping_c) %>% 
  pull(age_grouping_c)

tx_counties_dot_density_age_grouping_c <- tx_counties_dot_density_age_grouping_c %>% 
  mutate(age_grouping_c = fct_relevel(age_grouping_c, fct_ordering_age_group_c))

gg_tx_bad_dot_density <- ggplot() +
  geom_sf(data = tx_counties_dot_density_age_grouping_c,
          aes(color = age_grouping_c),
          size = 0.05) +
  geom_sf(data = tx_counties_sf,
          fill = "transparent") +
  scale_color_viridis_d() +
  guides(colour = guide_legend(override.aes = list(size=4),
                               title = "")) +
  labs(title = "Each dot represents 200 people") +
  theme_void()

gg_tx_bad_dot_density %>% 
  ggsave("gg_tx_bad_dot_density.png",
         .,
         width = 4.88 * 2,
         height = 4.88 *2)


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

tx_tracts_sf[unlist(st_crosses(tx_central_counties_sf, tx_tracts_sf)), ] %>% 
  mapview::mapview()



tx_county_names <- tx_counties_sf %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(name, countyfp) %>% 
  rename(county = name)

tx_tracts_dot_density_central_counties_200 <- tx_tracts_sf %>% 
  left_join(tx_county_names) %>% 
  filter(county %in% tx_central_counties) %>% 
  left_join(tx_tracts_age_ranges_all) %>% 
  mutate(value = total_pop) %>% 
  arrange(value) %>% 
  mutate(value = ifelse(value < 1, NA, value)) %>% 
  sf_dot_density(group_var = age_grouping_c, parallel = TRUE, scale = 100)

tic()
tx_dot_density_central_counties_200 <- tx_counties_sf %>% 
  filter(name %in% tx_central_counties) %>% 
  left_join(tx_counties_age_ranges_all) %>% 
  mutate(value = total_pop) %>% 
  arrange(value) %>% 
  mutate(value = ifelse(value < 1, NA, value)) %>% 
  sf_dot_density(group_var = age_grouping_c, parallel = TRUE, scale = 100)
toc("tx_dot_density_central_counties_200")

ggplot() +
  geom_sf(data = tx_dot_density_central_counties_200,
          aes(color = age_grouping_c),
          size = 0.05,
          alpha = 0.8) +
  geom_sf(data = tx_central_counties_sf,
          fill = "transparent") +
  scale_color_viridis_d() +
  guides(colour = guide_legend(override.aes = list(size=4),
                               title = "")) +
  labs(title = "Each dot represents 200 people") +
  theme_void()



ggplot() +
  geom_sf(data = tx_tracts_dot_density_central_counties_200,
          aes(color = age_grouping_c),
          size = 0.01,
          alpha = 0.8) +
  geom_sf(data = tx_central_counties_sf,
          fill = "transparent") +
  scale_color_viridis_d() +
  guides(colour = guide_legend(override.aes = list(size=4),
                               title = "")) +
  labs(title = "Each dot represents 100 people") +
  theme_void()


tx_central_counties_sf %>% 
  mapview::mapview()
