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


# ==== Texas shapefiles
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

# ==== Age distribution

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
  sf_dot_density(group_var = age_grouping_c, parallel = TRUE, scale = 50)
toc("tx_dot_density_central_counties_200")


fct_ordering_age_group_c <- acs_age_groupings %>% 
  arrange(ordering) %>% 
  distinct(age_grouping_c) %>% 
  pull(age_grouping_c)

tx_dot_density_central_counties_200 <- tx_dot_density_central_counties_200 %>% 
  mutate(age_grouping_c = fct_relevel(age_grouping_c, fct_ordering_age_group_c))

tx_dot_density_central_counties_200_fiddle <- tx_dot_density_central_counties_200 %>% 
  left_join(select(acs_age_groupings, ordering, age_grouping_c))

tx_dot_density_central_counties_200_fiddle_sampled <- tx_dot_density_central_counties_200_fiddle %>% 
  sample_frac(1)



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
                               title = ""),
         color = guide_none()) +
  labs(title = "Age distribution of residents in Central Texas",
       subtitle = "1 dot represents 50 people") +
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

ggplot(quakes) +
  geom_point(aes(x = long, y = lat)) +
  theme_ipsum() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

gg_dot_density_central_texas %>% 
  ggsave("gg_dot_density_central_texas.png",
         .,
         width = 4.88 * 2,
         height = 3.5 *2)

source("data/make-hexbins.R")

tx_central_counties_sf




