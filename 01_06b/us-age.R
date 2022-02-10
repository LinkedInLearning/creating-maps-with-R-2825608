library(janitor)
library(sf)
library(tidycensus)
library(tigris)
library(rmapshaper)
library(readxl)
library(tictoc)
library(future)
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

list_acs_age_by_sex %>% class()

all_counties_sex_by_age <- get_acs("county",
        variables = list_acs_age_by_sex,
        year = 2019) %>% 
  clean_names()

all_counties_sex_by_age_tidy <- all_counties_sex_by_age %>% 
  separate(name,
           into = c("county", "state"),
           sep = ", ")



tx_age_ranges_all <- all_counties_sex_by_age_tidy %>% 
  filter(state == "Texas") %>% 
  left_join(vars_acs_age_by_sex,
            by = c("variable" = "variable_label")) %>% 
  group_by(geoid, age_range) %>% 
  summarise(total_pop = sum(estimate)) %>% 
  ungroup()


tx_grouped_ages_grouping_c <- tx_age_ranges_all %>% 
  left_join(select(acs_age_groupings, age_range, age_grouping_c)) %>% 
  group_by(geoid, age_grouping_c) %>% 
  summarise(total_pop = sum(total_pop))

tx_counties_sf <- counties(state = "TX",
                           cb = TRUE) %>% 
  ms_simplify() %>% 
  clean_names()

tx_counties_sf %>% 
  left_join(tx_grouped_ages_grouping_c) %>% 
  mutate(value = total_pop) %>% 
  arrange(value) %>% 
  mutate(value = ifelse(value < 200, NA, value))

tx_dot_density_age_grouping_c <- tx_counties_sf %>% 
  left_join(tx_grouped_ages_grouping_c) %>% 
  mutate(value = total_pop) %>% 
  arrange(value) %>% 
  mutate(value = ifelse(value < 200, NA, value)) %>% 
  sf_dot_density(group_var = age_grouping_c, parallel = TRUE, scale = 200)
toc("tx_dot_density_age_grouping_c")

tx_dot_density_age_grouping_c %>% 
  ggplot() +
  geom_sf(aes(color = age_grouping_c),
          size = 0.1)









