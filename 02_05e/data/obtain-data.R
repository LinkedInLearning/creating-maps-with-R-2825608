library(rvest)
library(tidyverse)
library(janitor)
library(writexl)

wiki_capitol_buildings <- read_html("https://en.wikipedia.org/wiki/List_of_state_and_territorial_capitols_in_the_United_States")

capitol_table_raw <- wiki_capitol_buildings %>% 
  html_table() %>% 
  .[[3]] %>% 
  clean_names()


capitol_table_clean <- capitol_table_raw %>% 
  select(capitol_name, address) %>% 
  mutate(state = str_extract(capitol_name, ".*(?=State|Legislative)")) %>% 
  relocate(state) %>% 
  separate(address,
           into = c("address_capitol",
                    "address_state_house"),
           sep = ",") %>% 
  select(-address_state_house) %>% 
  mutate(address_capitol = str_remove(address_capitol, "(capitol)"),
         address_capitol = str_remove_all(address_capitol, "[(]|[)]"),
         address_capitol = str_trim(address_capitol))

capitol_table_tidy <- capitol_table_clean %>% 
  mutate(state = str_trim(state)) %>% 
  separate(capitol_name,
           into = c("capitol_name",
                    "name_state_house"),
           sep = "\\n") %>% 
  select(capitol_name, address_capitol, state)

capitol_table_tidy %>% 
  write_xlsx("data/us-capitol-buildings.xlsx")
