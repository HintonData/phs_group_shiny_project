#imports
library(tidyverse)
library(here)
library(janitor)
library(lubridate)

# Cleaning Script (read data -> clean -> save)

##health boards
hb_raw <- read_csv(here("data/raw_data/hb14_hb19.csv")) %>% clean_names()

hb_clean <- hb_raw %>% 
    select(hb, hb_name) %>% 
    mutate(hb_name = str_remove(hb_name, "NHS "))

shb_raw <- read_csv(here("data/raw_data/special_health_boards_and_national_facilities.csv")) %>% clean_names()

shb_clean <- shb_raw %>% 
    select(shb, shb_name) %>% 
    rename(hb = shb, hb_name = shb_name)


hb_joined <- rbind(hb_clean, shb_clean)

write_csv(hb_joined, here("data/clean_data/hb_simple.csv"))

##Demographics Age & Sex
as_demo_raw <- read_csv(here("data/raw_data/inpatient_and_daycase_by_nhs_board_of_treatment_age_and_sex.csv")) %>% clean_names()


as_demo_clean <- as_demo_raw %>% 
    left_join(hb_joined, "hb") %>%
    select(-ends_with("qf")) %>%
    mutate(sex = factor(sex, c("Male", "Female")),
           year = as.numeric(str_sub(quarter, 1, 4)),
           is_covid_year = case_when(
               year <= 2019 ~ "Pre_Covid", # before covid
               year >= 2020 ~ "Covid"), # 2020 Q1 + covid appears?
           is_covid_year = factor(is_covid_year, c("Pre_Covid", "Covid"))) %>% 
    filter(!is.na(is_covid_year))

write_csv(as_demo_clean, here("data/clean_data/as_demo_clean"))

##

##