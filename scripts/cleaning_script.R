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

write_csv(hb_clean, here("data/clean_data/hb_basic.csv"))

shb_raw <- read_csv(here("data/raw_data/special_health_boards_and_national_facilities.csv")) %>% clean_names()

shb_clean <- shb_raw %>% 
    select(shb, shb_name) %>% 
    rename(hb = shb, hb_name = shb_name)


hb_joined <- rbind(hb_clean, shb_clean)

write_csv(hb_joined, here("data/clean_data/hb_simple.csv"))

##hospitals

hsp_raw <- read_csv(here("data/raw_data/hospital_data.csv")) %>% 
    clean_names()

hsp_clean <- hsp_raw %>% 
    select(location, location_name, postcode, address_line, hb, hscp)

write_csv(hsp_clean, here("data/clean_data/hsp.csv"))

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

write_csv(as_demo_clean, here("data/clean_data/as_demo_clean.csv"))

##A&E and Beds
ae_activity_raw <- read_csv(here("data/raw_data/ae_activity.csv")) %>% 
    clean_names()

beds_raw <- read_csv(here("data/raw_data/beds_by_nhs_board_of_treatment_and_specialty.csv")) %>% 
    clean_names()

ae_activity_clean <- ae_activity_raw %>% 
    select(-country) %>% 
    separate(col = month, into = c("year", "month"), sep = 4) %>% 
    left_join(hb_joined, by = c("hbt" = "hb"))  %>% 
    left_join(hsp_clean %>% 
                  select(location, location_name), by = c("treatment_location" = "location")) %>% 
    relocate(hb_name, .after = hbt) %>% 
    relocate(location_name, .after = treatment_location) %>%
    mutate(is_covid_year = case_when(year <= 2019 ~ FALSE,
                                     TRUE ~ TRUE)) %>% 
    filter(between(as.numeric(year), 2017, 2022))

write_csv(ae_activity_clean, here("data/clean_data/ae_activity_clean.csv"))

beds_clean <- beds_raw %>% 
    left_join(hb_joined, by = "hb") %>% 
    left_join(hsp_clean %>% 
                  select(location, location_name), by = "location") %>% 
    filter(hb != location) %>% 
    mutate(yq = as.factor(quarter)) %>% 
    separate(col = quarter, into = c("year", "quarter"), sep = 4) %>% 
    mutate(quarter = str_remove(quarter, "Q")) %>% 
    relocate(hb_name, .after = hb) %>% 
    relocate(location_name, .after = location) %>% 
    filter(between(as.numeric(year), 2017, 2022))

write_csv(beds_clean, here("data/clean_data/beds_clean.csv"))

## COVID Impacts

covid_impacts <- read_csv(here("data/raw_data/covid_wider_impacts.csv"))

covid_impacts <- covid_impacts %>% 
    clean_names() %>% 
    left_join(hb_clean)

write_csv(covid_impacts, here("data/clean_data/covid_impacts_clean.csv"))

## Deprivation

demo_deprivation_raw <- read_csv(here::here("data/raw_data/inpatient_and_daycase_by_nhs_board_of_treatment_and_simd.csv")) %>% 
    clean_names()

demo_deprivation_clean <- demo_deprivation_raw %>% 
    left_join(hb_joined, "hb") %>%
    select(-ends_with("qf")) %>% 
    mutate(simd = factor(simd, 1:5),
           year = as.numeric(str_sub(quarter, 1, 4)),
           is_covid_year = case_when(
               year <= 2019 ~ "Pre_Covid", # before covid
               year >= 2020 ~ "Covid"), # 2020 Q1 + covid appears?
           is_covid_year = factor(is_covid_year, c("Pre_Covid", "Covid")))

write_csv(demo_deprivation_clean, here("data/clean_data/demo_deprivation_clean.csv"))

