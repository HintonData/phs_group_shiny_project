library(shinydashboard)
library(tidyverse)
library(here)
library(leaflet)
library(lubridate)
library(sf)
library(plotly)

health_board_map <- st_read(dsn = here("data/clean_data/"),
                            layer = "health_board_map_simple") %>% 
    st_transform("+proj=longlat +datum=WGS84")

as_demo_clean <- read_csv(here("data/clean_data/as_demo_clean.csv"))
hb_names <- read_csv(here("data/clean_data/hb_simple.csv"))
hb_names_basic <- read_csv(here("data/clean_data/hb_basic.csv"))
ae_activity <- read_csv(here("data/clean_data/ae_activity_clean.csv"))
covid_impacts <- read_csv(here("data/clean_data/covid_impacts_clean.csv"))
beds <- read_csv(here("data/clean_data/beds_clean.csv"))
demo_deprivation_clean <- read_csv(here("data/clean_data/demo_deprivation_clean.csv"))

hb_choices <- sort(append(unique(hb_names$hb_name), "All"))
hb_choices_basic <- sort(append(unique(hb_names_basic$hb_name), "All"))

wt_basic <- read_csv(here("data/clean_data/waiting_times_basic.csv"))