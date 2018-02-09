library(tidyverse)
library(here)
library(janitor)
library(sf)

## 2017
# define vector of eds in cd11
cd11_eds_2017 <- read_csv(
  here("data/cd11_eds_2017.csv"),
  col_types = cols(.default = "c")
  ) %>%
  pull(elect_dist)

# select only cd11 eds and write to geojson
read_sf(here("data/geo/nyed_17c")) %>%
  clean_names() %>%
  mutate(elect_dist = as.character(elect_dist)) %>%
  filter(elect_dist %in% cd11_eds_2017) %>%
  select(elect_dist) %>%
  st_transform(4326) %>%
  st_write(here("output/geo/cd11_ed_2017.geojson"))


## 2014
# define vector of eds in cd11
cd11_eds_2014 <- read_csv(
  here("output/elect_results/2014_cd11.csv"),
  col_types = cols(.default = "c")
  ) %>%
  pull(elect_dist)

# select only cd11 eds and write to geojson
read_sf(here("data/geo/nyed_14c")) %>%
  clean_names() %>%
  mutate(elect_dist = as.character(elect_dist)) %>%
  filter(elect_dist %in% cd11_eds_2014) %>%
  select(elect_dist) %>%
  st_transform(4326) %>%
  st_write(here("output/geo/cd11_ed_2014.geojson"))

hi <- read_sf(here("output/geo/cd11_ed_2014.geojson"))
