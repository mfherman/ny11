## here's a script that takes ed shapefiles for all of nyc and
## selects only eds within NY 11, then write to geojson

library(tidyverse)
library(here)
library(janitor)
library(sf)
library(lwgeom)
library(tmap)

## 2017
# define vector of eds in cd11
cd11_eds_2017 <- read_csv(
  here("data/cd11_eds_2017.csv"),
  col_types = cols(.default = "c")
  ) %>%
  pull(elect_dist)

# select only cd11 eds
cd11_2017 <- read_sf(here("data/geo/nyed_17c")) %>%
  clean_names() %>%
  mutate(elect_dist = as.character(elect_dist)) %>%
  filter(elect_dist %in% cd11_eds_2017) %>%
  select(elect_dist) %>%
  st_transform(4326)

# get combined eds
cd11_2017_combined <- read_csv(
  file = here("data/2017_mayor_results.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(
    ed_split = str_sub(edad_status, start = -6),
    ed_new = paste0(str_sub(ed_split, start = -2), str_sub(ed_split, end = 3)),
    elect_dist = paste0(ad, ed)
  ) %>%
  select(elect_dist, ed_new) %>%
  filter(ed_new != "AYN-P") %>%
  distinct(elect_dist, ed_new) %>%
  filter(elect_dist %in% cd11_eds_2017)

# write new combined shp file, filter out in valid geometries
cd11_2017 %>%
  left_join(cd11_2017_combined) %>%
  mutate(elect_dist = if_else(is.na(ed_new), elect_dist, ed_new)) %>%
  filter(st_is_valid(.) == TRUE) %>%
  group_by(elect_dist) %>%
  summarise() %>%
  st_cast() %>%
  write_sf(here("output/geo/cd11_ed_2017.geojson"))

## 2016
# define vector of eds in cd11
cd11_eds_2016 <- read_csv(
  here("output/elect_results/2016_cd11.csv"),
  col_types = cols(.default = "c")
) %>%
  pull(elect_dist)

# select only cd11 eds
cd11_2016 <- read_sf(here("data/geo/nyed_16c")) %>%
  clean_names() %>%
  mutate(elect_dist = as.character(elect_dist)) %>%
  filter(elect_dist %in% cd11_eds_2016) %>%
  select(elect_dist) %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  st_cast()

# get combined eds
cd11_2016_combined <- read_csv(
  file = here("data/2016_cd11_results.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(
    ed_split = str_sub(edad_status, start = -6),
    ed_new = paste0(str_sub(ed_split, start = -2), str_sub(ed_split, end = 3)),
    elect_dist = paste0(ad, ed)
  ) %>%
  select(elect_dist, ed_new) %>%
  filter(ed_new != "AYN-P") %>%
  distinct(elect_dist, ed_new)

# write new combined shp file, filter out in valid geometries
cd11_2016 %>%
  left_join(cd11_2016_combined) %>%
  mutate(elect_dist = if_else(is.na(ed_new), elect_dist, ed_new)) %>%
  filter(st_is_valid(.) == TRUE) %>%
  group_by(elect_dist) %>%
  summarise() %>%
  st_cast() %>%
  write_sf(here("output/geo/cd11_ed_2016.geojson"))


## 2014
# define vector of eds in cd11
cd11_eds_2014 <- read_csv(
  here("output/elect_results/2014_cd11.csv"),
  col_types = cols(.default = "c")
  ) %>%
  pull(elect_dist)

# select only cd11 eds
cd11_2014 <- read_sf(here("data/geo/nyed_14c")) %>%
  clean_names() %>%
  mutate(elect_dist = as.character(elect_dist)) %>%
  filter(elect_dist %in% cd11_eds_2014) %>%
  select(elect_dist) %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  st_cast()

# NO combined eds in cd11
cd11_2014_combined <- read_csv(
  file = here("data/2014_cd11_results.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(
    ed_split = str_sub(edad_status, start = -6),
    ed_new = paste0(str_sub(ed_split, start = -2), str_sub(ed_split, end = 3)),
    elect_dist = paste0(ad, ed)
  ) %>%
  select(elect_dist, ed_new) %>%
  filter(ed_new != "AYN-P") %>%
  distinct(elect_dist, ed_new)

# write new combined shp file, filter out in valid geometries
cd11_2014 %>%
  left_join(cd11_2014_combined) %>%
  mutate(elect_dist = if_else(is.na(ed_new), elect_dist, ed_new)) %>%
  filter(st_is_valid(.) == TRUE) %>%
  group_by(elect_dist) %>%
  summarise() %>%
  st_cast() %>%
  write_sf(here("output/geo/cd11_ed_2014.geojson"))



## 2012
# define vector of eds in cd11
cd11_2012_eds <-
  read_csv(here("output/elect_results/2012_cong.csv")) %>% 
  pull(elect_dist)

water_eds <- c("62074", "63076", "63074", "61062", "64057", "64056", "46047", "64094")

cd11_2012 <- read_sf(here("data/geo/nyed_12b")) %>%
  clean_names() %>%
  mutate(elect_dist = as.character(elect_dist)) %>%
  filter(elect_dist %in% cd11_2012_eds & !(elect_dist %in% water_eds)) %>%
  select(elect_dist) %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  st_cast()

# write new combined shp file
cd11_2012 %>% write_sf(here("output/geo/cd11_ed_2012.geojson"))