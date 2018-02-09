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
cd11_2017 <- read_sf(here("data/geo/nyed_17c")) %>%
  clean_names() %>%
  mutate(elect_dist = as.character(elect_dist)) %>%
  filter(elect_dist %in% cd11_eds_2017) %>%
  select(elect_dist) %>%
  st_transform(4326)

st_write(cd11_2017, here("output/geo/cd11_ed_2017.geojson"))


## 2016
# define vector of eds in cd11
cd11_eds_2016 <- read_csv(
  here("output/elect_results/2016_cd11.csv"),
  col_types = cols(.default = "c")
) %>%
  pull(elect_dist)

# select only cd11 eds and write to geojson
cd11_2016 <- read_sf(here("data/geo/nyed_16c")) %>%
  clean_names() %>%
  mutate(elect_dist = as.character(elect_dist)) %>%
  filter(elect_dist %in% cd11_eds_2016) %>%
  select(elect_dist) %>%
  st_transform(4326)

st_write(cd11_2016, here("output/geo/cd11_ed_2016.geojson"))

## 2014
# define vector of eds in cd11
cd11_eds_2014 <- read_csv(
  here("output/elect_results/2014_cd11.csv"),
  col_types = cols(.default = "c")
  ) %>%
  pull(elect_dist)

# select only cd11 eds and write to geojson
cd11_2014 <- read_sf(here("data/geo/nyed_14c")) %>%
  clean_names() %>%
  mutate(elect_dist = as.character(elect_dist)) %>%
  filter(elect_dist %in% cd11_eds_2014) %>%
  select(elect_dist) %>%
  st_transform(4326)

st_write(cd11_2014, here("output/geo/cd11_ed_2014.geojson"))

library(tmap)

tmap_mode("view")
tm_shape(cd11_2014) +
  tm_borders(col = "red") +
  tm_text("elect_dist", col = "red") +
  tm_shape(cd11_2016) +
  tm_borders(col = "green") +
  tm_text("elect_dist", col = "green")
  

hi <- cd11_2014 %>%
  mutate(area_2014 = as.double(st_area(.))) %>%
  st_set_geometry(NULL)
  
hi2 <- cd11_2016 %>%
  mutate(area_2016 = as.double(st_area(.))) %>%
  st_set_geometry(NULL)

joined <- full_join(hi2, hi) %>%
  mutate(
    abs_dif = abs(area_2016 - area_2014),
    same = if_else(abs_dif < 1000, TRUE, FALSE)
    )

sum(joined$same, na.rm = TRUE)


?tm_text

ny2010 <- load("/Users/matthewherman/Downloads/NY_2010.RData")

