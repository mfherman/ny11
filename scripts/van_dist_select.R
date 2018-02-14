library(tidyverse)
library(sf)
library(tmap)
library(here)
library(mapview)

current_van <- c("46035",
                 "46047",
                 "46049",
                 "46050",
                 "46051",
                 "46052",
                 "46053",
                 "46066",
                 "46067",
                 "46068",
                 "46069",
                 "46070",
                 "46071",
                 "47002",
                 "47005",
                 "47006",
                 "49047",
                 "49048",
                 "49049",
                 "49051",
                 "49052")

new_van <- c("63011",
             "63013",
             "63016",
             "63024",
             "63053",
             "63055",
             "63060",
             "63069",
             "63068",
             "63056",
             "63054",
             "67074",
             "61024",
             "61039",
             "61038",
             "63003",
             "63061",
             "63073",
             "63074",
             "64043",
             "64045",
             "64046",
             "64047",
             "61052",
             "64053",
             "64038")



cd11_2016 <- read_sf(here("/output/geo/cd11_demo_2016.geojson"))

van_sf <- 
  cd11_2016 %>%
  filter(elect_dist %in% current_van | elect_dist %in% new_van)

st_write(van_sf, here("output/geo/van_ed_demo.geojson"))

tmap_mode("view")
tm_shape(cd11_2016) +
  tm_polygons() +
  tm_shape(van_sf) +
  tm_fill(col = "orange")