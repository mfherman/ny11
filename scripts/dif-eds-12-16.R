if(!exists("make_pct", mode = "function")) source("scripts/util.R")

ed2016 <- read_sf(here("output/geo/cd11_ed_2016.geojson")) %>%
  mutate(
    area_16 = st_area(.),
    test_16 = 1L
    ) %>%
  rename(elect_dist_16 = elect_dist)

ed2012 <- read_sf(here("output/geo/cd11_ed_2012.geojson")) %>%
  mutate(
    area_12 = st_area(.),
    test_12 = 1L
    )  %>%
  rename(elect_dist_12 = elect_dist)

pres_2016 <- read_csv(
  here("output/elect_results/2016_pres.csv"),
  col_types = cols(year = "i", total_votes = "i", .default = "c")) %>%
  mutate_at(vars(ends_with("_tot")), as.integer) %>%
  select(elect_dist, total_votes_16 = total_votes, clinton_tot, trump_tot)

pres_2012 <- read_csv(
  here("output/elect_results/2012_pres.csv"),
  col_types = cols(year = "i", total_votes = "i", .default = "c")) %>%
  mutate_at(vars(ends_with("_tot")), as.integer) %>%
  select(elect_dist, total_votes_12 = total_votes, obama_tot, romney_tot)

sf_2012 <- right_join(ed2012, pres_2012, by = c("elect_dist_12" = "elect_dist"))

pres_12_adj <-
  st_intersection(sf_2012, ed2016) %>%
  mutate(
    area_16_12 = st_area(.),
    area_16_prop = as.numeric(area_16_12 / area_12)
  ) %>%
  mutate_if(is.integer, funs(adj = (. * area_16_prop))) %>%
  group_by(elect_dist_16) %>%
  summarise_at(vars(contains("tot")), funs(sum(., na.rm = TRUE))) %>%
  mutate_if(is.numeric, as.integer) %>%
  st_set_geometry(NULL) %>%
  select(elect_dist = elect_dist_16, total_votes_12_adj:romney_tot_adj) %>%
  mutate(
    obama_prop_adj = obama_tot_adj / total_votes_12_adj,
    romney_prop_adj = romney_tot_adj / total_votes_12_adj
  )
  
write_csv(pres_12_adj, here("output/elect_results/pres_2012_adj.csv"))






area16 <- ed2016 %>%
  mutate(area1 = st_area(.)) %>%
  st_set_geometry(NULL)

area12 <- ed2012 %>% 
  mutate(area2 = st_area(.)) %>%
  st_set_geometry(NULL)

changed_eds <- full_join(area16, area12) %>%
  mutate(dif = area1 - area2) %>%
  filter(abs(as.double(dif)) > 1000) %>%
  pull(elect_dist)


dif2016 <- 
  ed2016 %>%
  filter(elect_dist %in% changed_eds) %>%
  select(elect_dist_16 = elect_dist) %>%
  mutate(area_16 = st_area(.),
         test_16 = 1L)
dif2012 <- 
  ed2012 %>%
  filter(elect_dist %in% changed_eds) %>%
  select(elect_dist_12 = elect_dist) %>%
  mutate(
    area_12 = st_area(.),
    test_12 = 2L)



map <- tm_shape(ed2016) +
  tm_polygons(border.col = "red", alpha = 0, id = "elect_dist", lwd = 2) +
  tm_text("elect_dist", size = 0.8, col = "red") +
  tm_shape(ed2012) +
  tm_polygons(border.col = "black", alpha = 0, id = "elect_dist", lwd = 2) +
  tm_text("elect_dist", size = 0.8, col = "black") +
  add_basemap

map %>% add_leaflet_opts(search = "ed2016")

write_csv(dif2016, here("output/dif1216.csv"))