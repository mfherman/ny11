library(tidyverse)
library(here)
library(janitor)
library(sf)
library(tmap)

# # only do this once to get a cd11 shapefile -- its saved in data/geo/cd11_ed.geojson now
# # download nyc ed shapefile
# nyc_ed <- "http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nyed/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson"
# ed <- read_sf(nyc_ed) %>%
#   clean_names() %>%
#   mutate(elect_dist = as.character(elect_dist))
# 
# # read in cd11 2016
# cd11_2016_results <- read_csv(
#   file = here("data/2016_cd11_results.csv"),
#   col_types = cols(.default = "c")
# ) %>%
#   clean_names() %>%
#   mutate(
#     elect_dist = paste0(ad, ed),
#     tally = as.integer(tally),
#     candidate = case_when(
#       str_detect(unit_name, "Donovan") ~ "Donovan",
#       str_detect(unit_name, "Reichard") ~ "Reichard",
#       str_detect(unit_name, "Bardel") ~ "Bardel"
#     )
#   ) %>%
#   filter(!is.na(candidate)) %>%
#   select(elect_dist, candidate, tally)
# 
# # define vector of cd11 election districts
# cd11_eds <- cd11_2016_results %>% distinct(elect_dist) %>% pull()
# 
# # filter nyc_ed file to cd11 only
# cd11_ed_sf <- ed %>% 
#   filter(elect_dist %in% cd11_eds) %>%
#   select(elect_dist, shape_area, shape_length)
# 
# # write to geojson
# write_sf(cd11_ed_sf, here("data/geo/cd11_ed.geojson"))

# read in cd 11 ed shape file
cd11_ed_sf <- read_sf(here("data/geo/cd11_ed.geojson"))

# read in pres results 2016
pres_2016 <- read_csv(
  here("output/elect_results/2016_pres.csv"),
  col_types = cols(year = "i", total_votes = "i", .default = "c")) %>%
  mutate_at(vars(ends_with("_prop")), as.double) %>%
  mutate_at(vars(ends_with("_tot")), as.integer)

# create dem/rep margins, join with ed geo file
pres_2016_margin <- pres_2016 %>%
  mutate(margin = clinton_prop - trump_prop) %>%
  select(elect_dist, total_votes, margin, clinton_tot,
         trump_tot, clinton_prop, trump_prop)

pres_2016_to_map <- left_join(cd11_ed_sf, pres_2016_margin) %>%
  mutate(elect_dist = paste("ED", elect_dist))

pres_2016_close_ed <- pres_2016_to_map %>%
  filter(between(margin, -.1, .1))

# define a little helper function to format percentages
make_pct <- function(x, digits = 1) {
  paste0(formatC(x * 100, digits = digits, format = "f"), "%")
}

# define a little helper function to format margins
make_margin <- function(x, digits = 1) {
  if_else(x > 0,
          paste0("+",formatC(x * 100, digits = digits, format = "f")),
          formatC(x * 100, digits = digits, format = "f")
          )
}

make_margin(c(-.111111, .333233))

# make a map of clinton/trump margin 2016, highlight ed +-10pt
tmap_mode("view")
tm_shape(pres_2016_to_map, name = "2016 Presidential") +
  tm_fill(
    col = "margin",
    palette = "RdBu",
    style = "cont",
    breaks = seq(-1, 1, by = 0.2),
    title = "D/R Margin",
    textNA = "No Votes",
    popup.vars = c(
      "Dem/Rep Margin" = "margin",
      "Total Votes" = "total_votes",
      "Clinton %" = "clinton_prop",
      "Trump %" = "trump_prop"
    ),
    id = "elect_dist",
    popup.format = list(
      margin = list(fun = make_margin),
      total_votes = list(format = "f"),
      clinton_prop = list(fun = make_pct),
      trump_prop = list(fun = make_pct)
      )
  ) +
  tm_borders(col = "darkgray") +
  tm_shape(pres_2016_close_ed) +
  tm_borders(col = "green", lwd = 2)