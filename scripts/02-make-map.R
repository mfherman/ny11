library(tidyverse)
library(here)
library(janitor)
library(sf)
library(tmap)

# read in cd 11 ed shape file
cd11_ed_sf <- read_sf(here("data/geo/cd11_ed.geojson"))

cong_2016 <- read_csv(
  here("output/elect_results/2016_cd11.csv"),
  col_types = cols(year = "i", total_votes = "i", .default = "c")) %>%
  mutate_at(vars(ends_with("_prop")), as.double) %>%
  mutate_at(vars(ends_with("_tot")), as.integer)

nyed_16c <- read_sf(here("data/geo/nyed_16c"))
nyed_16d <- read_sf(here("data/geo/nyed_16d"))
nyed_17d <- read_sf(here("data/geo/nyed_17d"))

all.equal(nyed_16c, nyed_16d)

mapview(nyed_16c, col.regions = "red") + mapview(nyed_17d, col.regions = "blue")

# find combined eds
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
  distinct(elect_dist, ed_new)







cd11_new <- read_sf(here("data/geo/new_eds.geojson"))

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