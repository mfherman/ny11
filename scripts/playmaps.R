library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)
options(tigris_use_cache = TRUE)

# total population B01001_001
# % non hispanic white B03002_003
# % non hispanic black B03002_004
# % hispanic B03002_012
# median age B01002_001
# median hh income B19013_001
# below 100% poverty level B06012_002
# poverty level denom B06012_001
# gini index B19083_001
# pub assistance/food stamp B19058_002
# total households B19058_001
# median gross rent as a pct of income B25071_001
# lived in owner-occuppied B07013_002
# live in renter-occuppied B07013_003
# total housesholds for owner/rent B07013_001

# select counties
si_bk <- c("Richmond", "Kings")

# select variables
variables <- c("B01001_001", "B03002_003", "B03002_004", "B03002_012",
               "B01002_001", "B19013_001", "B06012_002", "B06012_001",
               "B19083_001", "B19058_002", "B19058_001", "B25071_001",
               "B07013_002", "B07013_003", "B07013_001")

# download acs 5 year estimates
si_bk_bg <- get_acs(
  state = "NY",
  county = si_bk,
  geography = "block group",
  variables = variables,
  survey = "acs5",
  year = 2016,
  geometry = TRUE,
  output = "wide"
)

# percentage and rename variables
si_bk_stat <- si_bk_bg %>%
  mutate(
    white_pct = B03002_003E / B01001_001E * 100,
    black_pct = B03002_004E / B01001_001E * 100,
    hisp_pct = B03002_012E / B01001_001E * 100,
    pov_pct = B06012_002E / B06012_001E * 100,
    pub_assist_pct = B19058_002E / B19058_001E * 100,
    own_pct = B07013_002E / B07013_001E * 100,
    rent_pct = B07013_003E / B07013_001E * 100
  ) %>%
  select(
    geoid = GEOID,
    name = NAME,
    pop_tot = B01001_001E,
    med_age = B01002_001E,
    med_hhinc = B19013_001E,
    gini = B19083_001E,
    med_rent_to_inc = B25071_001E,
    white_pct:rent_pct
  )

# make a simple choropleth map
tm_shape(si_bk_stat) +
  tm_fill(
    col = "med_hhinc",
    n = 5,
    style = "jenks"
  )

hi <- read_sf("./data/geo/cd11_ed.geojson")


mapview::mapview(hi, col.regions = "blue") + mapview::mapview(si_bk_stat, col.regions = "red")

# take it to the next level with an interactive map
tmap_mode(mode = "view")
tm_shape(si_bk_stat) +
  tm_fill(
    col = "med_hhinc",
    palette = "GnBu",
    contrast = c(0.2, 0.8),
    n = 5,
    style = "jenks",
    title = "Median HH Income",
    textNA = "No Income Data",
    legend.format = list(
      fun = function(x) {
        paste0("$", formatC(x, digits = 0 , format = "f"))
      }
    ),
    popup.vars = c(
      "Total Population" = "pop_tot"
    ),
    id = "name",
    popup.format = list(
      pop_tot = list(format = "f")
    )
  ) +
  tm_borders(col = "dark grey")
