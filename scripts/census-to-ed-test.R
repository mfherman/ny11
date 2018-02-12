library(tidyverse)
library(here)
library(sf)
library(mapview)
library(tidycensus)
library(tmap)

options(tigris_use_cache = TRUE)

# download census data and geometry
variables <- c(
  "B01001_001",
  "B03002_003",
  "B03002_004",
  "B03002_006",
  "B03002_012",
  "B01002_001",
  "B19013_001",
  "B19058_002",
  "B25071_001",
  "B17021_002",
  "B17021_001",
  "B15003_001", # education
  "B15003_022",
  "B15003_023",
  "B15003_024",
  "B15003_025",
  "B25003_001",
  "B25003_003", # renter occupied,
  "B23025_003",
  "B23025_005" # unemployed
  )

bg <- get_acs(
  state = "NY",
  county = c("Richmond", "Kings"),
  geography = "block group",
  variables = variables,
  survey = "acs5",
  year = 2016,
  geometry = TRUE,
  output = "wide"
)

bg_stat <- bg %>%
  mutate(
    pop_other = B01001_001E - (B03002_003E + B03002_004E + B03002_012E + B03002_006E),
    white_pct = B03002_003E / B01001_001E,
    black_pct = B03002_004E / B01001_001E,
    hisp_pct = B03002_012E / B01001_001E,
    asian_pct = B03002_006E / B01001_001E,
    other_pct = white_pct + black_pct + hisp_pct + asian_pct,
    pov_pct = B17021_002E / B17021_001E,
    pop_ba = B15003_022E + B15003_023E + B15003_024E + B15003_025E,
    pop_ba_denom = B15003_001E,
    pop_rent = B25003_003E,
    pop_rent_denom = B25003_001E,
    pop_unempl = B23025_005E,
    pop_unempl_denom = B23025_003E,
    rent_pct = B25003_003E / B25003_001E,
    ba_pct = pop_ba / B15003_001E,
    unempl_pct = B23025_005E / B23025_003E
  ) %>%
  select(
    geoid = GEOID,
    name = NAME,
    pop_tot = B01001_001E,
    med_age = B01002_001E,
    med_hhinc = B19013_001E,
    med_rburd = B25071_001E,
    pop_pov = B17021_002E,
    pop_pov_denom = B17021_001E,
    pop_white = B03002_003E,
    pop_black = B03002_004E,
    pop_hisp = B03002_012E,
    pop_asian = B03002_006E,
    pop_other,
    pop_ba:pop_unempl_denom,
    white_pct:unempl_pct
  ) %>%
  st_transform(4326)

# read in cd11 borders
cd11_border <- read_sf(here("data/geo/nycg_16c")) %>%
  filter(CongDist == 11) %>%
  st_transform(4326)

# clip to waterline cd11
bg_clip <- bg_stat %>%
  st_intersection(cd11_border) %>%
  select(-(CongDist:Shape_Area))

# read in cd11 eds
cd11 <- read_sf(here("output/geo/cd11_ed_2016.geojson"))

# define function to estimate census demographics in election districts
# intersect eds with block groups and calc proportion of area, etc.
bg_ed_calc <- function(ed, bg) {
  
  ed <- ed %>% mutate(area_ed = st_area(.))
  bg <- bg %>% mutate(area_bg = st_area(.))

  # intersect eds with block groups
  # calculate prop of bg in ed and prop of ed of each bg
  # calculate adjusted population totals
  ed_bg <- st_intersection(ed, bg) %>%
    mutate(
      area_bg_ed = st_area(.),
      area_bg_prop = as.numeric(area_bg_ed / area_bg),
      area_ed_prop = as.numeric(area_bg_ed / area_ed)
    ) %>%
    mutate_at(vars(contains("pop")), funs(adj = (. * area_bg_prop))) %>%
    select(
      elect_dist, geoid, area_bg_ed:area_ed_prop,
      med_age:med_rburd, pop_tot_adj:pop_unempl_denom_adj
    )

  pop_sum <- ed_bg %>%
    group_by(elect_dist) %>%
    summarize_at(vars(contains("pop")), funs(sum(., na.rm = TRUE))) %>%
    mutate_at(vars(pop_white_adj:pop_other_adj), funs(pct = (. / pop_tot_adj))) %>%
    mutate_at(vars(pop_pov_adj), funs(pop_pov_pct = (. / pop_pov_denom_adj))) %>%
    mutate_at(vars(pop_rent_adj), funs(pop_rent_pct = (. / pop_rent_denom_adj))) %>%
    mutate_at(vars(pop_unempl_adj), funs(pop_unempl_pct = (. / pop_unempl_denom_adj))) %>%
    mutate_at(vars(pop_ba_adj), funs(pop_ba_pct = (. / pop_ba_denom_adj))) %>%
    st_set_geometry(NULL)

  med_sum <- ed_bg %>%
    group_by(elect_dist) %>%
    summarize_at(vars(contains("med")), funs(weighted.mean(., pop_tot_adj, na.rm = TRUE))) %>%
    st_set_geometry(NULL)

  demo <- left_join(pop_sum, med_sum, by = "elect_dist")
  
  left_join(ed, demo)
}

cd11_demo_2016 <- bg_ed_calc(cd11, bg_clip)

mapview(cd11_demo_2016, zcol = "pop_rent_pct")

st_write(cd11_demo_2016, here("output/geo/cd11_demo_2016.geojson"))



mapview(cd11_demo, zcol = "pop_black_adj_pct", legend = TRUE)

cong_2016 <- read_csv(
  here("output/elect_results/2016_cd11.csv"),
  col_types = cols(year = "i", total_votes = "i", .default = "c")) %>%
  mutate_at(vars(ends_with("_prop")), as.double) %>%
  mutate_at(vars(ends_with("_tot")), as.integer)

# create dem/rep margins, join with ed geo file
cong_2016_margin <- cong_2016 %>%
  mutate(margin = reichard_prop - donovan_prop) %>%
  select(elect_dist, total_votes, margin, reichard_tot,
         donovan_tot, reichard_prop, donovan_prop)

cong_2016_to_map <- left_join(cd11_demo, cong_2016_margin) %>%
  mutate(elect_dist = paste("ED", elect_dist))

cong_2016_close_rep <- cong_2016_to_map %>%
  filter(margin >= -0.1 & margin < 0)

cong_2016_close_dem <- cong_2016_to_map %>%
  filter(margin >= 0 & margin <= 0.1)


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


tmap_mode("view")
tm_shape(cong_2016_to_map, name = "2016 Congressional") +
  tm_fill(
    col = "margin",
    palette = "RdBu",
    alpha = 0.5,
    style = "cont",
    breaks = seq(-1, 1, by = 0.2),
    title = "D/R Margin",
    textNA = "No Votes",
    popup.vars = c(
      "Dem/Rep Margin" = "margin",
      "Total Votes" = "total_votes",
      "Reichard %" = "reichard_prop",
      "Donovan %" = "donovan_prop"
    ),
    id = "elect_dist",
    popup.format = list(
      margin = list(fun = make_margin),
      total_votes = list(format = "f", digits = 0),
      reichard_prop = list(fun = make_pct),
      donovan_prop = list(fun = make_pct)
    )
  ) +
  tm_borders(col = "darkgray") +
  tm_shape(cong_2016_to_map, name = "Demographics") +
  tm_fill(
    col = "med_hhinc",
    palette = "Greens",
    contrast = c(0.2, 0.8),
    style = "cont",
    title = "HH Income",
    textNA = "No Votes",
    id = "elect_dist"
    ) +
  tm_borders(col = "darkgray") +
  tm_shape(cong_2016_close_dem, name = "Lean Dem") +
  tm_borders(col = "orange", lwd = 2) +
  tm_shape(cong_2016_close_rep, name = "Lean Rep") +
  tm_borders(col = "green", lwd = 2) +
  tm_view(
    alpha = 0.85,
    basemaps = "Stamen.TonerLite",
    legend.position = c("right", "bottom")
  )
        
        

# sum_bg <- function(x) {
#   summary <- summarise_at(x, vars(med_hhinc, pop_pov_pct, pop_black_adj_pct),
#                           funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE)))
#   return(summary)
# }