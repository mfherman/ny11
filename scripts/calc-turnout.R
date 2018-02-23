if(!exists("make_pct", mode = "function")) source("scripts/util.R")

ed_turnout <- read_csv(here("data/2016_turnout_si_count.csv")) %>%
  gather(key = "ED", value = "total_votes", -X1) %>%
  mutate(
    elect_dist = str_remove(ED, "ED_"),
    year = year(X1)
    ) %>%
  select(elect_dist, year, total_votes) %>%
  mutate_if(is.numeric, as.integer) %>%
  filter(year == 2016)


ed_registered <- read_csv(
  here("data/2016_ed_registered_si.csv"),
  col_names = c("elect_dist", "total_reg")) %>%
  mutate(elect_dist = str_remove(elect_dist, "ED_")) %>%
  select(elect_dist, total_reg) %>%
  mutate_if(is.numeric, as.integer)

si_turnout_2016 <-
  left_join(ed_turnout, ed_registered) %>%
  mutate(turnout_reg_prop = total_votes / total_reg)

write_csv(si_turnout_2016, here("output/2016_turnout_si.csv"))

cd11_sf <- read_sf(here("output/geo/cd11_demo_2016.geojson")) %>%
  left_join(si_turnout_2016) %>%
  mutate(turnout_tot_prop = total_votes / pop_tot_adj)

mapview(cd11_sf, zcol = "turnout_reg_prop")

