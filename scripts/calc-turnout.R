if(!exists("make_pct", mode = "function")) source("scripts/util.R")

### read in si registered voters
si_reg <- read_csv(here("data/si_registered_voters.csv")) %>% 
  gather(key = "ED", value = "total_reg", -Year) %>%
  mutate(elect_dist = str_remove(ED, "ED_")) %>%
  select(elect_dist, year = Year, total_reg) %>%
  mutate_if(is.numeric, as.integer)

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





### 2016 registered voters
ed_registrered_bk <- read_csv(
  here("data/ed_registered_2016_brooklyn.csv"),
  col_names = c("elect_dist", "total_reg")) %>%
  mutate(elect_dist = str_remove(elect_dist, "ED_")) %>%
  mutate_if(is.numeric, as.integer)

ed_registered_2016 <- read_csv(
  here("data/2016_ed_registered_si.csv"),
  col_names = c("elect_dist", "total_reg")) %>%
  mutate(elect_dist = str_remove(elect_dist, "ED_")) %>% 
  mutate_if(is.numeric, as.integer) %>%
  bind_rows(ed_registrered_bk)

write_csv(ed_registered_2016, here("output/2016_registered_all.csv"))

### 2014 registered voters
ed_registrered_bk_14 <- read_csv(
  here("data/ed_registered_2016_brooklyn.csv"),
  col_names = c("elect_dist", "total_reg")) %>%
  mutate(elect_dist = str_remove(elect_dist, "ED_")) %>%
  mutate_if(is.numeric, as.integer)

ed_registered_2014 <- read_csv(
  here("data/2014_ed_registered_si.csv"),
  col_names = c("elect_dist", "total_reg")) %>%
  mutate(elect_dist = str_remove(elect_dist, "ED_")) %>% 
  mutate_if(is.numeric, as.integer) %>%
  bind_rows(ed_registrered_bk_14)

write_csv(ed_registered_2014, here("output/2014_registered_all.csv"))


### 2012 registered voters
ed_registrered_bk_12 <- read_csv(
  here("data/ed_registered_2012_brooklyn.csv"),
  col_names = c("elect_dist", "total_reg")) %>%
  mutate(elect_dist = str_remove(elect_dist, "ED_")) %>%
  mutate_if(is.numeric, as.integer)

ed_registered_2012 <- read_csv(
  here("data/2012_ed_registered_si.csv"),
  col_names = c("elect_dist", "total_reg")) %>%
  mutate(elect_dist = str_remove(elect_dist, "ED_")) %>% 
  mutate_if(is.numeric, as.integer) %>%
  bind_rows(ed_registrered_bk_12)

write_csv(ed_registered_2012, here("output/2016_registered_all.csv"))


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


