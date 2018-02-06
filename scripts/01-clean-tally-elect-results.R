## tally votes and calculate vote proportions at the election district level
## currently for 7 elections congress, mayor, president, 2014 - 2017
## waiting for more data from older elections from board of elections

library(tidyverse)
library(janitor)
library(here)

# read in cd11 2016
cd11_2016_results <- read_csv(
  file = here("data/2016_cd11_results.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(
    elect_dist = paste0(ad, ed),
    tally = as.integer(tally),
    candidate = case_when(
      str_detect(unit_name, "Donovan") ~ "Donovan",
      str_detect(unit_name, "Reichard") ~ "Reichard",
      str_detect(unit_name, "Bardel") ~ "Bardel"
    )
  ) %>%
  filter(!is.na(candidate)) %>%
  select(elect_dist, candidate, tally)

# tally and tidy cd11 2016 cong results
cd11_2016_clean <- cd11_2016_results %>%
  group_by(elect_dist) %>%
  summarise(
    year = 2016L,
    type = "general",
    office = "cong",
    donovan = sum(tally[candidate == "Donovan"]),
    reichard = sum(tally[candidate == "Reichard"]),
    bardel = sum(tally[candidate == "Bardel"])
  ) %>%
  gather(candidate, tally, -(elect_dist:office)) %>%
  arrange(elect_dist)

# define vector of cd11 election districts
cd11_eds <- cd11_2016_results %>% distinct(elect_dist) %>% pull()

# read in 2016 pres results
pres_2016_results <- read_csv(
  file = here("data/2016_pres_results.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(
    elect_dist = paste0(ad, ed),
    tally = as.integer(tally),
    candidate = case_when(
      str_detect(unit_name, "Clinton") ~ "Clinton",
      str_detect(unit_name, "Stein") ~ "Stein",
      str_detect(unit_name, "Johnson") ~ "Johnson",
      str_detect(unit_name, "Trump") ~ "Trump"
    )
  ) %>%
  filter(!is.na(candidate) & elect_dist %in% cd11_eds) %>%
  select(elect_dist, candidate, tally)

# tally and tidy 2016 pres results
pres_2016_clean <- pres_2016_results %>%
  group_by(elect_dist) %>%
  summarise(
    year = 2016L,
    type = "general",
    office = "pres",
    clinton = sum(tally[candidate == "Clinton"]),
    trump = sum(tally[candidate == "Trump"]),
    stein = sum(tally[candidate == "Stein"]),
    johnson = sum(tally[candidate == "Johson"])
  ) %>%
  gather(candidate, tally, -(elect_dist:office)) %>%
  arrange(elect_dist)

# read in 2017 mayor results
mayor_2017_results <- read_csv(
  file = here("data/2017_mayor_results.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(
    elect_dist = paste0(ad, ed),
    tally = as.integer(tally),
    candidate = case_when(
      str_detect(unit_name, "Blasio") ~ "de Blasio",
      str_detect(unit_name, "Malliotakis") ~ "Malliotakis",
      str_detect(unit_name, "Browder") ~ "Browder",
      str_detect(unit_name, "Albanese") ~ "Albanese",
      str_detect(unit_name, "Dietl") ~ "Dietl",
      str_detect(unit_name, "Commey") ~ "Commey",
      str_detect(unit_name, "Tolkin") ~ "Tolkin"
    )
  ) %>%
  filter(!is.na(candidate) & elect_dist %in% cd11_eds) %>%
  select(elect_dist, candidate, tally)

# tally and tidy 2017 mayor results
mayor_2017_clean <- mayor_2017_results %>%
  group_by(elect_dist) %>%
  summarise(
    year = 2017L,
    type = "general",
    office = "mayor",
    deblasio = sum(tally[candidate == "de Blasio"]),
    malliotakis = sum(tally[candidate == "Malliotakis"]),
    commey = sum(tally[candidate == "Commey"]),
    browder = sum(tally[candidate == "Browder"]),
    albanese = sum(tally[candidate == "Albanese"]),
    dietl = sum(tally[candidate == "Dietl"]),
    tolkin = sum(tally[candidate == "Tolkin"])
  ) %>%
  gather(candidate, tally, -(elect_dist:office)) %>%
  arrange(elect_dist)

# read in 2015 cd11 results
cd11_2015_results <- read_csv(
  file = here("data/2015_cd11_results.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(
    elect_dist = paste0(ad, ed),
    tally = as.integer(tally),
    candidate = case_when(
      str_detect(unit_name, "Gentile") ~ "Gentile",
      str_detect(unit_name, "Donovan") ~ "Donovan",
      str_detect(unit_name, "Lane") ~ "Lane"
    )
  ) %>%
  filter(!is.na(candidate)) %>%
  select(elect_dist, candidate, tally)

# tally and tidy 2015 cd11 results
cd11_2015_clean <- cd11_2015_results %>%
  group_by(elect_dist) %>%
  summarise(
    year = 2015L,
    type = "special",
    office = "cong",
    donovan = sum(tally[candidate == "Donovan"]),
    gentile = sum(tally[candidate == "Gentile"]),
    lane = sum(tally[candidate == "Lane"])
  ) %>%
  gather(candidate, tally, -(elect_dist:office)) %>%
  arrange(elect_dist)

# read in 2014 cd11 results
cd11_2014_results <- read_csv(
  file = here("data/2014_cd11_results.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(
    elect_dist = paste0(ad, ed),
    tally = as.integer(tally),
    candidate = case_when(
      str_detect(unit_name, "Grimm") ~ "Grimm",
      str_detect(unit_name, "Recchia") ~ "Recchia",
      str_detect(unit_name, "Bardel") ~ "Bardel"
    )
  ) %>%
  filter(!is.na(candidate)) %>%
  select(elect_dist, candidate, tally)

# tally and tidy 2014 cd11 results
cd11_2014_clean <- cd11_2014_results %>%
  group_by(elect_dist) %>%
  summarise(
    year = 2014L,
    type = "general",
    office = "cong",
    grimm = sum(tally[candidate == "Grimm"]),
    recchia = sum(tally[candidate == "Recchia"]),
    bardel = sum(tally[candidate == "Bardel"])
  ) %>%
  gather(candidate, tally, -(elect_dist:office)) %>%
  arrange(elect_dist)

# read in 2016 dem primary results
dem_pres_prim_2016_results <- read_csv(
  file = here("data/2016_pres_dem_prim_results.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(
    elect_dist = paste0(ad, ed),
    tally = as.integer(tally),
    candidate = case_when(
      str_detect(unit_name, "Sanders") ~ "Sanders",
      str_detect(unit_name, "Clinton") ~ "Clinton"
    )
  ) %>%
  filter(!is.na(candidate) & elect_dist %in% cd11_eds) %>%
  select(elect_dist, candidate, tally)

# tally and tidy 2016 dem pres primary results
dem_pres_prim_2016_clean <- dem_pres_prim_2016_results %>%
  group_by(elect_dist) %>%
  summarise(
    year = 2016L,
    type = "dem primary",
    office = "pres",
    sanders = sum(tally[candidate == "Sanders"]),
    clinton = sum(tally[candidate == "Clinton"])
  ) %>%
  gather(candidate, tally, -(elect_dist:office)) %>%
  arrange(elect_dist)

# read in 2016 rep primary results
rep_pres_prim_2016_results <- read_csv(
  file = here("data/2016_pres_rep_prim_results.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(
    elect_dist = paste0(ad, ed),
    tally = as.integer(tally),
    candidate = case_when(
      str_detect(unit_name, "Trump") ~ "Trump",
      str_detect(unit_name, "Carson") ~ "Carson",
      str_detect(unit_name, "Cruz") ~ "Cruz",
      str_detect(unit_name, "Kasich") ~ "Kasich"
    )
  ) %>%
  filter(!is.na(candidate) & elect_dist %in% cd11_eds) %>%
  select(elect_dist, candidate, tally)

# tally and tidy 2016 rep pres primary results
rep_pres_prim_2016_clean <- rep_pres_prim_2016_results %>%
  group_by(elect_dist) %>%
  summarise(
    year = 2016L,
    type = "rep primary",
    office = "pres",
    trump = sum(tally[candidate == "Trump"]),
    carson = sum(tally[candidate == "Carson"]),
    cruz = sum(tally[candidate == "Cruz"]),
    kasich = sum(tally[candidate == "Kasich"])
  ) %>%
  gather(candidate, tally, -(elect_dist:office)) %>%
  arrange(elect_dist)

# define function to convert to wide and calc vote proportions for each elect dist
widen_add_tot_prop <- function(x) {
  cand_totals <- x %>%
    count(elect_dist, candidate, wt = tally) %>%
    mutate(candidate = paste0(candidate, "_tot")) %>%
    spread(candidate, n)
  
  dist_totals <- x %>%
    count(elect_dist, wt = tally) %>%
    rename(total_votes = n) %>%
    left_join(cand_totals, by = "elect_dist")
  
  x %>%
    add_count(elect_dist, wt = tally) %>%
    mutate(prop = tally / n) %>%
    select(-tally, -n) %>%
    mutate(candidate = paste0(candidate, "_prop")) %>%
    spread(key = candidate, value = prop) %>%
    left_join(dist_totals, by = "elect_dist") %>%
    select(elect_dist, year, type, office, total_votes, everything())
}


# process each election and write to csv
pres_2016_clean %>%
  widen_add_tot_prop() %>%
  write_csv(here("output/elect_results/2016_pres.csv"))

cd11_2014_clean %>%
  widen_add_tot_prop() %>%
  write_csv(here("output/elect_results/2014_cd11.csv"))

cd11_2015_clean %>%
  widen_add_tot_prop() %>%
  write_csv(here("output/elect_results/2015_cd11.csv"))

cd11_2016_clean %>%
  widen_add_tot_prop() %>%
  write_csv(here("output/elect_results/2016_cd11.csv"))

dem_pres_prim_2016_clean %>%
  widen_add_tot_prop() %>%
  write_csv(here("output/elect_results/2016_dem_prim.csv"))

mayor_2017_clean %>%
  widen_add_tot_prop() %>%
  write_csv(here("output/elect_results/2017_mayor.csv"))

rep_pres_prim_2016_clean %>%
  widen_add_tot_prop() %>%
  write_csv(here("output/elect_results/2016_rep_prim.csv"))