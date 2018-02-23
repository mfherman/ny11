library(tidyverse)
library(janitor)

load("/Users/matthewherman/Downloads/NY_2008.RData")

cd11_2008_sf <- read_sf(here("output/geo/cd11_ed_2008.geojson")) %>%
  pull(elect_dist)

pres_2008 <- 
  as_tibble(x) %>%
  clean_names() %>%
  filter(county_name %in% c("Kings", "Richmond")) %>%
  mutate(
    ed = as.character(ed),
    ed = case_when(
      nchar(ed) == 1 ~ paste0("00", ed),
      nchar(ed) == 2 ~ paste0("0", ed),
      nchar(ed) == 3 ~ paste0(ed)
    ),
    elect_dist = paste0(ad, ed)) %>%
  filter(elect_dist %in% cd11_2008_sf) %>%
  transmute(
    elect_dist = elect_dist,
    year = 2008L,
    type = "general",
    office = "president",
    total_votes = g_2008_usp_tv,
    obama_tot = g_2008_usp_dv,
    mccain_tot = g_2008_usp_rv,
    obama_prop = obama_tot / total_votes,
    mccain_prop = mccain_tot / total_votes
    )

cong_2008 <- 
  as_tibble(x) %>%
  clean_names() %>%
  filter(county_name %in% c("Kings", "Richmond")) %>%
  mutate(
    ed = as.character(ed),
    ed = case_when(
      nchar(ed) == 1 ~ paste0("00", ed),
      nchar(ed) == 2 ~ paste0("0", ed),
      nchar(ed) == 3 ~ paste0(ed)
    ),
    elect_dist = paste0(ad, ed)) %>%
  filter(elect_dist %in% cd11_2008_sf) %>%
  transmute(
    elect_dist = elect_dist,
    year = 2008L,
    type = "general",
    office = "congress",
    total_votes = g_2008_usp_tv,
    mcmahon_tot = g_2008_usp_dv,
    straniere_tot = g_2008_usp_rv,
    mcmahon_prop = mcmahon_tot / total_votes,
    straniere_prop = straniere_tot / total_votes
  )
  
  
write_csv(pres_2008, here("output/elect_results/2008_pres.csv"))
write_csv(cong_2008, here("output/elect_results/2008_cong.csv"))
