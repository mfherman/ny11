library(tidyverse)
library(janitor)
library(here)
library(readxl)

cd11_cong_2012 <- read_excel(
  path = here("data/2012_nyc_all_results.xls"),
  sheet = "Congress",
  col_types = "text",
  skip = 1
) %>%
  clean_names() %>%
  filter(cd == 11) %>%
  mutate_at(vars(murphy_d:bardel_g), as.integer) %>%
  mutate(
    year = 2012L,
    type = "general",
    office = "congressional",
    murphy_tot = murphy_d + murphy_wf,
    grimm_tot = grimm_r + grimm_c,
    total_votes = murphy_tot + grimm_tot + bardel_g,
    murphy_prop = murphy_tot / total_votes,
    grimm_prop = grimm_tot / total_votes
    ) %>%
  select(
    elect_dist = ad_ed, 
    year:office,
    total_votes,
    murphy_tot:grimm_prop
    )

write_csv(cd11_cong_2012, here("output/elect_results/2012_cong.csv"))

cd11_2012_eds <- cd11_cong_2012 %>% pull(elect_dist)

# read in 2012
cd11_2012_results <- read_excel(
  path = here("data/2012_pres_results.xls"),
  sheet = "Sheet2",
  col_types = "text"
) %>%
  clean_names() %>%
  mutate_at(vars(total_votes:blank), as.integer) %>%
  mutate(
    year = 2012L,
    type = "general",
    office = "president",
    obama_tot = barack_obama_democratic + barack_obama_working_families,
    romney_tot = mitt_romney_republican + mitt_romney_conservative,
    obama_prop = obama_tot / total_votes,
    romney_prop = romney_tot / total_votes,
  ) %>%
  select(
    elect_dist = district,
    year:office,
    total_votes,
    obama_tot:romney_prop
  ) %>%
  filter(elect_dist %in% cd11_2012_eds)

write_csv(cd11_2012_results, here("output/elect_results/2012_pres.csv"))