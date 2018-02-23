if(!exists("make_pct", mode = "function")) source("scripts/util.R")

pres_2016 <- read_csv(
  here("output/elect_results/2016_pres.csv"),
  col_types = cols(year = "i", total_votes = "i", .default = "c")) %>%
  mutate_at(vars(ends_with("_prop")), as.double) %>%
  mutate_at(vars(ends_with("_tot")), as.integer)

# create dem/rep margins
pres_2016_margin <- pres_2016 %>%
  mutate(
    margin_16 = clinton_prop - trump_prop,
    total_ed_16 = sum(total_votes, na.rm = TRUE),
    clinton_ed_16 = sum(clinton_tot, na.rm = TRUE) / total_ed_16,
    trump_ed_16 = sum(trump_tot, na.rm = TRUE) / total_ed_16,
    margin_ed_16 = clinton_ed_16 - trump_ed_16,
    dif_avg_16 = margin_16 - margin_ed_16
    ) %>%
  select(
    elect_dist,
    total_votes_16 = total_votes,
    margin_16,
    dif_avg_16,
    clinton_tot_16 = clinton_tot,
    trump_tot_16 = trump_tot,
    clinton_prop_16 = clinton_prop,
    trump_prop_16 = trump_prop
    )

pres_2012 <- read_csv(
  here("output/elect_results/pres_2012_adj.csv"),
  col_types = cols(.default = "c")) %>%
  mutate_at(vars(contains("_prop")), as.double) %>%
  mutate_at(vars(contains("tot")), as.integer)

# create dem/rep margins, join with ed geo file
pres_2012_margin <- pres_2012 %>%
  mutate(
    margin_12 = obama_prop_adj - romney_prop_adj,
    total_ed_12 = sum(total_votes_12_adj, na.rm = TRUE),
    obama_ed_12 = sum(obama_tot_adj, na.rm = TRUE) / total_ed_12,
    romney_ed_12 = sum(romney_tot_adj, na.rm = TRUE) / total_ed_12,
    margin_ed_12 = obama_ed_12 - romney_ed_12,
    dif_avg_12 = margin_12 - margin_ed_12
  )  %>%
  select(
    elect_dist,
    total_votes_12 = total_votes_12_adj,
    margin_12,
    dif_avg_12,
    obama_tot_12 = obama_tot_adj,
    romney_tot_12 = romney_tot_adj,
    obama_prop_12 = obama_prop_adj,
    romney_prop_12 = romney_prop_adj
  )

pres_margin_16_12 <-
  left_join(pres_2016_margin, pres_2012_margin, by = "elect_dist") %>%
  mutate(
    delta_margin_16_12 = margin_16 - margin_12,
    delta_tot_vote_16_12 = total_votes_16 - total_votes_12,
    delta_avg_margin_16_12 = dif_avg_16 - dif_avg_12,
    pct_change_tot_vote_16_12 = delta_tot_vote_16_12 / total_votes_16
    ) %>%
  select(
    elect_dist,
    margin_16,
    margin_12,
    delta_margin_16_12,
    delta_avg_margin_16_12,
    delta_tot_vote_16_12,
    pct_change_tot_vote_16_12,
    everything()
  ) %>%
  arrange(desc(delta_margin_16_12))

cd11_sf <- read_sf(here("output/geo/cd11_ed_2016.geojson"))

# select eds with largest swing towards R from 12 to 16
# filter eds that haven't changed more than 1000 sq m between 12/16




tmap_mode("view")
margin_map <- left_join(cd11_sf, pres_margin_16_12) %>%
  filter(elect_dist %in% current_van) %>%
  tm_shape(name = "2016 Presidential") +
  tm_fill(
    col = "delta_margin_16_12",
    palette = "RdBu",
    n = 7,
    style = "fixed",
    breaks = seq(-0.4, 0.3, by = 0.1),
    title = "&#916; D/R Margin <br> Relative to CD11 <br> 2012 to 2016",
    textNA = "No Votes",
    popup.vars = c(
      "Relative Change in Margin" = "delta_avg_margin_16_12",
      "Change in Total Votes" = "pct_change_tot_vote_16_12",
      "2016 Margin" = "margin_16",
      "2012 Margin" = "margin_12",
      "2016 Clinton %" = "clinton_prop_16",
      "2016 Trump %" = "trump_prop_16",
      "2012 Obama %" = "obama_prop_12",
      "2012 Romney %" = "romney_prop_12"
    ),
    id = "elect_dist",
    popup.format = list(
      delta_avg_margin_16_12 = list(fun = make_margin),
      pct_change_tot_vote_16_12 = list(fun = make_pct),
      margin_16 = list(fun = make_margin),
      margin_12 = list(fun = make_margin),
      clinton_prop_16 = list(fun = make_pct),
      trump_prop_16 = list(fun = make_pct),
      obama_prop_12 = list(fun = make_pct),
      romney_prop_12 = list(fun = make_pct)
    )
  ) +
  tm_borders(col = "darkgray") +
  add_basemap

margin_map <- left_join(cd11_sf, pres_margin_16_12) %>%
  filter(elect_dist %in% current_van) %>%
  tm_shape(name = "2016 Presidential") +
  tm_fill(
    col = "delta_margin_16_12",
    palette = "RdBu",
    n = 7,
    style = "fixed",
    breaks = seq(-0.4, 0.3, by = 0.1),
    title = "&#916; D/R Margin <br> Relative to CD11 <br> 2012 to 2016",
    textNA = "No Votes",
    popup.vars = c(
      "Relative Change in Margin" = "delta_avg_margin_16_12",
      "Abs Change in Margin" = "delta_margin_16_12",
      "Change in Total Votes" = "pct_change_tot_vote_16_12",
      "2016 Margin" = "margin_16",
      "2012 Margin" = "margin_12",
      "2016 Clinton %" = "clinton_prop_16",
      "2016 Trump %" = "trump_prop_16",
      "2012 Obama %" = "obama_prop_12",
      "2012 Romney %" = "romney_prop_12"
    ),
    id = "elect_dist",
    popup.format = list(
      delta_avg_margin_16_12 = list(fun = make_margin),
      pct_change_tot_vote_16_12 = list(fun = make_pct),
      delta_margin_16_12 = list(fun = make_margin),
      margin_16 = list(fun = make_margin),
      margin_12 = list(fun = make_margin),
      clinton_prop_16 = list(fun = make_pct),
      trump_prop_16 = list(fun = make_pct),
      obama_prop_12 = list(fun = make_pct),
      romney_prop_12 = list(fun = make_pct)
    )
  ) +
  tm_borders(col = "darkgray") +
  add_basemap

margin_map %>% add_leaflet_opts(search = "2016 Presidential")



pres_margin_16_12 %>%
  filter(elect_dist %in% current_van) %>%
  View()


rel_margin_map <- left_join(cd11_sf, pres_margin_16_12) %>%
  filter(elect_dist %in% current_van) %>%
  tm_shape(name = "2016 Presidential") +
  tm_fill(
    col = "delta_avg_margin_16_12",
    palette = "RdBu",
    n = 7,
    style = "fixed",
    breaks = c(-1, seq(-0.2, 0.35, by = 0.1), 1),
    title = "% Change in Votes Cast <br> 2012 to 2016",
    textNA = "No Votes",
    popup.vars = c(
      "Relative Change in Margin" = "delta_avg_margin_16_12",
      "Change in Total Votes" = "pct_change_tot_vote_16_12",
      "2016 Margin" = "margin_16",
      "2012 Margin" = "margin_12",
      "2016 Clinton %" = "clinton_prop_16",
      "2016 Trump %" = "trump_prop_16",
      "2012 Obama %" = "obama_prop_12",
      "2012 Romney %" = "romney_prop_12"
    ),
    id = "elect_dist",
    popup.format = list(
      delta_avg_margin_16_12 = list(fun = make_margin),
      pct_change_tot_vote_16_12 = list(fun = make_pct),
      margin_16 = list(fun = make_margin),
      margin_12 = list(fun = make_margin),
      clinton_prop_16 = list(fun = make_pct),
      trump_prop_16 = list(fun = make_pct),
      obama_prop_12 = list(fun = make_pct),
      romney_prop_12 = list(fun = make_pct)
    ),
    labels = c(
      "Less than -20%",
      "-20% to -10%",
      "-10% to 0%",
      "0% to +10%",
      "+10% to +20%",
      "+20% to +30%",
      "More than 30%"
      )
    ) +
  tm_borders(col = "darkgray") +
  add_basemap


rel_margin_map %>% add_leaflet_opts(search = "2016 Presidential")


change_tot_map <- left_join(cd11_sf, pres_margin_16_12) %>%
  filter(elect_dist %in% current_van) %>%
  tm_shape(name = "2016 Presidential") +
  tm_fill(
    col = "pct_change_tot_vote_16_12",
    palette = "RdBu",
    n = 7,
    style = "fixed",
    breaks = c(-1, seq(-0.2, 0.35, by = 0.1), 1),
    title = "% Change in Votes Cast <br> 2012 to 2016",
    textNA = "No Votes",
    popup.vars = c(
      "Relative Change in Margin" = "delta_avg_margin_16_12",
      "Change in Total Votes" = "pct_change_tot_vote_16_12",
      "2016 Margin" = "margin_16",
      "2012 Margin" = "margin_12",
      "2016 Clinton %" = "clinton_prop_16",
      "2016 Trump %" = "trump_prop_16",
      "2012 Obama %" = "obama_prop_12",
      "2012 Romney %" = "romney_prop_12"
    ),
    id = "elect_dist",
    popup.format = list(
      delta_avg_margin_16_12 = list(fun = make_margin),
      pct_change_tot_vote_16_12 = list(fun = make_pct),
      margin_16 = list(fun = make_margin),
      margin_12 = list(fun = make_margin),
      clinton_prop_16 = list(fun = make_pct),
      trump_prop_16 = list(fun = make_pct),
      obama_prop_12 = list(fun = make_pct),
      romney_prop_12 = list(fun = make_pct)
    ),
    labels = c(
      "Less than -20%",
      "-20% to -10%",
      "-10% to 0%",
      "0% to +10%",
      "+10% to +20%",
      "+20% to +30%",
      "More than 30%"
    )
  ) +
  tm_borders(col = "darkgray") +
  add_basemap


change_tot_map %>% add_leaflet_opts(search = "2016 Presidential")

