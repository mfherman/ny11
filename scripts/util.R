##### LOAD MY LIBRARIES #####

library(tidyverse)
library(janitor)
library(lubridate)
library(here)
library(sf)
library(tmap)
library(mapview)
library(leaflet)
library(leaflet.extras)
library(knitr)
library(kableExtra)
library(DT)

##### DEFINE HELPER FUNCTIONS #####

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

# define a little helper function to format $$
make_dollar <- function(x, digits = 0) {
  paste0("$", formatC(x, digits = digits, format = "f", big.mark = ","))
}

# add basemap to tmaps
add_basemap <- 
  tm_view(
    alpha = 0.85,
    basemaps = "Stamen.TonerLite",
    legend.position = c("right", "bottom")
  )

# add addiontal leaflet options
add_leaflet_opts <- function(x, search){
  tmap_leaflet(x) %>%
    addSearchFeatures(
      targetGroups = search,
      options = searchFeaturesOptions(
        zoom = 14,
        openPopup = TRUE,
        collapsed = FALSE,
        position = "topright",
        textPlaceholder = "Search EDs..."
      )
    ) %>%
    addFullscreenControl(pseudoFullscreen = TRUE)
}


# define van districts for filtering

current_van <- c(
  "46035",
  "46047",
  "46049",
  "46050",
  "46051",
  "46052",
  "46053",
  "46066",
  "46067",
  "46068",
  "46069",
  "46070",
  "46071",
  "47002",
  "47005",
  "47006",
  "49047",
  "49048",
  "49049",
  "49051",
  "49052",
  "63011",
  "63013",
  "63016",
  "63024",
  "63053",
  "63055",
  "63060",
  "63069",
  "63068",
  "63056",
  "63054",
  "61024",
  "61039",
  "61038",
  "63003",
  "63061",
  "63073",
  "63074",
  "64043",
  "64045",
  "64046",
  "64047",
  "61052",
  "64053",
  "64038"
)

#############################################
