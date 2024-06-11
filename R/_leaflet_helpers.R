knitr::opts_chunk$set(
  echo = F,
  message = F,
  warning = F,
  fig.pos = "H",
  out.width = "100%",
  dpi = 300
)

if (exists("bbox") == FALSE) {
  bbox <- list(
    xmin = -93.521749,
    ymin = 44.854051,
    xmax = -92.899649,
    ymax = 45.081892
  )
}

ssrc_leaflet <- function() {
  leaflet::leaflet() %>%
    leaflet::addMapPane(
      name = "Carto Positron", 
      zIndex = 430
    ) %>%
    leaflet::addProviderTiles(
      "CartoDB.PositronOnlyLabels",
      options = leaflet::leafletOptions(pane = "Carto Positron"),
      group = "Carto Positron"
    ) %>%
    leaflet::addProviderTiles(
      "CartoDB.PositronNoLabels",
      group = "Carto Positron"
    ) %>% 
    leaflet::addProviderTiles("Stamen.Watercolor") %>%
    leaflet::addProviderTiles("Stamen.TonerHybrid")
  # %>%
  #   leaflet::fitBounds(
  #     bbox$xmin, 
  #     bbox$ymin, 
  #     bbox$xmax, 
  #     bbox$ymax)
}

ssrc_leaflet_2 <- function() {
  leaflet::leaflet() %>%
    leaflet::addProviderTiles(providers$CartoDB.DarkMatter, group = 'Negative') %>%
    leaflet::addTiles(group = "OpenStreetMap") %>%
    leaflet::addProviderTiles(providers$CartoDB.Positron, group = 'Grey', options = providerTileOptions(noWrap = TRUE)) %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group='Satelite') %>% 
    leaflet::addLayersControl(baseGroups = c("Grey", "Negative",  "OpenStreetMap", "Satelite"), position = 'topright') 
}

cli::cli_inform(
  c("v" = "loaded leaflet helpers...NOW GET TO MAPPING!\n")
)