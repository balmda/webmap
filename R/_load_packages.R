rm(packages_loaded)
if (exists("packages_loaded") == FALSE) {
  wants_packages <- c(
    "RPostgreSQL", 
    "dplyr", 
    "magrittr", 
    "tidyverse", 
    "kableExtra", 
    "readr", 
    "janitor", 
    "scales",
    "cowplot", 
    "data.table", 
    "docstring",
    "paletteer", 
    "sf", 
    "leaflet", 
    "ggmap", 
    "htmlwidgets", 
    "ggplot2", 
    "plotly", 
    "lubridate", 
    "RColorBrewer", 
    "ggpattern", 
    "gt", 
    "extrafont"
  )
  has_packages <- wants_packages %in% rownames(installed.packages())
  if (any(has_packages == FALSE)) {
    install.packages(wants_packages[!has_packages])
  }
  invisible(lapply(wants_packages, library, character.only = TRUE))
  
  packages_loaded <- TRUE
  
}
packages_loaded <- FALSE

cli::cli_inform(
  c("v" = "loaded packages\n")
)
