source("R/_load_packages.R")
source("R/_project_directory.R")
source("R/_leaflet_helpers.R")
source("R/_color_palettes.R")

# 
# source("R/d_perm_counts.R")
# source("R/d_sdc_locss.R")

perm_locs <- read_rdata('perm_locs.rds')
sdc_locs <- read_rdata('sdc_locs.rds')

count_locs <- ssrc_leaflet_2() %>%
    leaflet::addCircleMarkers(
        data = perm_locs,
        color = '#6b135a',
        stroke = TRUE, 
        weight = 0.7, 
        opacity = 0.7, 
        fill = TRUE, 
        fillColor = ssrc_magenta, 
        fillOpacity = 0.6, 
        radius = 5.5,
        # label = perm_locs$name,
        # labelOptions = labelOptions(noHide = TRUE, 
        #                             textOnly = TRUE,
        #                             direction = "bottom",
        #                             style = list(
        #                                 "color" = ssrc_magenta,
        #                                 "font-family" = "Arial Black",
        #                                 "font-size" = "10px"
        #                             )),
        lng = perm_locs$long,
        lat = perm_locs$lat,
        popup = ~ paste0(
            "<b>", "Name: ", name, "</b>",
            "<br>",
            "Duration: ", obs_duration,
            "<br>",
            "Min Year: ", min_year,
            "<br>",
            "Max Year: ", max_year,
            "<extra></extra>"),
        group = "perm_count"
    ) %>% 
    leaflet::addCircleMarkers(
        data = sdc_locs,
        color = '#faa100',
        stroke = TRUE, 
        weight = 0.7, 
        opacity = 0.7, 
        fill = TRUE, 
        fillColor = "#faa100", 
        fillOpacity = 0.6, 
        radius = 2.5,
        # label = sdc_locs$name,
        # labelOptions = labelOptions(noHide = TRUE, 
        #                             textOnly = TRUE,
        #                             direction = "bottom",
        #                             style = list(
        #                                 "color" = "#faa100",
        #                                 "font-family" = "Arial Bold Italic",
        #                                 "font-style" = "italic",
        #                                 "font-size" = "10px"
        #                             )),
        lng = sdc_locs$long,
        lat = sdc_locs$lat,
        popup = ~ paste0(
            "<b>", "Name: ", name, "</b>",
            "<extra></extra>"),
        group = "sdc_locs"
    ) %>% 
    addLayersControl(
        baseGroups = c("Grey", "Negative",  "OpenStreetMap", "Satelite"),
        overlayGroups = c("Permanent Counters", 'Short Durection Counts'),
        options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addLegend(
        position = "topright",
        values = c("Permanent Counters", 'Short Durection Counts'),
        labels = c("Permanent Counters", 'Short Durection Counts'),
        opacity = 1.0,
        title='Map Key',
        colors = c(
            paste0(ssrc_magenta, "; border-radius: 50%; width:", 15, "px; height:", 15, "px"), 
            paste0('#faa100', "; border-radius: 50%; width:", 15, "px; height:", 15, "px")
        ), 
    )

count_locs
