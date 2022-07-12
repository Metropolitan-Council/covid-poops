sewershed <- readRDS("~/MetC_Locals/MTS/covid-poops/metc-wastewater-covid-monitor/data/simple_wwtp_sewershed.rds") %>%
  filter(WWTP %in% c("Blue Lake", "Empire", 
                     "Metro", "Seneca"))
  pal <- leaflet::colorFactor(c("#1D94B7", # Blue Lake
                                "#6D3571", # Empire
                                "#0054A4", # Metro - councilBlue
                                "#84BB25"), # Seneca
                              domain = sewershed$WWTP
  )
  
  map <- 
    leaflet::leaflet() %>%
    leaflet::addMapPane("Main", zIndex = 400) %>%
    leaflet::addMapPane(name = "Carto Positron", zIndex = 430) %>%
    leaflet::addProviderTiles(
      "CartoDB.PositronOnlyLabels",
      options = leaflet::leafletOptions(pane = "Carto Positron"),
      group = "Carto Positron"
    ) %>%
    leaflet::addProviderTiles("CartoDB.PositronNoLabels",
                              group = "Carto Positron") %>%
    leaflet::addPolygons(
      data = sewershed,
      fillColor = ~ pal(WWTP),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "",
      fillOpacity = 0.8,
      highlightOptions = leaflet::highlightOptions(
        weight = 5,
        color = "white",
        dashArray = "",
        fillOpacity = 0.6,
        bringToFront = TRUE
      ),
      label = ~ WWTP,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    leaflet::addLegend(
      data = sewershed,
      title = "Service Area",
      position = "topright",
      pal = pal,
      values = ~ WWTP,
      opacity = 0.7
    ) %>%
    leaflet.extras::addFullscreenControl(pseudoFullscreen = TRUE)
  map

