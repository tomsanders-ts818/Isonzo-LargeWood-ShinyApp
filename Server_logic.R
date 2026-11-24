#################################################################
##          Practical 6 for GEOM184 - Open Source GIS          ##
##                         05/11/2026                          ##
##                     Creating a ShinyApp                     ##
##                          Server.R                           ##
##        code by Diego Panici (d.panici@exeter.ac.uk)         ##
##          adapted by Tom Sanders for the River Isonzo        ##
#################################################################

# S1 Render leaflet map ----
# Leaflet map output
# Changed: bridges removed from here and added with pop ups below (Task 4)
output$map <- renderLeaflet({
  leaflet() %>%
    setView(lng=13.533545, lat=45.850065, zoom=11.3) %>%
    addProviderTiles (providers$OpenStreetMap, group = "Colour") %>%
    # Satellite basemap (imagery used to identify large wood)
    addProviderTiles (providers$Esri.WorldImagery, group = "Satellite") %>%
    # Task 7: slope raster
    addRasterImage(slope, colors = pal_slope, opacity = 0.6, project = FALSE,
                   maxBytes = 8 * 1024 * 1024, group = "Slope") %>%
    # Changed: rasters already in web map projection (project = FALSE)
    addRasterImage(heatmap, colors = pal_heatmap, opacity = 0.7, project = FALSE,
                   layerId = "LW density (KDE)", group = "Heatmap") %>%
    # Section 2.5: image query removed, embeds every raster cell in the page and exceeds server memory
    # Extra layers: analysis extent and upstream zones
    addPolygons(data = extent_buffer, color = "#333333", weight = 1.5,
                dashArray = "6", fillColor = "#808080", fillOpacity = 0.3,
                label = "Analysis extent (channel and floodplain)",
                group = "Analysis extent") %>%
    addPolygons(data = upstream_zones, color = "#6a3d9a", weight = 1.5,
                fillColor = "#6a3d9a", fillOpacity = 0.15,
                popup = ~paste("<b>2 km upstream zone of:</b>", Name,
                               "<br><b>Large wood in zone:</b>", lw_count,
                               "<br><b>Tree cover:</b>", round(tree_ratio * 100, 1), "%"),
                group = "Upstream zones (2 km)") %>%
    addPolylines(data = river, color = "blue",
                 weight = 2, opacity = 0.8, group = "River") %>%
    # Extra layers: reaches coloured by morphology
    addPolylines(data = reaches, color = ~pal_morph(Morph),
                 weight = 5, opacity = 0.9,
                 popup = ~paste("<b>Setting:</b>", Setting,
                                "<br><b>Morphology:</b>", Morph,
                                "<br><b>Length:</b>", round(Len_km, 1), "km"),
                 group = "Reaches") %>%
    # Task 6: nearest distance between large wood and bridges
    addPolylines(data = nearest_lines, color = "black",
                 weight = 1, opacity = 0.8,
                 popup = ~paste("<b>Large wood id:</b>", id,
                                "<br><b>Nearest bridge:</b>", HubName,
                                "<br><b>Distance:</b>", round(HubDist), "m"),
                 group = "Nearest distance") %>%
    # Task 9: large wood catchers (black outline under green line)
    addPolylines(data = catchers, color = "black",
                 weight = 11, opacity = 1, group = "LW catchers") %>%
    addPolylines(data = catchers, color = "limegreen",
                 weight = 7, opacity = 1,
                 label = ~paste("LW catcher", id),
                 popup = ~paste("<b>Large wood catcher:</b>", id,
                                "<br><b>Protects:</b>", Protects,
                                "<br><b>Rationale:</b>", Rationale,
                                "<br><b>Access:</b>", Access,
                                "<br><b>Span:</b>", round(Span_m), "m"),
                 group = "LW catchers") %>%
    # Legends (each one shown only when its layer is ticked)
    addLegend(position = "bottomleft", pal = pal_risk,
              values = factor(bridges$Risk, levels = c("High", "Medium", "Low")),
              title = "Bridge risk", opacity = 0.9, group = "Bridges") %>%
    addLegend(position = "bottomleft", colors = "limegreen",
              labels = "Proposed LW catcher", opacity = 1, group = "LW catchers") %>%
    addLegend(position = "bottomright", colors = c(cluster_swatch, "grey"),
              labels = c("In a DBSCAN cluster (one colour per cluster)", "Noise (not in a cluster)"),
              title = "Large wood", opacity = 0.9, group = "Large Wood") %>%
    addLegend(position = "bottomright", pal = pal_heatmap,
              values = range(values(heatmap), na.rm = TRUE),
              title = "LW density (KDE, r = 250 m)", opacity = 0.7, group = "Heatmap") %>%
    addLegend(position = "bottomright", pal = pal_slope, values = c(0, slope_q98),
              title = "Slope (degrees)", opacity = 0.6, group = "Slope") %>%
    addLegend(position = "bottomleft", pal = pal_morph, values = reaches$Morph,
              title = "Reach morphology", opacity = 0.9, group = "Reaches") %>%
    addLegend(position = "bottomleft", colors = "#6a3d9a",
              labels = "2 km upstream zone of a bridge", opacity = 0.5, group = "Upstream zones (2 km)") %>%
    addLegend(position = "bottomleft", colors = "#808080",
              labels = "Channel and floodplain (heatmap extent)", opacity = 0.8, group = "Analysis extent") %>%
    # Task 8: layer control, including slope, satellite basemap and extra layers
    addLayersControl(
      baseGroups = c("Colour", "Satellite"),
      overlayGroups = c("River", "Bridges", "Nearest distance", "Large Wood", "Heatmap", "Slope",
                        "LW catchers", "Reaches", "Upstream zones (2 km)", "Analysis extent"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    # Slope, nearest distance and extra layers switched off when the app opens
    hideGroup(c("Slope", "Nearest distance", "Reaches", "Upstream zones (2 km)", "Analysis extent"))
})

# DBSCAN clustering set by the user (section 2.1, advanced option) ----
# Re-runs DBSCAN whenever the sliders change (same method as practical code: dbscan(coords, eps, minPts))
lw_db <- reactive({
  cl <- dbscan(lw_xy, eps = input$eps, minPts = input$minpts)$cluster
  out <- clusters
  out$CLUSTER_ID <- ifelse(cl == 0, NA, cl)
  sizes <- table(cl[cl > 0])
  out$CLUSTER_SI <- ifelse(cl == 0, NA, as.integer(sizes[as.character(cl)]))
  # Draw order: noise underneath, then clustered wood as Single, Jam, Blockage
  out[order(!is.na(out$CLUSTER_ID), match(out$LW_Type, c("Single", "Jam", "Blockage"))), ]
})

# Summary of clusters for the current settings
output$cluster_summary <- renderUI({
  d <- lw_db()
  n <- length(unique(na.omit(d$CLUSTER_ID)))
  HTML(paste0("<b>", n, "</b> clusters; <b>", sum(is.na(d$CLUSTER_ID)), "</b> of ", nrow(d),
              " pieces not in a cluster"))
})

# Reset sliders to the values used in the report
observeEvent(input$reset_db, {
  updateSliderInput(session, "eps", value = 250)
  updateSliderInput(session, "minpts", value = 5)
})

# Add popups for large wood points
# Changed: popup fields match my attributes, clusters update with the sliders,
# clearGroup used so only large wood is redrawn
observe({
  d <- lw_db()
  ids <- sort(unique(na.omit(d$CLUSTER_ID)))
  if (length(ids) > 0) {
    pal_live <- colorFactor(palette = colorRampPalette(brewer.pal(12, "Paired"))(length(ids)),
                            domain = ids, na.color = "grey")
    fill_cols <- pal_live(d$CLUSTER_ID)
  } else {
    fill_cols <- rep("grey", nrow(d))
  }
  leafletProxy("map") %>%
    clearGroup("Large Wood") %>%
    # Clustered large wood full size, noise smaller and faded
    addCircleMarkers(data = d,
                     fillColor = fill_cols,
                     color = "black",
                     weight = ifelse(is.na(d$CLUSTER_ID), 0.5, 1),
                     radius = ifelse(is.na(d$CLUSTER_ID), 3.5, 5),
                     stroke = TRUE,
                     fillOpacity = ifelse(is.na(d$CLUSTER_ID), 0.5, 0.8),
                     popup = ~paste("<b>Type:</b>", LW_Type,
                                    "<br><b>Geomorphic unit:</b>", GeomUnit,
                                    "<br><b>Cluster ID:</b>", ifelse(is.na(CLUSTER_ID), "Noise (not in a cluster)", CLUSTER_ID),
                                    "<br><b>Points in cluster:</b>", ifelse(is.na(CLUSTER_SI), "-", CLUSTER_SI)),
                     group = "Large Wood")
})

# Bridges and catchers (drawn once)
observe({
  leafletProxy("map") %>%
    # Task 4 and 5: bridges with pop ups, as squares coloured by risk
    # Drawn Low, then Medium, then High so higher risk bridges sit on top
    addMarkers(data = bridges[bridges$Risk == "Low", ], icon = bridge_icons[["Low"]],
               label = ~Name, popup = ~popup, group = "Bridges",
               options = markerOptions(zIndexOffset = 1000)) %>%
    addMarkers(data = bridges[bridges$Risk == "Medium", ], icon = bridge_icons[["Medium"]],
               label = ~Name, popup = ~popup, group = "Bridges",
               options = markerOptions(zIndexOffset = 2000)) %>%
    addMarkers(data = bridges[bridges$Risk == "High", ], icon = bridge_icons[["High"]],
               label = ~Name, popup = ~popup, group = "Bridges",
               options = markerOptions(zIndexOffset = 3000)) %>%
    # Task 9: catcher symbols visible at all zoom levels (pin with funnel icon), on top of everything
    addAwesomeMarkers(data = catchers_pts,
                      icon = awesomeIcons(icon = "filter", library = "fa",
                                          markerColor = "green", iconColor = "white"),
                      label = ~paste("LW catcher", id),
                      popup = ~paste("<b>Large wood catcher:</b>", id,
                                     "<br><b>Protects:</b>", Protects,
                                     "<br><b>Rationale:</b>", Rationale,
                                     "<br><b>Access:</b>", Access,
                                     "<br><b>Span:</b>", round(Span_m), "m"),
                      options = markerOptions(zIndexOffset = 4000),
                      group = "LW catchers")
})

# Plots tab: render interactive charts ----
plot_options <- list(opts_hover(css = "stroke:black;stroke-width:1.5px;"),
                     opts_hover_inv(css = "opacity:0.35;"),
                     opts_tooltip(css = "background:white;color:black;padding:6px;border:1px solid grey;border-radius:4px;font-size:12px;"),
                     opts_toolbar(saveaspng = TRUE))

output$plot_chainage <- renderGirafe({
  girafe(ggobj = p_chainage, width_svg = 12, height_svg = 4.5, options = plot_options)
})
output$plot_bridges <- renderGirafe({
  girafe(ggobj = p_bridges, width_svg = 7, height_svg = 5.5, options = plot_options)
})
output$plot_geomunit <- renderGirafe({
  girafe(ggobj = p_geomunit, width_svg = 5, height_svg = 5.5, options = plot_options)
})