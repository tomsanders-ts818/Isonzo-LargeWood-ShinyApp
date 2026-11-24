#################################################################
##          Practical 6 for GEOM184 - Open Source GIS          ##
##                         05/11/2026                          ##
##                     Creating a ShinyApp                     ##
##                          Global.R                           ##
##        code by Diego Panici (d.panici@exeter.ac.uk)         ##
##          adapted by Tom Sanders for the River Isonzo        ##
#################################################################

# Load packages (also loaded here because Shiny runs Global.R before app.R on the server)
library(shiny)
library(leaflet)
library(sf)
library(raster)
library(ggplot2)
library(ggiraph)
library(RColorBrewer)
library(terra)
library(leafem)
library(dbscan)

# G1 Load large wood, river, and bridge data ----
# Load my files from the data folder
lw_points <- st_read("data/LW_Isonzo.shp")
river <- st_read("data/RiverIsonzo_32633.shp")
bridges <- st_read("data/BridgesIsonzo_32633.shp")

#Convert vectors to CRS 4326
lw_points <- st_transform(lw_points, crs = 4326)
river <- st_transform(river, crs = 4326)
bridges <- st_transform(bridges, crs = 4326)

# Load clusters (DBSCAN, eps = 250 m, minPts = 5, from QGIS practical 2)
clusters <- st_read("data/LW_dbscan_250m_min5.shp")

# Task 3: re-project the cluster vector
clusters <- st_transform(clusters, crs = 4326)
# Draw order: noise underneath, then clustered wood as Single, Jam, Blockage (most significant on top)
clusters <- clusters[order(!is.na(clusters$CLUSTER_ID),
                           match(clusters$LW_Type, c("Single", "Jam", "Blockage"))), ]

# Dynamically generate colours based on number of unique clusters
# Changed: noise points (no cluster) excluded from the count and shown in grey
num_clusters <- length(unique(na.omit(clusters$CLUSTER_ID)))
pal_clusters <- colorFactor(palette = colorRampPalette(brewer.pal(12, "Paired"))(num_clusters),
                            domain = na.omit(clusters$CLUSTER_ID),
                            na.color = "grey")

# Legend swatch showing all cluster colours (one colour per cluster)
cluster_cols <- colorRampPalette(brewer.pal(12, "Paired"))(num_clusters)
cluster_swatch <- paste0("linear-gradient(to right, ", paste(cluster_cols, collapse = ", "), ")")

# Task 5: add upstream large wood and terrain metrics (practical 3) to bridges
bridges_up <- st_read("data/P3_bridges_upstream.gpkg")
bridges$LW_upstream <- bridges_up$LW_upstream[match(bridges$id, bridges_up$id)]
bridges$tree_ratio <- bridges_up$tree_ratio[match(bridges$id, bridges_up$id)]
bridges$mean_slope <- bridges_up$mean_slope[match(bridges$id, bridges_up$id)]

# Risk colours (same on map and plots): High red, Medium orange, Low yellow
risk_cols <- c("High" = "#d7191c", "Medium" = "#fd8d3c", "Low" = "#ffd92f")
pal_risk <- colorFactor(palette = unname(risk_cols), levels = names(risk_cols))

# Bridge symbols: squares coloured by risk (different shape from large wood circles)
bridge_svg <- function(col) {
  svg <- paste0("<svg xmlns='http://www.w3.org/2000/svg' width='18' height='18'>",
                "<rect x='1.5' y='1.5' width='15' height='15' fill='", col,
                "' stroke='black' stroke-width='2'/></svg>")
  paste0("data:image/svg+xml;charset=utf-8,", URLencode(svg, reserved = TRUE))
}

# One square icon per risk level, used to draw High risk bridges on top
bridge_icons <- lapply(risk_cols, function(col) makeIcon(iconUrl = bridge_svg(col),
                                                         iconWidth = 18, iconHeight = 18,
                                                         iconAnchorX = 9, iconAnchorY = 9))

# Bridge pop up text
bridges$popup <- paste("<b>Bridge:</b>", bridges$Name,
                       "<br><b>Risk:</b>", bridges$Risk, paste0("(score ", bridges$Risk_score, ")"),
                       "<br><b>Blockages recorded:</b>", bridges$Block_n,
                       "<br><b>LW within 1 km upstream:</b>", bridges$Up1000,
                       "<br><b>Jams within 1 km upstream:</b>", bridges$JamUp1000,
                       "<br><b>LW in 2 km upstream zone:</b>", bridges$LW_upstream,
                       "<br><b>Upstream tree cover:</b>", round(bridges$tree_ratio * 100, 1), "%",
                       "<br><b>Upstream mean slope:</b>", round(bridges$mean_slope, 1), "degrees")

# Task 6: load nearest distance lines (large wood to nearest bridge, QGIS practical 2)
nearest_lines <- st_read("data/LW_to_nearest_bridge.shp")
nearest_lines <- st_transform(nearest_lines, crs = 4326)

# Load heatmap (KDE, radius 250 m, clipped to channel and floodplain, QGIS practical 2)
# Changed: pre-projected to web map projection (EPSG:3857, ~10 m) with values below 1 removed,
# so the online server does not run out of memory
heatmap <- rast("data/heatmap_3857.tif")
# Changed: YlOrRd so the highest density is the darkest colour on a light basemap
pal_heatmap <- colorNumeric(palette = "YlOrRd", domain = na.omit(values(heatmap)),
                            na.color = "transparent")

# Task 7: load slope (10 m, from DTM in practical 3)
# Changed: pre-projected to web map projection (EPSG:3857, ~10 m) to reduce memory use online
slope <- rast("data/slope_3857.tif")
# Colour scale capped at 98th percentile so the flat valley floor shows detail
slope_q98 <- quantile(values(slope), 0.98, na.rm = TRUE)
slope <- clamp(slope, upper = slope_q98, values = TRUE)
pal_slope <- colorNumeric(palette = "viridis", domain = c(0, slope_q98),
                          na.color = "transparent")

# Task 9: load large wood catchers (retention structures)
catchers <- st_read("data/LW_retention_structures.shp")
# Catcher number (id field not filled in QGIS)
catchers$id <- seq_len(nrow(catchers))
# Catcher midpoints, used as symbols visible at all zoom levels
catchers_pts <- st_centroid(catchers)
catchers <- st_transform(catchers, crs = 4326)
catchers_pts <- st_transform(catchers_pts, crs = 4326)

# Extra layers: river reaches (setting and morphology)
reaches <- st_read("data/Reaches_Isonzo.shp")
reaches <- st_transform(reaches, crs = 4326)
# Clearer label for single-thread reaches in legend and pop ups
reaches$Morph[reaches$Morph == "SingleThr"] <- "Single-thread"
pal_morph <- colorFactor(palette = c("#d95f02", "#1b9e77"), domain = reaches$Morph)

# Extra layers: analysis extent (channel and floodplain used to clip the heatmap)
extent_buffer <- st_read("data/River_buffer_reach_based_v2.shp")
extent_buffer <- st_transform(extent_buffer, crs = 4326)

# Extra layers: 2 km upstream zones of each bridge (practical 3)
upstream_zones <- st_read("data/P3_upstream_zones.gpkg")
upstream_zones <- st_transform(upstream_zones, crs = 4326)
# Draw order: zones of higher risk bridges on top where zones overlap
zone_risk <- bridges$Risk[match(upstream_zones$Name, bridges$Name)]
upstream_zones <- upstream_zones[order(match(zone_risk, c("Low", "Medium", "High"))), ]

# Plots tab: data for interactive charts ----
# Chainage of catchers (m downstream from upstream end, same as Chain_m for LW and bridges)
river_m <- st_transform(river, crs = 32633)
catchers_pts$Chain_m <- st_line_project(st_geometry(river_m),
                                        st_geometry(st_transform(catchers_pts, crs = 32633)))

# Colours used in all plots
type_cols <- c("Single" = "#9ecae1", "Jam" = "#3182bd", "Blockage" = "#08306b")
marker_cols <- c("High risk bridge" = "#d7191c", "Medium risk bridge" = "#fd8d3c",
                 "Low risk bridge" = "#ffd92f", "LW catcher" = "limegreen")

# Plot 1: large wood along the river (per 1 km, by type), with bridges and catchers
lw_df <- st_drop_geometry(lw_points)
lw_df$LW_Type <- factor(lw_df$LW_Type, levels = c("Single", "Jam", "Blockage"))
lw_df$km <- floor(lw_df$Chain_m / 1000)
lw_km <- as.data.frame(table(km = lw_df$km, LW_Type = lw_df$LW_Type))
lw_km$km <- as.numeric(as.character(lw_km$km))
lw_km <- lw_km[lw_km$Freq > 0, ]

markers <- rbind(
  data.frame(Name = bridges$Name, Chain_m = bridges$Chain_m,
             Marker = paste(bridges$Risk, "risk bridge")),
  data.frame(Name = paste("LW catcher", catchers_pts$id), Chain_m = as.numeric(catchers_pts$Chain_m),
             Marker = "LW catcher"))
markers$Marker <- factor(markers$Marker, levels = names(marker_cols))

p_chainage <- ggplot() +
  geom_col_interactive(data = lw_km,
                       aes(x = km + 0.5, y = Freq, fill = LW_Type,
                           tooltip = paste0(km, "-", km + 1, " km: ", Freq, " ", LW_Type),
                           data_id = km),
                       width = 1, colour = "white", linewidth = 0.2) +
  # Bridges as thin dashed lines, catchers as thicker solid lines
  geom_vline_interactive(data = markers[markers$Marker != "LW catcher", ],
                         aes(xintercept = Chain_m / 1000, colour = Marker,
                             tooltip = paste0(Name, " (", round(Chain_m / 1000, 1), " km)"),
                             data_id = Name),
                         linewidth = 0.6, linetype = "dashed") +
  geom_vline_interactive(data = markers[markers$Marker == "LW catcher", ],
                         aes(xintercept = Chain_m / 1000, colour = Marker,
                             tooltip = paste0(Name, " (", round(Chain_m / 1000, 1), " km)"),
                             data_id = Name),
                         linewidth = 1.4, linetype = "solid") +
  scale_fill_manual(values = type_cols, name = "Large wood type") +
  scale_colour_manual(values = marker_cols, name = NULL, drop = FALSE) +
  guides(colour = guide_legend(override.aes = list(linetype = c("dashed", "dashed", "dashed", "solid"),
                                                   linewidth = c(0.6, 0.6, 0.6, 1.4)))) +
  scale_x_continuous(breaks = seq(0, 45, 5), expand = c(0.01, 0)) +
  labs(x = "Distance downstream from upstream end of reach (km)", y = "Large wood pieces per km",
       title = "Large wood along the River Isonzo") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Plot 2: large wood upstream of each bridge, coloured by risk
br_df <- st_drop_geometry(bridges)
br_df$Risk <- factor(br_df$Risk, levels = c("High", "Medium", "Low"))
br_df$Name <- factor(br_df$Name, levels = br_df$Name[order(br_df$Chain_m, decreasing = TRUE)])

p_bridges <- ggplot(br_df) +
  geom_col_interactive(aes(x = Name, y = Up1000, fill = Risk,
                           tooltip = paste0("<b>", Name, "</b>",
                                            "<br>Risk: ", Risk, " (score ", Risk_score, ")",
                                            "<br>LW within 1 km upstream: ", Up1000,
                                            "<br>Jams within 1 km upstream: ", JamUp1000,
                                            "<br>LW in 2 km upstream zone: ", LW_upstream,
                                            "<br>Blockages recorded: ", Block_n),
                           data_id = Name),
                       colour = "grey30", linewidth = 0.2) +
  coord_flip() +
  scale_fill_manual(values = risk_cols, name = "Risk") +
  labs(x = NULL, y = "Large wood within 1 km upstream",
       title = "Large wood upstream of each bridge",
       subtitle = "Bridges listed from upstream (top) to downstream") +
  theme_minimal(base_size = 12)

# Plot 3: large wood type by geomorphic unit
gu_df <- as.data.frame(table(GeomUnit = lw_df$GeomUnit, LW_Type = lw_df$LW_Type))
gu_levels <- c("Channel", "Bank", "Bar", "Island")
gu_df$GeomUnit <- factor(gu_df$GeomUnit,
                         levels = c(intersect(gu_levels, unique(lw_df$GeomUnit)),
                                    setdiff(unique(lw_df$GeomUnit), gu_levels)))
gu_df <- gu_df[gu_df$Freq > 0, ]

p_geomunit <- ggplot(gu_df) +
  geom_col_interactive(aes(x = GeomUnit, y = Freq, fill = LW_Type,
                           tooltip = paste0(Freq, " ", LW_Type, " on ", GeomUnit),
                           data_id = paste(GeomUnit, LW_Type))) +
  scale_fill_manual(values = type_cols, name = "Large wood type") +
  labs(x = "Geomorphic unit", y = "Large wood pieces",
       title = "Where large wood sits in the channel") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")


# DBSCAN sliders: large wood coordinates in metres (EPSG:32633) so eps is in metres
lw_xy <- st_coordinates(st_transform(clusters, crs = 32633))

# River extent used to set the opening view of the map
river_bb <- st_bbox(river)