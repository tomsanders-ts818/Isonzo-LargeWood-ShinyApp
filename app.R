#################################################################
##          Practical 6 for GEOM184 - Open Source GIS          ##
##                         05/11/2026                          ##
##                     Creating a ShinyApp                     ##
##                            App.R                            ##
##        code by Diego Panici (d.panici@exeter.ac.uk)         ##
##          adapted by Tom Sanders for the River Isonzo        ##
#################################################################

# Load packages ----
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
options(shiny.maxRequestSize = 1000 * 1024^2)

# Run global script containing all your relevant data ----
source("Global.R")

# Define UI for visualisation ----
source("UI.R")
ui <- navbarPage("Instream large wood on the River Isonzo", id = 'nav',
                 tabPanel("Map",
                          div(class="outer",
                              leafletOutput("map", height = "calc(100vh - 70px)"),
                              # User-defined DBSCAN clustering (section 2.1, advanced option)
                              absolutePanel(top = 130, left = 25, width = 260, draggable = TRUE,
                                            style = "background: rgba(255,255,255,0.92); padding: 10px 14px;
                                                     border-radius: 6px; box-shadow: 0 1px 5px rgba(0,0,0,0.4);",
                                            h5(strong("DBSCAN clustering of large wood")),
                                            sliderInput("eps", "Search radius, eps (m)",
                                                        min = 50, max = 1000, value = 250, step = 25),
                                            sliderInput("minpts", "Minimum points per cluster",
                                                        min = 2, max = 15, value = 5, step = 1),
                                            htmlOutput("cluster_summary"),
                                            actionButton("reset_db", "Reset to report values (250 m, 5)",
                                                         style = "margin-top: 8px; font-size: 12px;")
                              )
                          )
                 ),
                 # Plots tab with interactive charts of large wood distribution
                 tabPanel("Plots",
                          fluidPage(
                            girafeOutput("plot_chainage"),
                            fluidRow(
                              column(7, girafeOutput("plot_bridges")),
                              column(5, girafeOutput("plot_geomunit"))
                            ),
                            p("Hover over bars and lines for values. Large wood digitised from satellite imagery;
                              distances measured downstream along the river centreline.")
                          )
                 )
)

# Define the server that performs all necessary operations ----
server <- function(input, output, session){
  source("Server.R", local = TRUE)
}

# Run the application ----
shinyApp(ui, server)