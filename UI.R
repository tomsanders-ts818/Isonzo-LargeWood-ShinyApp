#################################################################
##          Practical 6 for GEOM184 - Open Source GIS          ##
##                         05/11/2026                          ##
##                     Creating a ShinyApp                     ##
##                            UI.R                             ##
##        code by Diego Panici (d.panici@exeter.ac.uk)         ##
##          adapted by Tom Sanders for the River Isonzo        ##
#################################################################

div(class="outer",
    leafletOutput("map", height = "calc(100vh - 70px)")
)