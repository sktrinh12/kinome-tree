# load basic libraries
library(shiny)
library(shinydashboard)
library(shinyBS)
library(readr)
library(rsvg)
library(shinyWidgets)
library(RColorBrewer)
library(shinycssloaders)

# load Manning-related libraries
library(svgPanZoom)

# load ui-related libraries
library(colourpicker)
library(DT)

# load other network libraries
library(data.table)

#---------------------- SOURCE R FILES ----------------------#

source("ui.R")
source("server.R")

# Run the application 
shinyApp(ui = ui, server = server)
