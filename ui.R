#---------------------- UI SIDE ----------------------#
# load basic libraries
library(shiny)
library(shinydashboard)
library(shinyBS)
library(readr)
library(rsvg)
library(shinyWidgets)
library(shinycssloaders)

# load Manning-related libraries
library(svgPanZoom)

# load ui-related libraries
library(colourpicker)
library(DT)

source("lib/Rename.R")
source("global.R")
source("db.R")
source("coralR/writekinasetree.R")
source("coralR/colorby.R")

ui <- dashboardPage(skin = "blue", dashboardHeader(title = "Kinome Tree"), dashboardSidebar(sidebarMenu(menuItem("Data",
    tabName = "data", icon = icon("table")), menuItem("Chart", tabName = "chart",
    icon = icon("chart-bar")))), dashboardBody(tags$head(tags$script(src="js.js"), tags$link(rel = "stylesheet",
    type = "text/css", href = "styles.css")), bsAlert("alert"), tabItems(tabItem("data",
    fluidRow(box(width = 8, fileInput("kinasefile", "Choose Excel File", multiple = FALSE,
        accept = c(".xlsx")), p("Cleaned File: removes Kinase values with a bracket ( [ ), or hyphen (-) but will split text on forward slash ( / ) delimiter. The CRO kinase symbols are converted to HGNC symbols. Each result is tagged with bin values for plotting.")),
        box(width = 4, numericInput(inputId = "cutoff", label = "Remove Values Less Than",
            50), textInput(inputId = "kinasefilter", label = "Remove Value from Just_Kinase Column",
            value = "Cascade"))), fluidRow(box(width = 8, selectizeInput(inputId = "search_cmpd_ids",
        label = "Search by Compound ID", choices = NULL, selected = NULL, multiple = FALSE,
        options = list(create = FALSE)  # if TRUE, allows newly created inputs
),
        hr(), uiOutput("exp_id_output"), uiOutput("tech_output"), actionButton("submit",
            "Submit"))), tabBox(id = "filetabs", width = 12, selected = "Original Data",
        tabPanel("Original Data", withSpinner(dataTableOutput("kinasetable"), type = 2),
            downloadButton("downloadInputData", "Download")), tabPanel("Cleaned Data",
            withSpinner(dataTableOutput("cleanedtable"), type = 2), downloadButton("downloadCleanedData",
                "Download")), tabPanel("Filtered Data", withSpinner(dataTableOutput("cleanedfilteredtable"),
            type = 2), downloadButton("downloadCleanedFilteredData", "Download")))),
    tabItem("chart", tabBox(id = "charttabs", width = 10, selected = "Kinome Tree",
        tabPanel("Kinome Tree", div(id = "treediv"), div(id="tooltip")), tabPanel("Mutants", h4(strong("Visualise mutant kinases")),
            actionButton("click_polar", "PolarPlot"), withSpinner(plotOutput("polarplot"),
                type = 2), downloadButton("downloadpolarplot", "Download Plot"),
            hr(), actionButton("click_lolli", "LolliPlot"), withSpinner(plotOutput("lolliplot"),
                type = 2), downloadButton("downloadlolliplot", "Download Plot"))),
        box(width = 2, selectInput(inputId = "plotresultcolumn", label = "Result Column for Tree",
            choices = c("ratio", "bin_10", "bin_25"), selected = "bin_25", multiple = FALSE),
            textInput(inputId = "charttitle", label = "Chart Title"), numericInput(inputId = "titlefontsize",
                label = "Title Font Size (px)", value = 18), numericInput(inputId = "titley",
                label = "Title Vertical Position", value = 0), numericInput(inputId = "nodesizemin",
                label = "Node Size Min", value = 5), numericInput(inputId = "nodesizemax",
                label = "Node Size Max", value = 30), numericInput(inputId = "nodelabelfontsize",
                label = "Node Label Font Size", value = 7), numericInput(inputId = "nodeopacity",
                label = "Node Opacity (default 50%)", value = 50, min = 0, max = 100,
                step = 5)), box(width = 2, selectInput(inputId = "node_colours_tgt",
            label = "Target Nodes", choices = c(), selected = NULL, multiple = TRUE),
            colourInput("node_tgt_cpick", "", cpalette[1], showColour = "background"),
            colourInput("text_tgt_cpick", "Text-label", "#000000", showColour = "background"),
            hr(), h5(strong("Off-Target Nodes")), colourInput("node_offtgt_cpick",
                "", cpalette[2], showColour = "background"), colourInput("text_offtgt_cpick",
                "Text-label", "#000000", showColour = "background")), box(width = 12,
            downloadButton(outputId = "downloadtree", label = "Download Tree"))))))
