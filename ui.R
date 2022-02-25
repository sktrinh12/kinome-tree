#---------------------- UI SIDE ----------------------#

source("coralR/writekinasetree.R")
source("coralR/colorby.R")
source("lib/Rename.R")
source("global.R")
source("db.R")

ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(
        title = "Kinome Tree"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data", tabName = "data", icon = icon("table")),
            menuItem("Chart", tabName = "chart", icon = icon("chart-bar"))
        )
    ),
    dashboardBody(
			  tags$head(
						tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
				),
        bsAlert("alert"),
        tabItems(
            tabItem("data",
                    fluidRow(
                        box(
                            width = 8,
                            fileInput("kinasefile", "Choose Excel File", multiple = FALSE, accept = c(".xlsx")),
                            p("Cleaned File: removes Kinase values with a bracket ( [ ), or hyphen (-) but will split text on forward slash ( / ) delimiter. The CRO kinase symbols are converted to HGNC symbols. Each result is tagged with bin values for plotting.")
                        ),
                        box(
                            width = 4,
                            numericInput(inputId = "cutoff", label = "Remove Values Less Than", 50),
                            textInput(inputId = "kinasefilter", label = "Remove Value from Just_Kinase Column", value = "Cascade")
                        ),
											  box(width = 8,
												     selectizeInput(inputId = 'search_cmpd_ids',
																	label = 'Search by Compound ID',
																	choices = NULL,
																	selected = NULL,
																	multiple = FALSE,
																	options = list(create = FALSE) # if TRUE, allows newly created inputs
																  ),
														hr(),
														uiOutput("exp_id_output"),
														uiOutput("tech_output"),
														actionButton("submit", "Submit")
												)
                    ),
                    tabBox(
                        id = "filetabs",
                        width = 12,
                        selected = "Original Data",
                        tabPanel("Original Data", 
                            withSpinner(dataTableOutput("kinasetable"), type = 2),
                            downloadButton("downloadInputData", "Download")
                        ),
                        tabPanel("Cleaned Data", 
                            dataTableOutput("cleanedtable"),
                            downloadButton("downloadCleanedData", "Download")
                        ),
                        tabPanel("Filtered Data", 
                                 dataTableOutput("cleanedfilteredtable"),
                                 downloadButton("downloadCleanedFilteredData", "Download")
                        )
                    )
            ),
            tabItem("chart",
                box(
                    width = 10,
                    div(id="treediv")
                ),
                box(
                  width = 2,
                  selectInput(inputId = "plotresultcolumn", label = "Result Column for Tree", choices = c("Result", "bin_10", "bin_25"), selected = "bin_25", multiple = FALSE),
                  textInput(inputId = "charttitle", label = "Chart Title"),
                  numericInput(inputId = "titlefontsize", label = "Title Font Size (px)", value = 18),
                  numericInput(inputId = "titley", label = "Title Vertical Position", value = 0),
                  numericInput(inputId = "nodesizemin", label = "Node Size Min", value = 5),
                  numericInput(inputId = "nodesizemax", label = "Node Size Max", value = 30),
                  numericInput(inputId = "nodelabelfontsize", label = "Node Label Font Size", value = 7),
                  colourInput('nodelabelcolor', 'Node Label Colour', "#1A1818", showColour = "background"),
                  numericInput(inputId = "nodeopacity", label = "Node Opacity (default 50%)", value = 50, min = 0, max = 100, step = 5)
                ),
								box(
								  width = 2,
                  selectInput(inputId = "node_colours_tgt", label = "Target Nodes", choices = c(), selected = NULL, multiple = TRUE),
								  colourInput('node_tgt_cpick', '', cpalette[1], showColour = "background"),
								  hr(),
                  # selectInput(inputId = "node_colours_offtgt", label = "Off-Target Nodes", choices = c(), selected = NULL, multiple = TRUE),
                  h5(strong("Off-Target Nodes")),
								  colourInput('node_offtgt_cpick', '', cpalette[2], showColour = "background")
								  # hr(),
                  # h5(strong("Neutral Nodes")),
								  # colourInput('node_net_cpick', '', cpalette[3], showColour = "background")
								),
                box(
                    width = 12,
                    downloadButton(outputId = "downloadtree",label= "Download")
                )
            )
        )
    )
)
