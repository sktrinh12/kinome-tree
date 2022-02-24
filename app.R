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
library(data.tree)


library(data.table)


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#---------------------- SOURCE R FILES ----------------------#

source("coralR/colorby.R")
source("coralR/writekinasetree.R")
source("coralR/colors.R")
source("lib/Rename.R")
source("db.R")

#---------------------- READ IN AND ORGANIZE DATA ----------------------#
# options for spinner (loading)
options(spinner.color="#0275D8", 
				spinner.color.background="#ffffff",
				spinner.size=3)

# read RDS
orig_svginfo = readRDS("Data/kintree.RDS")

# remove NAs from subfamilies
NAs = which(is.na(orig_svginfo$dataframe$kinase.subfamily))
orig_svginfo$dataframe$kinase.subfamily[NAs] = ""

# remove NAs from HGNCs
NAs = which(is.na(orig_svginfo$dataframe$id.HGNC))
orig_svginfo$dataframe$id.HGNC[NAs] = ""

# add correct header
orig_svginfo$header = "<svg width=\"940\" height=\"940\"\n
xmlns=\"http://www.w3.org/2000/svg\"\n
xmlns:xlink=\"http://www.w3.org/1999/xlink\" >\n"

# intitialize title
orig_svginfo$title = ""

# initilize legend
orig_svginfo$legend = c()

# add node opacity
orig_svginfo$dataframe$node.opacity = 0.5 #1

# add node order
orig_svginfo$dataframe$node.selected = -1

# make svginfo (leaving the original intact)
svginfo = orig_svginfo

# assign node and branch orders
svginfo$dataframe$nodeorder = 1:nrow(svginfo$dataframe)
svginfo$dataframe$branchorder = 1:nrow(svginfo$dataframe)


#---------------------- DEFAULT COLORS ----------------------#

# Default tree branch color
BG_col1 = "#D3D3D3"


cpalette <- brewer.pal(9, "Set1")
other_case_stmt <- glue::glue('TRUE ~ "{BG_col1}"') # to colour node grey

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

reactive_data <- reactiveValues()

HGNC <- data.table(read_excel("data-input/HGNC-protein-coding-genes.xlsx")) %>% 
				mutate(SYMBOL_UPPER = stringr::str_to_upper(SYMBOL))

manual_map <- data.table(read_excel("data-input/manual-map.xlsx"))

 
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
								  span(colourInput('node_tgt_cpick', '', cpalette[1], showColour = "background"), style = "margin:1px; padding:1px;"),
								  hr(),
                  selectInput(inputId = "node_colours_offtgt", label = "Off-Target Nodes", choices = c(), selected = NULL, multiple = TRUE),
								  span(colourInput('node_offtgt_cpick', '', cpalette[2], showColour = "background"), style = "margin:1px; padding:1px;"),
								  hr(),
                  selectInput(inputId = "node_colours_neutral", label = "Neutral Nodes", choices = c(), selected = NULL, multiple = TRUE),
								  span(colourInput('node_net_cpick', '', cpalette[3], showColour = "background"), style = "margin:1px; padding:1px;")
								),
                box(
                    width = 12,
                    downloadButton(outputId = "downloadtree",label= "Download")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    kinaseData <- reactive({
        req(input$kinasefile)
        tryCatch(
            { dt <- data.table(readxl::read_xlsx(input$kinasefile$datapath))
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        return(dt)
    })
    
    kinaseDataCleaned <- reactive({
        req(isTruthy(input$kinasefile) || isTruthy(input$submit))
				if (isTruthy(input$kinasefile)) {
								dt <- clean_kinase_data(HGNC, 
																				kinaseData(), 
																				manual_map
																				)
				}
				if (isTruthy(input$submit)) {
								req(input$search_cmpd_ids, input$exp_id, input$tech_id)
								dt <- reactive_data$df_c
				}	
        return(dt)
    })
    
    kinaseDataCleanedFiltered <- reactive({
        req(isTruthy(input$kinasefile) || isTruthy(input$submit))
				if (isTruthy(input$kinasefile)) {
								dt <- clean_kinase_data(HGNC, 
																				kinaseData(), 
																				manual_map, 
																				input$cutoff, 
																				input$kinasefilter
																				)
				}
				if (isTruthy(input$submit)) {
								req(input$search_cmpd_ids, input$exp_id, input$tech_id)
								dt <- reactive_data$df_f
				}	

				return(dt)
    })
    
    output$kinasetable <- renderDataTable({
        datatable(kinaseData(), options = list(pageLength = 25))
    })
    
    output$cleanedtable <- renderDataTable({
        datatable(kinaseDataCleaned(), options = list(pageLength = 25))
    })
    
    output$cleanedfilteredtable <- renderDataTable({
        datatable(kinaseDataCleanedFiltered(), options = list(pageLength = 25))
    })
    
    output$downloadInputData <- downloadHandler(
        filename = function() {
            paste(input$kinasefile$name, format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(kinaseData(), file, row.names = FALSE)
        }
    )
    
    output$downloadCleanedData <- downloadHandler(
        filename = function() {
            paste(input$kinasefile$name, "-cleaned-", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(kinaseDataCleaned(), file, row.names = FALSE)
        }
    )
    
    output$downloadCleanedFilteredData <- downloadHandler(
        filename = function() {
            paste(input$kinasefile$name, "-filtered-", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(kinaseDataCleanedFiltered(), file, row.names = FALSE)
        }
    )
    
    svgoutfile      = tempfile(pattern="kintreeout",tmpdir="tempfiles",fileext = ".svg") # session specific tree svg file
    
    insertUI(selector = "#treediv",where = "afterEnd",ui = source("coralR/renderTree.R",local=TRUE)$value)
    
    newdf <- reactive({ 
        
        # get current values
        tempdf = svginfo$dataframe
        
        # set font family
        tempdf$text.font = paste("'","Arial","'",sep="")
        
				kdata = kinaseDataCleanedFiltered()
				print(paste('KINASE DATA CLEANED & FILTERED', paste0(rep('-',20), collapse="")))
				print(head(kdata))
				print(input$plotresultcolumn)        

				sel_colm <- input$plotresultcolumn # either bin10, bin25 or result (drop down menu)
				resizedf <- tempdf %>% 
												select(id.coral, id.HGNC) %>%
												merge(kdata, by.x = "id.HGNC", by.y = "HGNC_SYMBOL") %>%
												mutate(KINASE = id.coral, HGNC_SYMBOL = id.HGNC) %>%
												arrange(HGNC_SYMBOL)

				print(paste('RESIZEDF', paste0(rep('-',20), collapse="")))
				print(resizedf)

				if (nrow(resizedf) == 0) {
								createAlert(session, "alert", "resizedf_qcheck", title = "Error",
									 content = "Cannot map to kinome tree; there was no HGNC match for those kinases", append = FALSE)
				} else {
								closeAlert(session, "resizedf_qcheck")
								print(paste0(rep('-',20), collapse=""))


								# ------------------ NODE SIZE & COLOUR ------------------ #

								# set colors based on selected ids
								if (nrow(resizedf)>0)
								{
								radii_and_mapping = resizes.by.value(
										df = tempdf, 
										resizedf = resizedf, 
										sizerange = c(input$nodesizemin, input$nodesizemax),  #c(1, 60) 
										controlledrange = FALSE, 
										minvalue = 0, 
										maxvalue = 100, 
										showall = "hide",
										sel_colm
								)
								}
								
								# ------------------ ADVANCED OPTIONS ------------------ #
								
								# Change Color and Size of Font for Selected kinases
								tempdf <- tempdf %>% mutate(node.selected = ifelse(id.coral %in% resizedf$id.coral, 1, -1),
														node.radius = radii_and_mapping[[1]],
														node.val.radius = radii_and_mapping[[2]],
														text.col = ifelse(node.selected == 1, input$nodelabelcolor, BG_col1), # set selected font color and size
														text.size = ifelse(node.selected == 1, input$nodelabelfontsize, 0), 
														node.opacity = input$nodeopacity/100,
														branch.col = BG_col1
														) 

								# temp df kinase group for legend
								tdf_kgrp <- tempdf %>%
															filter(id.HGNC %in% resizedf$HGNC_SYMBOL) %>%
															select(id.coral, id.HGNC, kinase.group, node.radius) %>%
															arrange(node.radius)

								if (length(tdf_kgrp) == 0) {
												stop("Empty dataframe")
								}

								reactive_data$kinase_groups <- unique(tdf_kgrp$kinase.group)
                nbr_kgroups <- length(reactive_data$kinase_groups)

								# for renderUI colourInput
								reactive_data$nbr_nodes <- nbr_kgroups

								colours <- c(input$node_tgt_cpick,
														input$node_offtgt_cpick,
														input$node_net_cpick
													  )
																
								if (all(sapply(colours, function(x) {!isTruthy(x)}))) {
												# print('empty input for kinase symbol colour map list')
												colours <- cpalette[seq(3)]
								}

								hgnc_node_selected <- list(input$node_colours_tgt, input$node_colours_offtgt)
								
								# default nodes to select
								if (all(sapply(seq(2), function(x) {!isTruthy(hgnc_node_selected[[x]])}))) {
												# print('not selected yet')
												hgnc_node_selected <- list(resizedf$HGNC_SYMBOL[1], resizedf$HGNC_SYMBOL[seq(2, nrow(resizedf))])
								}

								# print(paste('hgnc_node_selected',paste0(rep('-',20), collapse="")))
								# print(hgnc_node_selected)
								
                # set node colours
                tempdf <- tempdf %>%
                            mutate(node.col = case_when(
															id.HGNC %in%  hgnc_node_selected[[1]] ~ colours[1],
												      id.HGNC %in%  hgnc_node_selected[[2]] ~ colours[2],
															TRUE ~ colours[3]
												      )
													  )


								# combine the input types (result, bins) with the node.radius and misc columns
								# node size dataframe for reference legend

								tdf_node_size <- tempdf %>%
																				filter(id.coral %in% resizedf$id.coral) %>%
																				merge(resizedf, by = c("id.coral", "id.HGNC")) %>%
																				mutate(!!sel_colm := as.integer(!!as.name(sel_colm))) %>%
																				arrange(!!as.name(sel_colm)) %>%
																				distinct(!!as.name(sel_colm), .keep_all = T) %>%
																				mutate(row_index = row_number(),
																				      cumsum_radius = cumsum(2*node.radius + buffer),
																							y_pos = ifelse(row_index == 1, y + node.radius, lag(cumsum_radius) + node.radius + y),
																						  cy_pos = y_pos,
																						  x_pos = 112.75,
																						  cx_pos = 117.5,
																						  radius = node.radius,
																						  label = !!as.name(sel_colm),
																						  colours = BG_col1,
																						  font_size = 9) %>%
																				select(-branch.coords)
				
								print(paste('TDF_NODE_SIZE', paste0(rep('-',20), collapse="")))
								print(tdf_node_size)

								return(list(tempdf, tdf_node_size))

								} # end if-else stmt
						}) # end reactive
						
						
						# build the manning tree
						output$plot1  <- renderSvgPanZoom ({
								
								# recolor the official matrix
								df_data = newdf()
								if (length(df_data) == 0) {
												stop("Error")
								}
								svginfo$dataframe = df_data[[1]]
								svginfo$node_size_legend = df_data[[2]]

								# set title
								svginfo$title = input$charttitle
								
								if (! dir.exists(dirname(svgoutfile))) {
										dir.create(dirname(svgoutfile),showWarnings = F);  
								}
								
								# Write SVG file
								writekinasetree(svginfo, 
																destination = svgoutfile, 
																font="Arial", 
																labelselect = "HGNC", 
																groupcolor = "#000000", 
																titley = input$titley, 
																titlefontsize = input$titlefontsize
																)
								
								# Render SVG
								svgPanZoom(svgoutfile, viewBox = F, controlIconsEnabled=F)
    })
    
    output$downloadtree <- downloadHandler(
        
        filename <- function(file) { paste("CORAL",".","tree",".","svg",sep="")},
        content <- function(file) {
            file.copy(svgoutfile, file)
        }
    )
    
    # ----------------- FILTER BY COMPONUD ID -----------------

  # Return the unfiltered kinome dataset (all exp ids)
  get_unfil_kdata <- reactive({
    req(input$search_cmpd_ids)
    fetch_uf_kdata(input$search_cmpd_ids)
  })

  # Return the sub-final kinome dataset (single exp id)
  get_subfil_kdata <- reactive({
    req(input$search_cmpd_ids, input$exp_id)
    fetch_f_kdata(input$search_cmpd_ids,
                  input$exp_id 
                 )
  })

  # show the final df from compd_id and experiment id after clicking Submit
  observeEvent(input$submit, {
    req(input$search_cmpd_ids, input$exp_id, input$tech_id)
    # Return the final kinome dataset (single exp id + tech)
    reactive_data$df_kd <- fetch_f_kdata(input$search_cmpd_ids,
                        input$exp_id, 
                        input$tech_id
                       )
    output$kinasetable <- renderDataTable(reactive_data$df_kd, 
                                  options = list(pageLength = 25)
													)

    # cleaned data
    reactive_data$df_c <- clean_kinase_data(HGNC, reactive_data$df_kd, manual_map)

    # cleaned & filtered data
    reactive_data$df_f <- clean_kinase_data(HGNC, reactive_data$df_kd, 
																manual_map, 
																input$cutoff, 
																input$kinasefilter
															)

    # update the node selection for target nodes 
	  updateSelectInput(session, 
											"node_colours_tgt",
											choices = reactive_data$df_f$HGNC_SYMBOL,
											selected = NULL
	  )


    # update the node selection for off target nodes
    updateSelectizeInput(session, 
											"node_colours_offtgt",
											choices = reactive_data$df_f$HGNC_SYMBOL,
											selected = NULL
    )

    # update the node selection for neutral nodes
    updateSelectizeInput(session, 
											"node_colours_neutral",
											choices = reactive_data$df_f$HGNC_SYMBOL,
											selected = NULL
    )

  })

  # poll the unique values every 1x10^6 milliseconds (~17 mins)
  current_cmpd_ids <- reactivePoll(1E6, 
                                    session, 
                                    checkFunc = fetch_len_cmpd_ids, 
                                    valueFunc = fetch_unq_cmpd_ids
                      )

  # update the search compound id input box with distinct values
  observe({
          updateSelectizeInput(session, 
                              "search_cmpd_ids",
                              choices = current_cmpd_ids(),
                              selected = NULL,
                              server = TRUE
          )
  })


  # reactively update the experiment id input box with CROs
  output$exp_id_output <- renderUI({
      req(input$search_cmpd_ids)
      exp_id_list <- unique(get_unfil_kdata()$EXPERIMENT_ID)
      mdata_result <- sapply(exp_id_list, function(x) fetch_exp_mdata(x))
      if (length(mdata_result) > 0) {
        cros_n_expids <- sapply(seq(1:length(exp_id_list)), function(i) {
                                  paste0(mdata_result[, i]$EXPERIMENT_ID, " (", 
                                         mdata_result[, i]$PROPERTY_VALUE, ")",
																				 " [", mdata_result[, i]$CONC, " nM]")
                                })
        # dropdown menu that allows for selection of experimental id
        selectInput(inputId = "exp_id", label = "Experiment ID (CRO) [Conc]",
                  choices = cros_n_expids,
                  selected = NULL, multiple = FALSE)
      } else { return(NULL) }
  })

  # reactively update the technology input box
  output$tech_output <- renderUI({
      req(input$search_cmpd_ids, input$exp_id)
      tech_list <- unique(get_subfil_kdata()$TECHNOLOGY)
      # print(tech_list)
      if (length(tech_list) > 0) {
        # dropdown menu that allows for selection of experimental id
        selectInput(inputId = "tech_id", label = "Technology",
                  choices = tech_list,
                  selected = NULL, multiple = FALSE)
      } else { return(NULL) }
  })


    # ----------------- DELETE TEMP FILES WHEN SESSION ENDS ---------------- #
    
    session$onSessionEnded(function() {
        if (file.exists(svgoutfile)){file.remove(svgoutfile)}
				dbDisconnect(conn)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
