# Libraries for Coral code
# load basic libraries
library(shiny)
library(shinydashboard)
library(shinyBS)
library(readr)
library(rsvg)
library(shinyWidgets)
library(RColorBrewer)

# load Manning-related libraries
library(svgPanZoom)

# load ui-related libraries
library(colourpicker)
library(DT)

# load other network libraries
library(data.tree)
library(jsonlite)
# End libraries for Coral code


library(data.table)


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#---------------------- SOURCE R FILES ----------------------#

source("coralR/colorby.R")
source("coralR/readinput.R")
source("coralR/writekinasetree.R")
source("coralR/legendfunctions.R")
source("coralR/map2color.R")
source("coralR/convertID.R")
source("coralR/makejson.R")
source("coralR/colors.R")
source("coralR/radiobuttonswithimages.R")

#---------------------- READ IN AND ORGANIZE DATA ----------------------#

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

# get example RNA data
rna_data     = paste(readLines("Data/RNAdata.txt"),collapse="\n")
rna_abs_data = paste(readLines("Data/RNAdata_pluripotent.txt"),collapse="\n")

#---------------------- DEFAULT COLORS ----------------------#

# Default tree branch color
BG_col1 = "#D3D3D3"

# Default selected color
Cor_col = "#db0606"  #FA6958"

# 2-color heatmap colors
HM2_low = "#1EA0F7"
HM2_hi = "#FACE1E"

# default heatmap colors
HM_low = "#1b8ed1"
HM_med = "#e0e0e0"
HM_hi = "#FA6958"


### Qualtative Palettes ###

qualitative_palette_choices <- c('<img src="images/Erika.png">' = 'Erika',
                                 '<img src="images/Accent.png">' = 'Accent',
                                 '<img src="images/Dark2.png">' = 'Dark2',
                                 '<img src="images/Paired.png">' = 'Paired',
                                 '<img src="images/Pastel1.png">' = 'Pastel1',
                                 '<img src="images/Pastel2.png">' = 'Pastel2',
                                 '<img src="images/Set1.png">' = 'Set1',
                                 '<img src="images/Set2.png">' = 'Set2',
                                 '<img src="images/Set3.png">' = 'Set3')

# my qualitative palettes
Erika = c("#FA6958","#3F9FFC","#FAD53F","#B0E6C2","#B348A1","#2CD1E0","#BEC956","#7C64FF","#C2374A","#70BD93","#FFBB99","#BA97F2")
Accent = brewer.pal(8,"Accent")
Dark2 = brewer.pal(8,"Dark2")
Paired = brewer.pal(12,"Paired")
Pastel1 = brewer.pal(9,"Pastel1")
Pastel2 = brewer.pal(8,"Pastel2")
Set1 = brewer.pal(9,"Set1")
Set2 = brewer.pal(8,"Set2")
Set3 = brewer.pal(12,"Set3")

qualpalettes = list(Erika,Accent,Dark2,Paired,Pastel1,Pastel2,Set1,Set2,Set3)
names(qualpalettes) = c("Erika","Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")

if (! dir.exists('www/images')) {
  dir.create('www/images', showWarnings = F) 
}

drawmypalettes("Erika",Erika,"www/images",boxes =5)
drawmypalettes("Accent",Accent,"www/images",boxes =5)
drawmypalettes("Dark2",Dark2,"www/images",boxes =5)
drawmypalettes("Paired",Paired,"www/images",boxes =5)
drawmypalettes("Pastel1",Pastel1,"www/images",boxes =5)
drawmypalettes("Pastel2",Pastel2,"www/images",boxes =5)
drawmypalettes("Set1",Set1,"www/images",boxes =5)
drawmypalettes("Set2",Set2,"www/images",boxes =5)
drawmypalettes("Set3",Set3,"www/images",boxes =5)

### Sequential Palettes ###

sequential_palette_choices <- c(
  '<img src="images/Greys.png">' = 'Greys',
  '<img src="images/Reds.png">' = 'Reds',
  '<img src="images/Oranges.png">' = 'Oranges',
  '<img src="images/Greens.png">' = 'Greens',
  '<img src="images/Blues.png">' = 'Blues',
  '<img src="images/Purples.png">' = 'Purples')


# my sequential palettes
Greys = brewer.pal(3,"Greys")
Reds = brewer.pal(3,"Reds")
Oranges = brewer.pal(3,"Oranges")
Greens = brewer.pal(3,"Greens")
Blues = brewer.pal(3,"Blues")
Purples = brewer.pal(3,"Purples")

seqpalettes = list(Greys,Reds,Oranges,Greens,Blues,Purples)
names(seqpalettes) = c("Greys","Reds","Oranges","Greens","Blues","Purples")

drawmypalettes("Greys",Greys,"www/images")
drawmypalettes("Reds",Reds,"www/images")
drawmypalettes("Oranges",Oranges,"www/images")
drawmypalettes("Greens",Greens,"www/images")
drawmypalettes("Blues",Blues,"www/images")
drawmypalettes("Purples",Purples,"www/images")

### Divergent Palettes ###

divergent_palette_choices <- c('<img src="images/Blue_Grey_Coral.png">' = 'Blue_Grey_Coral',
                               '<img src="images/Bro_Grey_Tur.png">' = 'Bro_Grey_Tur',
                               '<img src="images/Pink_Grey_Gre.png">' = 'Pink_Grey_Gre',
                               '<img src="images/Pur_Grey_Gre.png">' = 'Pur_Grey_Gre',
                               '<img src="images/Pur_Grey_Or.png">' = 'Pur_Grey_Or',
                               '<img src="images/Red_Grey_Gre.png">' = 'Red_Grey_Gre')

# my divergent palettes
Blue_Grey_Coral = c("#1B8ED1","#e5e5e5","#FA6958")
Bro_Grey_Tur = c("#A6611A","#e5e5e5", "#018571")
Pink_Grey_Gre = c("#D01C8B","#e5e5e5", "#4DAC26")
Pur_Grey_Gre = c("#7B3294","#e5e5e5", "#008837")
Pur_Grey_Or = c("#E66101","#e5e5e5", "#5E3C99")
Red_Grey_Gre = c("#CA0020","#e5e5e5", "#404040")

divpalettes = list(Blue_Grey_Coral,Bro_Grey_Tur,Pink_Grey_Gre,Pur_Grey_Gre,Pur_Grey_Or,Red_Grey_Gre)
names(divpalettes) = c("Blue_Grey_Coral","Bro_Grey_Tur","Pink_Grey_Gre","Pur_Grey_Gre","Pur_Grey_Or","Red_Grey_Gre")

drawmypalettes("Blue_Grey_Coral",Blue_Grey_Coral,"www/images")
drawmypalettes("Bro_Grey_Tur",Bro_Grey_Tur,"www/images")
drawmypalettes("Pink_Grey_Gre",Pink_Grey_Gre,"www/images")
drawmypalettes("Pur_Grey_Gre",Pur_Grey_Gre,"www/images")
drawmypalettes("Pur_Grey_Or",Pur_Grey_Or,"www/images")
drawmypalettes("Red_Grey_Gre",Red_Grey_Gre,"www/images")



# Default group color palette
defaultpalette = colorRampPalette( c(
  "#FA6958",
  "#3F9FFC",
  "#FAD53F",
  "#B0E6C2",
  "#B348A1",
  "#2CD1E0",
  "#BEC956",
  "#7C64FF",
  "#C2374A",
  "#70BD93",
  "#FFBB99",
  "#BA97F2"
))(12)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


source("lib/Rename.R")

HGNC <- data.table(read_excel("data-input/HGNC-protein-coding-genes.xlsx"))
#setDT(HGNC)

manual_map <- data.table(read_excel("data-input/manual-map.xlsx"))
#setDT(manual_map)

# 
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
        tabItems(
            tabItem("data",
                    fluidRow(
                        box(
                            width = 8,
                            fileInput("kinasefile", "Choose Excel File", multiple = FALSE, accept = c(".xlsx")),
                            p("Cleaned File: removes Kinase values with a bracket ( [ ),forward slash ( / ) or hyphen (-). The CRO kinase symbols are converted to HGNC symbols. Each result is tagged with bin values for plotting.")
                        ),
                        box(
                            width = 4,
                            numericInput(inputId = "cutoff", label = "Remove Values Less Than", 50),
                            textInput(inputId = "atpfilter", label = "Remove Value from ATP Column", value = "Cascade")
                        )
                    ),
                    tabBox(
                        #title = "Files",
                        id = "filetabs",
                        width = 12,
                        selected = "Input File",
                        tabPanel("Input File", 
                            dataTableOutput("kinasetable"),
                            downloadButton("downloadInputData", "Download")
                        ),
                        tabPanel("Cleaned File", 
                            dataTableOutput("cleanedtable"),
                            downloadButton("downloadCleanedData", "Download")
                        ),
                        tabPanel("Filtered File", 
                                 dataTableOutput("cleanedfilteredtable"),
                                 downloadButton("downloadCleanedFilteredData", "Download")
                        )
                    )
            ),
            tabItem("chart",
                # box(
                #     width = 8,
                #     plotOutput("distPlot")
                # ),
                # box(
                #     width = 4,
                #     numericInput(inputId = "bins", label = "Number of Bins", value = 4, min = 0, step = 1)
                # ),
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
                  numericInput(inputId = "nodesizemax", label = "Node Size Max", value = 60),
                  numericInput(inputId = "nodelabelfontsize", label = "Node Label Font Size", value = 0),
                  textInput(inputId = "nodelabelcolor", label = "Node Label Color (default #999999)", value = "#999999"),
                  #selectInput(inputId = "nodecolors", label = "Node Color", choices = c("red", "gray", "teal")),
                  textInput(inputId = "nodecolor", label = "Node Color (default #db0606)", value = "#db0606"),
                  textInput(inputId = "nodeoutlinecolor", label = "Node Outline Color (default #a80606)", value = "#a80606"),
                  numericInput(inputId = "nodeopacity", label = "Node Opacity (default 50%)", value = 50, min = 0, max = 100, step = 5)
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
            {
                dt <- data.table(readxl::read_xlsx(input$kinasefile$datapath))
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        return(dt)
    })
    
    kinaseDataCleaned <- reactive({
        req(input$kinasefile)
        
        dt <- setDT(clean_kinase_data(HGNC, kinaseData(), manual_map))
        
        return(dt)
    })
    
    kinaseDataCleanedFiltered <- reactive({
        req(input$kinasefile)
        
      
        return(kinaseDataCleaned()[Result >= input$cutoff & ATP != input$atpfilter])
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
    
    outputjson      = tempfile(pattern="kinome_tree",tmpdir="www/json",fileext = ".json") # session specific json file describing network
    #outputjsonshort = paste("json/",strsplit(outputjson,split = "/")[[1]][3],sep="") # used to communicate to js
    subdffile       = tempfile(pattern="subdf",tmpdir="tempfiles",fileext = ".txt") # temp file used to make json file
    svgoutfile      = tempfile(pattern="kintreeout",tmpdir="tempfiles",fileext = ".svg") # session specific tree svg file
    
    insertUI(selector = "#treediv",where = "afterEnd",ui = source("coralR/renderTree.R",local=TRUE)$value)
    
    newdf <- reactive({ 
        
        # get current values
        tempdf = svginfo$dataframe
        
        # set font family
        #tempdf$text.font = paste("'",input$fontfamilyselect,"'",sep="")
        tempdf$text.font = paste("'","Arial","'",sep="")
        
        # establish legend
        legend = c()
        # Set initial yoffset
        yoffset = 79.125
        
        # get current values
        tempdf$text.size = 4 #input$fontsize
        
        # Single branch color
        #if (input$branchcolortype == "Uniform")
        #{
            tempdf$branch.col = BG_col1 #input$col_branch_single
        #}
        
        # ------------------ NODE COLOR ------------------ #
        
            # set colors based on selected ids
            selkinases = ""
              
            #selkinases = unlist(strsplit(split = "\n",x=input$KinasesManualNodeText))
            selkinases = kinaseDataCleanedFiltered()$HGNC_Symbol
              
            selkinasescoral = ""
            tempdf$node.selected = -1
              
              if (length(selkinases) > 0)
              {
                # convert selected to coral ids
                kinasestoconvert = data.frame(kin1=selkinases,kin2=selkinases)
                selkinasesconverted = convertID (tempdf,kinasestoconvert,inputtype="HGNC") #input$NodeManualIDtype)
                
                if (nrow(selkinasesconverted) > 0)
                {
                  selkinasescoral = selkinasesconverted[,1]
                }
                
                # note them as selected so we can add them to the top of the plot later
                tempdf$node.selected[which(tempdf$id.coral %in% selkinasescoral)] = 1
              }

              # recolor based on selection
              #tempdf$node.col =  color.by.selected(df = tempdf, sel = selkinasescoral, bg.col  = input$col_node_bg,  sel.col = input$col_sel_node)
              tempdf$node.col =  color.by.selected(
                df = tempdf, 
                sel = selkinasescoral, 
                bg.col  = BG_col1, #input$col_node_bg,  
                sel.col = input$nodecolor #Cor_col #input$col_sel_node
              )
             
              # # build legend for Node Color (Manual Selection)
              # lines_and_offset = build.group.legend(yoffset=yoffset,groupslabels=c(input$node_select_label,input$node_nonselect_label),groupcolors=c(input$col_sel_node,input$col_node_bg),elementtype = "Node",fontfamily = input$fontfamilyselect)
              # lines = lines_and_offset[[1]]
              # yoffset = lines_and_offset[[2]] + 14
              # legend = c(legend,lines)
            #}        

        
        # ------------------ NODE SIZE ------------------ #
        
        # color nodes by single color
print(input$plotresultcolumn)            
result <- input$plotresultcolumn
            resizedf = data.frame(kinaseDataCleanedFiltered()[, c("HGNC_Symbol", result), with = FALSE])#input$plotresultcolumn
print(resizedf)
            # convert to coral id
            resizedf = convertID (tempdf, resizedf, inputtype = "HGNC") #inputtype=input$nodesizeValueIDtype)

            if (nrow(resizedf)>0)
            {
                radii_and_mapping = resizes.by.value(
                    df = tempdf, 
                    resizedf = resizedf, 
                    sizerange = c(input$nodesizemin, input$nodesizemax),  #c(1, 60), #input$nodesizeValueslider,
                    controlledrange = FALSE, #input$Manuallysetdatarange, 
                    minvalue = 1, #input$nodesizevaluemin, 
                    maxvalue = 1, #input$nodesizevaluemax,
                    showall = "hide" #input$nodesizefornotprovidedquantitative
                )
                
                # Get correct limits for legend
                    minvalforlegend = 1 #input$nodesizevaluemin
                    maxvalforlegend = 10 #input$nodesizevaluemax
                
                tempdf$node.radius     = radii_and_mapping[[1]]
                tempdf$node.val.radius = radii_and_mapping[[2]]
                
                # add legend info
                # lines_and_offset = build.nodesize.legend (
                #     yoffset=yoffset,
                #     minval=minvalforlegend,
                #     maxval=maxvalforlegend,
                #     minsize = 1, #input$nodesizeValueslider[1],
                #     maxsize = 60, #input$nodesizeValueslider[2],
                #     fontfamily = "Arial", #input$fontfamilyselect, 
                #     subtitle= "", #input$quantvaluenamenodesize
                # )
                # 
                # lines = lines_and_offset[[1]]
                # yoffset = lines_and_offset[[2]] + 14
                # #if (input$includelegend == TRUE)
                # #{
                #     legend = c(legend,lines)
                # #}
                #legend = c(legend,lines)
            }
        #}
        
        # ------------------ ADVANCED OPTIONS ------------------ #
        
        tempdf$node.opacity = input$nodeopacity/100 #0.5 #input$Node_Opacity
        
        # text color
        
        # Change Color and Size of Font for Selected kinases

            #selkinases = unlist(strsplit(split = "\n",x=input$KinasesManualLabelsText))
            selkinases = kinaseDataCleanedFiltered()$HGNC_Symbol
            
            selkinasescoral = ""
            if (length(selkinases) > 0)
            {
                # convert selected to coral ids
                kinasestoconvert = data.frame(kin1=selkinases,kin2=selkinases)
                #selkinasesconverted = convertID (tempdf,kinasestoconvert,inputtype=input$labelsManualIDtype)
                selkinasesconverted = convertID (tempdf,kinasestoconvert,inputtype = "HGNC")
                if (nrow(selkinasesconverted) > 0)
                {
                    selkinasescoral = selkinasesconverted[,1]
                }
            }
            
            # set background color and font size
            tempdf$text.col = BG_col1 #input$fontcolorbackground
            tempdf$text.size = 0 #input$fontsizebackground
            
            # set selected font color and size
            tempdf$text.col[which(tempdf$id.coral %in% selkinasescoral)]  = input$nodelabelcolor #"#000000" #input$fontcolorselection
            tempdf$text.size[which(tempdf$id.coral %in% selkinasescoral)] = input$nodelabelfontsize #input$fontsizeselection
        #}
        
        
        
        # Node stroke color

            tempdf$node.strokecol = input$nodeoutlinecolor #"#a80606" #input$nodestrokecol
        
        return(list(tempdf,legend))
    }) # end reactive
    
    
    # build the manning tree
    output$plot1  <- renderSvgPanZoom ({
        
        # recolor the official matrix
        dfandlegend = newdf()
        svginfo$dataframe = dfandlegend[[1]]
        svginfo$legend = dfandlegend[[2]]
        
        # set title
        svginfo$title = input$charttitle
        
        if (! dir.exists(dirname(svgoutfile))) {
            dir.create(dirname(svgoutfile),showWarnings = F);  
        }
        
        # Write SVG file
        #writekinasetree(svginfo,destination=svgoutfile,font=input$fontfamilyselect,labelselect=input$kinaselabelselect,groupcolor = input$groupcolorchoose)
        writekinasetree(svginfo, destination = svgoutfile, font="Arial", labelselect = "HGNC", groupcolor = "#000000", titley = input$titley, titlefontsize = input$titlefontsize)
        
        # Render SVG
        svgPanZoom(svgoutfile, viewBox = F, controlIconsEnabled=F)
    })
    
    output$downloadtree <- downloadHandler(
        
        filename <- function(file) { paste("CORAL",".","tree",".","svg",sep="")},
        content <- function(file) {
            file.copy(svgoutfile, file)
        }
    )
    
    # ----------------- DELETE TEMP FILES WHEN SESSION ENDS ---------------- #
    
    session$onSessionEnded(function() {
        if (file.exists(outputjson)){file.remove(outputjson)}
        if (file.exists(subdffile)){file.remove(subdffile)}
        if (file.exists(svgoutfile)){file.remove(svgoutfile)}
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
