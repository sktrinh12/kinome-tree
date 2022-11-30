#---------------------- GLOBAL DECLARATIONS ----------------------#
library(RColorBrewer)
library(readxl)
library(writexl)
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)

# options for spinner (loading)
options(spinner.color = "#0275D8", spinner.color.background = "#ffffff", spinner.size = 3)

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

# initilize legend orig_svginfo$legend = c()

# add node opacity
orig_svginfo$dataframe$node.opacity = 0.5  #1

# add node order
orig_svginfo$dataframe$node.selected = -1

# make svginfo (leaving the original intact)
svginfo = orig_svginfo

# assi
 node and branch orders
svginfo$dataframe$nodeorder = 1:nrow(svginfo$dataframe)
svginfo$dataframe$branchorder = 1:nrow(svginfo$dataframe)

# Default tree branch color
BG_col1 = "#D3D3D3"
empty_bars <- 1

cpalette <- c(brewer.pal(9, "Set1"), brewer.pal(8, "Set2"))

# base dataframe for group colour legend
tdf_base_gc <- data.frame(label = c("Target", "Off-Target"), colours = c(cpalette[1],
    cpalette[2]), y_pos = c(350.5302, 364.5302), cy_pos = c(347.5302, 361.5302),
    radius = 5, x_pos = 728.5648, cx_pos = 718.5684, font_size = 9)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

reactive_data <- reactiveValues(lolliplot = NULL, polarplot = NULL, dt = NULL)

HGNC <- data.table(read_excel("data-input/HGNC-protein-coding-genes.xlsx")) %>%
    mutate(SYMBOL_UPPER = stringr::str_to_upper(SYMBOL))

manual_map <- data.table(read_excel("data-input/manual-map.xlsx"))
