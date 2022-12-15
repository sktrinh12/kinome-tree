#---------------------- SERVER SIDE ----------------------#

server <- function(input, output, session) {
  
  # cleaned & filtered data
  observeEvent(input$cutoff, {
    if (!is.null(reactive_data$df_kd)) {
      reactive_data$df_f <- reactive_data$df_c %>% filter(Result > as.integer(input$cutoff))

      # update the node selection for target nodes
      updateSelectInput(
        session,
        "node_colours_tgt",
        choices = reactive_data$df_f$HGNC_SYMBOL,
        selected = NULL
      )
    }
  })

  # show the final df from compd_id and experiment id after clicking Submit
  observeEvent(input$submit, {
      # hide progress bar for excel file input
      session$sendCustomMessage("resetFileInputHandlerHide", "kinasefile") 

      req(input$search_cmpd_ids, input$exp_id, input$tech_id)

      # Return the final kinome dataset (single exp id + tech)
      reactive_data$df_kd <-
        fetch_f_kdata(input$search_cmpd_ids, input$exp_id,
                      input$tech_id)
      
      # cleaned data
      reactive_data$df_c <-
        clean_kinase_data(HGNC, reactive_data$df_kd, manual_map)
      
      # cleaned & filtered data
      reactive_data$df_f <- reactive_data$df_c %>% filter(Result > as.integer(input$cutoff))

      # update the node selection for target nodes
      updateSelectInput(
        session,
        "node_colours_tgt",
        choices = reactive_data$df_f$HGNC_SYMBOL,
        selected = NULL
      )
  })

  # uploading file
  observeEvent(input$kinasefile, {
      req(input$kinasefile)
      session$sendCustomMessage("resetFileInputHandlerShow", "kinasefile") 
      reactive_data$df_kd <- data.table(readxl::read_xlsx(input$kinasefile$datapath))
      reactive_data$df_c <-  clean_kinase_data(HGNC, reactive_data$df_kd, manual_map)
      reactive_data$df_f <- reactive_data$df_c %>% filter(Result > as.integer(input$cutoff))

    # update the node selection for target nodes
    updateSelectInput(
      session,
      "node_colours_tgt",
      choices = reactive_data$df_f$HGNC_SYMBOL,
      selected = NULL
    )
  })

  output$kinasetable <- renderDataTable({
    datatable(reactive_data$df_kd, options = list(pageLength = 25))
  })
  
  output$cleanedtable <- renderDataTable({
    datatable(reactive_data$df_c, options = list(pageLength = 25))
  })
  
  output$cleanedfilteredtable <- renderDataTable({
    datatable(reactive_data$df_f, options = list(pageLength = 25))
  })
  
  output$downloadInputData <-
    downloadHandler(
      filename = function() {
        paste(input$kinasefile$name,
              format(Sys.time(), "%Y%m%d"),
              ".csv",
              sep = "")
      },
      content = function(file) {
        if (isTruthy(input$kinasefile)) {
          write.csv(reactive_data$df_kd, file, row.names = FALSE)
        } else {
          write.csv(reactive_data$df_kd, file, row.names = FALSE)
        }
      }
    )
  
  output$downloadCleanedData <-
    downloadHandler(
      filename = function() {
        paste(input$kinasefile$name,
              "-cleaned-",
              format(Sys.time(), "%Y%m%d"),
              ".csv",
              sep = "")
      },
      content = function(file) {
        write.csv(reactive_data$df_c, file, row.names = FALSE)
      }
    )
  
  output$downloadCleanedFilteredData <-
    downloadHandler(
      filename = function() {
        paste(input$kinasefile$name,
              "-filtered-",
              format(Sys.time(), "%Y%m%d"),
              ".csv",
              sep = "")
      },
      content = function(file) {
        write.csv(reactive_data$df_f, file, row.names = FALSE)
      }
    )
  
  svgoutfile = tempfile(pattern = "kintreeout",
                        tmpdir = "tempfiles",
                        fileext = ".svg")  # session specific tree svg file
  
  insertUI(selector = "#treediv",
           where = "afterEnd",
           ui = source("coralR/renderTree.R",
                       local = TRUE)$value)
  
  observeEvent(input$plotresultcolumn, {
    if (input$plotresultcolumn == 'ratio') {
      hideTab(inputId = 'charttabs', target = 'Mutants')
    } else  {
      showTab(inputId = 'charttabs', target = 'Mutants')
    }
  })

  newdf <- reactive({
    # get current values
    tempdf = svginfo$dataframe
    
    # set font family
    tempdf$text.font = paste("'", "Arial", "'", sep = "")
    
    reactive_data$df_f <- reactive_data$df_c %>%  filter(Result > as.integer(input$cutoff))

    kdata = reactive_data$df_f 
    
    # print(paste('KINASE DATA CLEANED & FILTERED', paste0(rep('-',20), collapse='')))
    # print(kdata) print(input$plotresultcolumn)
    
    # either bin10, bin25 or ratio (drop down menu)
    sel_colm <- input$plotresultcolumn
    resizedf <- tempdf %>%
      select(id.coral, id.HGNC) %>%
      merge(kdata, by.x = "id.HGNC", by.y = "HGNC_SYMBOL") %>%
      mutate(KINASE = id.coral, HGNC_SYMBOL = id.HGNC) %>%
      arrange(HGNC_SYMBOL)
    
    # print(paste('RESIZEDF', paste0(rep('-',20), collapse='')))
    # print(resizedf)
    
    if (dim(resizedf)[1] == 0) {
      createAlert(
        session,
        "alert",
        "resizedf_qcheck",
        title = "Error",
        content = "Cannot map to kinome tree; there was no HGNC match for those kinases",
        append = FALSE
      )
      return()
    } else {
      closeAlert(session, "resizedf_qcheck")
      
      kdata_dim = dim(kdata)[1]
      rdf_dim = dim(resizedf)[1]
      if (kdata_dim != rdf_dim) {
        missing <- kdata %>%
          anti_join(resizedf, by = "HGNC_SYMBOL")
        # print(paste('MISSING', paste0(rep('-',20), collapse='')))
        # print(missing)
        missing <- tempdf %>%
          select(id.coral, id.HGNC) %>%
          merge(missing, by.x = "id.coral", by.y = "HGNC_SYMBOL") %>%
          mutate(KINASE = id.coral, HGNC_SYMBOL = id.HGNC) %>%
          arrange(HGNC_SYMBOL)
        
        resizedf <- rbind(resizedf, missing)
        # print(paste('RESIZEDF', paste0(rep('-',20), collapse='')))
        # print(resizedf)
      }
      node_colours <-
        c(input$node_tgt_cpick, input$node_offtgt_cpick)
      
      label_colours <-
        c(input$text_tgt_cpick, input$text_offtgt_cpick)
      
      hgnc_node_selected <- input$node_colours_tgt
      
      # default nodes to select
      if (!isTruthy(hgnc_node_selected)) {
        if (grepl("ratio", sel_colm)) {
          idx = which(resizedf$Result == min(resizedf$Result))
        } else {
          idx = which(resizedf$Result == max(resizedf$Result))
        }
        hgnc_node_selected <-
          resizedf$HGNC_SYMBOL[idx]  # default to top pct inhibitor; or lowest for ratio
      }
      
      # ------------------ NODE SIZE & COLOUR ------------------ #
      
      if (grepl("ratio", sel_colm)) {
        resizedf <- resizedf %>%
          mutate(`:=`(!!sel_colm, log(Result)))
      }
      
      # set colors based on selected ids
      if (nrow(resizedf) > 0) {
        # sizerange = c(1, 60)
        radii_and_mapping = resizes.by.value(
          df = tempdf,
          resizedf = resizedf,
          sizerange = c(input$nodesizemin, input$nodesizemax),
          controlledrange = FALSE,
          minvalue = 0,
          maxvalue = 100,
          showall = "hide",
          sel_colm = sel_colm
        )
      }
      
      # ------------------ ARTIFICIALLY INCREASE/DECREASE ---- #
      # ------------------ NODE SIZES FOR CONTRASTING -------- #
      if (grepl(x = sel_colm, pattern = "bin")) {
        resizedf <- radii_and_mapping[[3]] %>%
          mutate(
            radii = case_when(
              !!as.name(sel_colm) <= 60 & !!as.name(sel_colm) >
                0 ~ radii * 0.04,!!as.name(sel_colm) <= 70 &
                !!as.name(sel_colm) >
                60 ~ radii * 0.18,!!as.name(sel_colm) <= 80 &
                !!as.name(sel_colm) >
                70 ~ radii * 0.3,!!as.name(sel_colm) <= 90 &
                !!as.name(sel_colm) >
                80 ~ radii * 0.5,
              TRUE ~ radii
            )
          )
        # reduce size of off-target nodes by half re-map the node radii
        # size (for tree and legend) resizedf is passed into
        # tdf_node_size downstream
        radii_and_mapping[[1]][resizedf$dflookup] <-
          resizedf$radii
      }
      
      # add new columns for bins & inflate radius size when 'ratio'
      if (grepl(x = sel_colm, pattern = "ratio")) {
        resizedf <- radii_and_mapping[[3]] %>%
          mutate(
            radii = case_when(
              Result <= 1 & Result >
                0 ~ radii * 4.25,
              Result <= 10 & Result >
                1 ~ radii * 2,
              Result <= 100 & Result >
                10 ~ radii * 1.25,
              Result > 900 ~ radii * 0.7,
              TRUE ~ radii
            ),
            `:=`(
              !!as.name(paste0(sel_colm, "_range")),
              case_when(Result <= 1 & Result > 0 ~ "0-1",
                        Result <= 10 & Result > 1 ~ "1-10",
                        Result <= 100 & Result > 10 ~ "10-100",
                        Result >= 100 ~ ">100"
                        )
            )
          )
        
        radii_and_mapping[[1]][resizedf$dflookup] <-
          resizedf$radii
      }
      
      print(paste("RESIZEDF-AFTER", paste0(rep("-", 20), collapse = "")))
      print(resizedf)
      
      # ------------------ ADVANCED OPTIONS ------------------ #
      
      # Change Color and Size of Font for Selected kinases set selected
      # font size
      tempdf <- tempdf %>%
        mutate(
          node.selected = ifelse(id.coral %in% resizedf$id.coral, 1, -1),
          node.radius = radii_and_mapping[[1]],
          node.val.radius = radii_and_mapping[[2]],
          text.size = ifelse(node.selected == 1, input$nodelabelfontsize,
                             0),
          node.opacity = input$nodeopacity / 100,
          branch.col = BG_col1
        )
      
      
      # set node colours
      tempdf <- tempdf %>%
        mutate(
          node.col = case_when(
            id.HGNC %in% hgnc_node_selected ~ node_colours[1],
            TRUE ~ node_colours[2]
          ),
          text.col = ifelse(
            node.col == node_colours[1],
            input$text_tgt_cpick,
            input$text_offtgt_cpick
          )
        )
      
      tdf_group_colour <- tdf_base_gc %>%
        mutate(colours = c(input$node_tgt_cpick, input$node_offtgt_cpick))
      
      print(paste("TDF_GROUP_COLOUR", paste0(rep("-", 20), collapse = "")))
      print(tdf_group_colour)
      
      # combine the input types (result, bins) with the node.radius and
      # misc columns node size dataframe for reference legend
      
      
      # write.csv(resizedf, 'tmp-data.csv', row.names = FALSE)
      tdf_node_size <- tempdf %>%
        filter(id.coral %in% resizedf$id.coral) %>%
        merge(resizedf, by = c("id.coral", "id.HGNC")) %>%
        mutate(`:=`(!!sel_colm, as.integer(!!as.name(sel_colm)))) %>%
        arrange(!!as.name(sel_colm)) %>%
        distinct(!!as.name(ifelse(
          grepl(x = sel_colm, pattern = "ratio"),
          paste0(sel_colm, "_range"),
          sel_colm
        )), .keep_all = T) %>%
        mutate(
          row_index = row_number(),
          cumsum_radius = cumsum(2 * node.radius +
                                   buffer),
          y_pos = ifelse(
            row_index == 1,
            y + node.radius,
            lag(cumsum_radius) +
              node.radius + y
          ),
          cy_pos = y_pos,
          x_pos = 153.9432 - (1.25 * buffer +
                                max(node.radius)),
          cx_pos = 160.9432,
          radius = node.radius,
          label = !!as.name(paste0(sel_colm,
                                   "_range")),
          colours = BG_col1,
          font_size = 9
        ) %>%
        select(-branch.coords)
      
      print(paste("TDF_NODE_SIZE", paste0(rep("-", 20), collapse = "")))
      print(tdf_node_size)
      
      return(list(tempdf, tdf_node_size, tdf_group_colour, resizedf))
      
    }  # end if-else stmt
  })  # end reactive
  
  
  # build the manning tree
  output$plot1 <- renderSvgPanZoom({
    # recolor the official matrix
    df_data = newdf()
    
    if (dim(df_data[[1]])[1] == 0) {
      stop("Error - no data retrieved based on given constraints")
    }
    
    # join to get the Result column
    svginfo$dataframe = df_data[[1]] %>%
      left_join(df_data[[4]][, c("id.coral", "Result")])
    svginfo$node_size_legend = df_data[[2]]
    svginfo$node_group_colour = df_data[[3]]
    
    # set title
    svginfo$title = input$charttitle
    
    if (!dir.exists(dirname(svgoutfile))) {
      dir.create(dirname(svgoutfile), showWarnings = F)
    }
    
    # Write SVG file
    writekinasetree(
      svginfo,
      destination = svgoutfile,
      font = "Arial",
      labelselect = "HGNC",
      groupcolor = "#000000",
      titley = input$titley,
      titlefontsize = input$titlefontsize
    )
    
    # Render SVG
    svgPanZoom(svgoutfile,
               viewBox = F,
               controlIconsEnabled = F)
  })
  
  output$downloadtree <-
    downloadHandler(filename <- function(file) {
      "kinome.tree.svg"
    }, content <- function(file) {
      file.copy(svgoutfile, file)
    })
  
  # ----------------- FILTER BY COMPONUD ID -----------------
  
  # Return the unfiltered kinome dataset (all exp ids)
  get_unfil_kdata <- reactive({
    req(input$search_cmpd_ids)
    fetch_uf_kdata(input$search_cmpd_ids)
  })
  
  # Return the sub-final kinome dataset (single exp id)
  get_subfil_kdata <- reactive({
    req(input$search_cmpd_ids, input$exp_id)
    fetch_f_kdata(input$search_cmpd_ids, input$exp_id)
  })
  
  
  # poll the unique values every 1x10^6 milliseconds (~17 mins)
  current_cmpd_ids <-
    reactivePoll(1e+06, session, checkFunc = fetch_len_cmpd_ids,
                 valueFunc = fetch_unq_cmpd_ids)
  
  # update the search compound id input box with distinct values
  observe({
    updateSelectizeInput(
      session,
      "search_cmpd_ids",
      choices = current_cmpd_ids(),
      selected = NULL,
      server = TRUE
    )
  })
  
  # pass url parameters to the text input
  observe({
    param_query <- parseQueryString(session$clientData$url_search)
    updateSelectizeInput(
      session,
      "search_cmpd_ids",
      choices = current_cmpd_ids(),
      selected = param_query[["search_cmpd_ids"]],
      server = TRUE
    )
  })
  
  # reactively update the experiment id input box with CROs
  output$exp_id_output <- renderUI({
    req(input$search_cmpd_ids)
    exp_id_list <- sort(unique(get_unfil_kdata()$EXPERIMENT_ID))
    mdata_result <-
      sapply(exp_id_list, function(x)
        fetch_exp_mdata(x))
    if (length(mdata_result) > 0) {
      meta_data <- sapply(seq(1:length(exp_id_list)), function(i) {
        paste0(
          mdata_result[, i]$EXPERIMENT_ID,
          " (",
          mdata_result[, i]$PROPERTY_VALUE,
          ")",
          " [",
          mdata_result[, i]$CONC,
          " nM]",
          " [",
          mdata_result[,
                       i]$ORDER_NUM,
          "]"
        )
      })
      # dropdown menu that allows for selection of experimental id
      selectInput(
        inputId = "exp_id",
        label = "Experiment ID (CRO) [Conc] [Order]",
        choices = meta_data,
        selected = NULL,
        multiple = FALSE
      )
    } else {
      return(NULL)
    }
  })
  
  # reactively update the technology input box
  output$tech_output <- renderUI({
    req(input$search_cmpd_ids, input$exp_id)
    tech_list <- unique(get_subfil_kdata()$TECHNOLOGY)
    # print(tech_list)
    if (length(tech_list) > 0) {
      # dropdown menu that allows for selection of experimental id
      selectInput(
        inputId = "tech_id",
        label = "Technology",
        choices = tech_list,
        selected = NULL,
        multiple = FALSE
      )
    } else {
      return(NULL)
    }
  })
  
  
  observeEvent(input$click_polar, {
    req(input$search_cmpd_ids, input$exp_id, input$tech_id)
    
    # returns a list of df (without NA's and only NA's)
    dt <-
      clean_mutkinase_data(HGNC,
                           reactive_data$df_kd,
                           manual_map,
                           input$cutoff,
                           input$kinasefilter)
    
    if (is.null(dt)) {
      createAlert(
        session,
        "alert",
        "dt_polar_qcheck",
        title = "Error",
        content = "Could not fetch a valid dataset",
        append = FALSE
      )
      return()
    }
    
    reactive_data$missing <- dt$missing_kinasedata
    
    dt <- svginfo$dataframe %>%
      select(id.coral, id.HGNC) %>%
      merge(dt$kinasedata, by.x = "id.HGNC", by.y = "SYMBOL") %>%
      rename(HGNC_SYMBOL = id.HGNC, CRO_Kinase = KINASE) %>%
      mutate(group = as.factor(HGNC_SYMBOL)) %>%
      arrange(KINASE_NAME, Result)
    
    print(paste("SVGINFO MERGED ON KINASEDATA", paste0(rep("-", 20), collapse = "")))
    print(dt)
    
    if (dim(dt)[1] == 0) {
      createAlert(
        session,
        "alert",
        "dt_polar_qcheck",
        title = "Error",
        content = "No mutant kinase data to plot",
        append = FALSE
      )
      return()
    }
    
    NA_to_add <-
      data.frame(matrix(NA, empty_bars * nlevels(dt$group), ncol(dt)))
    colnames(NA_to_add) <- colnames(dt)
    NA_to_add$group <- rep(levels(dt$group), each = empty_bars)
    dt <- rbind(dt, NA_to_add) %>%
      arrange(group) %>%
      mutate(INDEX = row_number())
    
    number_of_bars <- nrow(dt)
    angle <-
      90 - 360 * (dt$INDEX - 0.5) / number_of_bars  # to place in the middle
    dt$hjust <-
      ifelse(angle < -90, 1, 0)  # horizontal adjust when on left/right
    dt$angle <- ifelse(angle < -90, angle + 180, angle)
    
    # set colour schema
    colours <- cpalette[seq(nlevels(dt$group))]
    
    names(colours) <- levels(dt$group)
    # print(paste('COLOURS FACTOR', paste0(rep('-',20), collapse='')))
    # print(colours)
    
    colours <-
      ggplot2::scale_fill_manual(name = "HGNC SYMBOLS", values = colours)
    
    reactive_data$polarplot <-
      ggplot2::ggplot(dt,
                      ggplot2::aes(
                        x = as.factor(INDEX),
                        y = Result,
                        fill = HGNC_SYMBOL
                      )) + ggplot2::geom_bar(ggplot2::aes(x = as.factor(INDEX),
                                                          y = Result),
                                             stat = "identity",
                                             alpha = 0.7) + ggplot2::ylim(-20, 122) +
      ggplot2::theme_minimal() + ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()  #,
      ) + ggplot2::coord_polar() + ggplot2::geom_text(
        ggplot2::aes(
          x = INDEX,
          y = Result + 4,
          label = CRO_Kinase,
          hjust = hjust
        ),
        color = "black",
        fontface = "bold",
        alpha = 0.9,
        size = 2.5,
        angle = dt$angle
      ) + ggplot2::geom_text(
        ggplot2::aes(
          x = INDEX,
          y = Result - 20,
          label = round(Result, 1),
          hjust = hjust
        ),
        color = "black",
        fontface = "bold",
        alpha = 0.6,
        size = 3,
        angle = dt$angle
      ) + ggplot2::ggtitle(paste("Mutant Pct Inb Polar Histogram -",
                                 input$search_cmpd_ids)) + colours
  })
  
  observeEvent(input$click_lolli, {
    req(input$search_cmpd_ids, input$exp_id, input$tech_id)
    dt <-
      clean_mutkinase_data(HGNC,
                           reactive_data$df_kd,
                           manual_map,
                           input$cutoff,
                           input$kinasefilter)
    
    if (is.null(dt)) {
      createAlert(
        session,
        "alert",
        "dt_lolli_qcheck",
        title = "Error",
        content = "Filters were too strict, no data to display",
        append = FALSE
      )
      return()
    }
    
    reactive_data$missing <- dt$missing_kinasedata
    
    # replace this var
    dt <- svginfo$dataframe %>%
      select(id.coral, id.HGNC) %>%
      merge(dt$kinasedata, by.x = "id.HGNC", by.y = "SYMBOL") %>%
      rename(HGNC_SYMBOL = id.HGNC, CRO_Kinase = KINASE)
    
    # print(paste('MUTANT DATA CLEANED & FILTERED', paste0(rep('-',20),
    # collapse=''))) print(dt)
    
    dt <- dt %>%
      arrange(KINASE_NAME, Result) %>%
      mutate(CRO_Kinase = factor(CRO_Kinase, levels = CRO_Kinase))
    
    if (dim(dt)[1] == 0) {
      createAlert(
        session,
        "alert",
        "dt_lolli_qcheck",
        title = "Error",
        content = "No mutant kinase data to plot",
        append = FALSE
      )
      return()
    }
    
    # set colour schema
    families <- factor(unique(dt$HGNC_SYMBOL))
    colours <- cpalette[seq(length(families))]
    
    names(colours) <- levels(families)
    # print(paste('COLOURS FACTOR', paste0(rep('-',20), collapse='')))
    # print(colours)
    colours <-
      ggplot2::scale_colour_manual(name = "HGNC SYMBOLS", values = colours)
    
    reactive_data$lolliplot <-
      ggplot2::ggplot(dt, ggplot2::aes(x = CRO_Kinase,
                                       y = Result)) + ggplot2::geom_segment(
                                         ggplot2::aes(
                                           x = CRO_Kinase,
                                           xend = CRO_Kinase,
                                           y = 0,
                                           yend = Result,
                                           colour = factor(HGNC_SYMBOL)
                                         ),
                                         size = 0.7
                                       ) + ggplot2::geom_point(ggplot2::aes(colour = factor(HGNC_SYMBOL)),
                                                               size = 2) + ggplot2::geom_text(
                                                                 ggplot2::aes(
                                                                   x = CRO_Kinase,
                                                                   y = Result,
                                                                   label = round(Result, 1)
                                                                 ),
                                                                 nudge_y = -5,
                                                                 size = 3
                                                               ) + colours + ggplot2::coord_flip() +
      ggplot2::scale_x_discrete(limits = rev) + ggplot2::ggtitle(paste("Mutant Pct Inb Lolliplot -",
                                                                       input$search_cmpd_ids)) + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7.5))
  })
  
  output$lolliplot <- renderPlot({
    if (is.null(reactive_data$lolliplot))
      return()
    reactive_data$lolliplot
  })
  
  output$polarplot <- renderPlot({
    if (is.null(reactive_data$polarplot))
      return()
    reactive_data$polarplot
  })
  
  output$downloadlolliplot <-
    downloadHandler(filename <- function(file) {
      paste("lolliplot_", input$search_cmpd_ids, ".png")
    }, content <- function(file) {
      ggplot2::ggsave(file, reactive_data$lolliplot)
    })
  
  output$downloadpolarplot <-
    downloadHandler(filename <- function(file) {
      paste("polarplot_", input$search_cmpd_ids, ".png")
    }, content <- function(file) {
      ggplot2::ggsave(file, reactive_data$polarplot)
    })
  # ----------------- DELETE TEMP FILES WHEN SESSION ENDS ---------------- #
  
  session$onSessionEnded(function() {
    if (file.exists(svgoutfile)) {
      file.remove(svgoutfile)
    }
    # dbDisconnect(conn)
  })
}
