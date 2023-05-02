source("lib/Cofactors.R")
source("lib/Coexpressions.R")

# regex string generator to find partial matches
get_match_indices <- function(ls_sp1, ls_sp2, match_values, i) {
  lower_sp1 <- tolower(substr(ls_sp1[i], 2, nchar(ls_sp1[i])))
  first_sp1 <- substr(ls_sp1[i], 1, 1)
  sp2 <- ls_sp2[i]
  pattern = paste0("(.*[",
                   toupper(first_sp1),
                   "|",
                   tolower(first_sp1),
                   "]",
                   lower_sp1,
                   ".*",
                   sp2,
                   ")")
  match_indices <-
    grep(pattern = pattern,
         ignore.case = F,
         x = match_values)
  # print(paste(i, ':', match_indices))
  return(match_indices)
}

# main call to loop thru indices for a 1 index match
map_regex_match <- function(df, HGNC, i) {
  ls_sp1 <- df$sp1
  ls_sp2 <- df$sp2
  name_match_indices <-
    get_match_indices(ls_sp1, ls_sp2, HGNC$NAME, i)
  if (length(name_match_indices) == 1) {
    return(name_match_indices)
  } else {
    if (length(name_match_indices > 1)) {
      for (j in name_match_indices) {
        # print(j) print(HGNC$NAME[j])
        alias_match_indices <-
          get_match_indices(ls_sp1, ls_sp2, HGNC$ALIAS_NAME[j],
                            i)
        if (length(alias_match_indices) == 1) {
          return(j)
        } else {
          prev_match_indices <-
            get_match_indices(ls_sp1, ls_sp2, HGNC$PREV_NAME[j],
                              i)
          if (length(prev_match_indices) == 1) {
            return(j)
          }
        }
      }
    }
  }
  return(NA)
}

get.hgnc.id <- function(HGNC, pattern) {
  d <- HGNC %>%
    filter(grepl(x = SYMBOL, pattern = paste0("^", pattern, "$"))) %>%
    select(HGNC_ID) %>%
    pull()
  # print(d)
  if (length(d) > 1) {
    return("")
  }
  if (length(d) == 0) {
    return("")
  }
  return(d)
}

# helper function to recursively regex find alternate kinase names (return only
# one value
regex_iterate_match <- function(pattern) {
  colnames <-
    c("SYMBOL",
      "ALIAS_SYMBOL",
      "PREV_SYMBOL",
      "NAME",
      "PREV_NAME",
      "ALIAS_NAME")
  rtn <- NA
  nbr_rows <- -1
  cnt <- 1
  nbr_of_fields <- length(colnames)
  pattern = paste0("(^|\\|)", pattern, "(\\||$)")
  
  while (nbr_rows != 1) {
    hgnc_df <- HGNC %>%
      filter(grepl(
        x = !!as.name(colnames[cnt]),
        pattern = pattern,
        perl = T,
        ignore.case = T
      ))
    
    nbr_rows <- nrow(hgnc_df)
    if (nbr_rows == 1) {
      return(hgnc_df$HGNC_ID)
    }
    if (cnt == nbr_of_fields) {
      break
    }
    cnt <- cnt + 1
  }
  return(rtn)
}

clean_kinase_data <-
  function(HGNC,
           kinasedata,
           manual_map,
           kinasefilter = "") {
    if ("PCT_INHIBITION_AVG" %in% names(kinasedata)) {
      kinasedata <- kinasedata %>%
        mutate(Result = PCT_INHIBITION_AVG, Kinase = KINASE) %>%
        select(Kinase, Result)
    }
    
    reactive_data$ht <- HGNC %>%
      select(SYMBOL_UPPER, HGNC_ID, SYMBOL)


    df <- kinasedata %>%
      # arrange(Kinase) %>%
      filter(
        !grepl(pattern = "\\[", x = Kinase),
        !grepl(pattern = "_", x = Kinase),
        !grepl(
          pattern = "^(?!.*\\/).*\\(.*\\)$",
          x = Kinase,
          perl = T
        )) %>%
      # mutate(Kinase = ifelse(Kinase %in% cofactors, strsplit(Kinase, "-")[[1]][1], Kinase)) %>%
      mutate(Kinase = ifelse(Kinase %in% cofactors, str_replace(Kinase, "-", "_"), Kinase))

    # check for co-expressions
    df_subset_coexp <- df %>% filter(grepl(paste(coexpressions, collapse = "|"), Kinase)) %>%
      separate_rows(Kinase, sep = '-') %>%
      distinct(Result, Kinase, .keep_all = T)

    if (nrow(df_subset_coexp) > 0) {
      df <- bind_rows(df_subset_coexp, df %>% filter(!grepl(pattern = '-', x = Kinase)))
    }

    df <- df %>%
      mutate(
        No_Greek = case_when(
          grepl(pattern = "α", Kinase) ~ gsub("α", "A", Kinase),
          grepl(pattern = "β", Kinase) ~ gsub("β", "B", Kinase),
          grepl(pattern = "γ",
                Kinase) ~ gsub("γ", "G", Kinase),
          grepl(pattern = "δ", Kinase) ~ gsub("δ",
                                              "d", Kinase),
          grepl(pattern = "ᐃ", Kinase) ~ gsub("ᐃ", "D", Kinase),
          grepl(pattern = "ε", Kinase) ~ gsub("ε", "E", Kinase),
          grepl(pattern = "ζ",
                Kinase) ~ gsub("ζ", "Z", Kinase),
          grepl(pattern = "η", Kinase) ~ gsub("η",
                                              "H", Kinase),
          grepl(pattern = "θ", Kinase) ~ gsub("θ", "Q", Kinase),
          grepl(pattern = "ι", Kinase) ~ gsub("ι", "I", Kinase),
          TRUE ~ Kinase
        )
      ) %>%
      mutate(
        No_Space = str_replace(No_Greek, " ", "_"),
        Just_Kinase = sub("_.*",
                          "", No_Space),
        Just_Kinase = stringr::str_to_upper(Just_Kinase)
      ) %>%
      separate(
        No_Space,
        into = c("sp1", "sp2", "sp3"),
        extra = "drop",
        remove = FALSE,
        fill = "right"
      ) %>%
      merge(
        reactive_data$ht,
        by.x = "Just_Kinase",
        by.y = "SYMBOL_UPPER",
        all.x = TRUE,
        all.y = FALSE
      ) %>%
      arrange(desc(Result))
    
    # find rows that have multiple split strings & add index column for
    # counting primarily aiming for strings like, 'CAPK/ALK3'
    dtt <- df %>%
      filter(!is.na(sp2) | !is.na(sp3), is.na(HGNC_ID)) %>%
      mutate(INDEX = row_number())
    
    # write.csv(dtt, '/Users/spencer.trinhkinnate.com/Downloads/output.csv')
    for (i in seq(nrow(dtt))) {
      p1 = dtt %>%
        filter(INDEX == i) %>%
        select(sp1) %>%
        pull()
      x1 = get.hgnc.id(HGNC, p1)
      if (x1 == "") {
        p2 = dtt %>%
          filter(INDEX == i) %>%
          select(sp2) %>%
          pull()
        x2 = get.hgnc.id(HGNC, p2)
        if (x2 == "") {
          x1 <- regex_iterate_match(p1)
          if (is.na(x1)) {
            x1 = regex_iterate_match(p2)
          }
        } else {
          x1 = rlang::duplicate(x2, shallow = F)
        }
      }
      if (!is.na(x1)) {
        tmp.symbol <- HGNC %>%
          filter(HGNC_ID == x1) %>%
          select(SYMBOL) %>%
          pull()
        df <- df %>%
          mutate(
            HGNC_ID = replace(HGNC_ID, Result == dtt$Result[i], x1),
            SYMBOL = replace(SYMBOL,
                             Result == dtt$Result[i], tmp.symbol)
          )
      }
    }
    
    # assign Aurora A/B/C given that it has 2 elements after splitting, i.e.
    # Aurora A
    dt <- df %>%
      filter(is.na(SYMBOL),!is.na(sp2), nchar(sp2) < 2)  #, !grepl('\\(', Kinase))
    loop_num <- nrow(dt)
    if (loop_num > 0) {
      for (i in seq(1, loop_num)) {
        idx <-
          map_regex_match(dt, HGNC[, .(NAME, ALIAS_NAME, PREV_NAME)], i)
        if (!is.na(idx)) {
          df <- df %>%
            mutate(
              HGNC_ID = replace(HGNC_ID, Result == dt$Result[i], HGNC$HGNC_ID[idx]),
              SYMBOL = replace(SYMBOL, Result == dt$Result[i], HGNC$SYMBOL[idx])
            )
        }
      }
    }
    
    df_sym_nomatch <- df %>%
      filter(is.na(HGNC_ID))
    
    df_sym_match <- df %>%
      filter(!is.na(HGNC_ID))
    
    SEC_HGNC <- HGNC %>%
      separate_rows(ALIAS_SYMBOL, sep = "\\|") %>%
      separate_rows(PREV_SYMBOL, sep = "\\|") %>%
      mutate(
        ALIAS_UPPER = stringr::str_to_upper(ALIAS_SYMBOL),
        PREV_UPPER = stringr::str_to_upper(PREV_SYMBOL)
      )
    
    df_sym_nomatch <- df_sym_nomatch %>%
      mutate(HGNC_ID = NULL, SYMBOL = NULL)
    
    df_alias_match <-
      merge(
        df_sym_nomatch,
        SEC_HGNC,
        by.x = "Just_Kinase",
        by.y = "ALIAS_UPPER",
        all.x = T,
        all.y = F
      )
    
    df_alias_nomatch <- df_alias_match %>%
      filter(is.na(HGNC_ID)) %>%
      mutate(HGNC_ID = NULL, SYMBOL = NULL)
    
    df_alias_match <- df_alias_match %>%
      filter(!is.na(HGNC_ID))
    
    df_prev_match <-
      merge(
        df_alias_nomatch,
        SEC_HGNC,
        by.x = "Just_Kinase",
        by.y = "PREV_UPPER",
        all.x = T,
        all.y = F
      )
    
    df_prev_nomatch <- df_prev_match %>%
      filter(is.na(HGNC_ID)) %>%
      mutate(HGNC_ID = NULL, SYMBOL = NULL)
    
    df_prev_match <- df_prev_match %>%
      filter(!is.na(HGNC_ID))
    
    df_mnl_match <-
      merge(
        df_prev_nomatch,
        manual_map[, .(UPPER_KINASE, HGNC_ID,
                       SYMBOL = GENE_SYMBOL)],
        by.x = "Just_Kinase",
        by.y = "UPPER_KINASE",
        all.x = TRUE,
        all.y = FALSE
      )
    
    # Stack the matched table back into one.
    l = list(df_sym_match, df_alias_match, df_prev_match, df_mnl_match)
    matches <- rbindlist(l, use.names = TRUE, fill = TRUE) %>%
      arrange(HGNC_ID)
    renamed <- unique(matches)
    
    # Keep just a few columns.
    kinome_data <- renamed %>%
      select(SYMBOL, Just_Kinase, No_Space, Result) %>%
      mutate(HGNC_SYMBOL = SYMBOL, CRO_Kinase = No_Space) %>%
      select(-No_Space) %>%
      arrange(HGNC_SYMBOL)
    
    # Tag each result with a bin value.
    kinome_data <- kinome_data %>%
      mutate(
        bin_25 = case_when(
          Result <= 25 ~ "25",
          Result <= 50 ~ "50",
          Result <=
            75 ~ "75",
          Result <= 90 ~ "90",
          Result > 90 ~ "100"
        ),
        bin_10 = case_when(
          Result <=
            10 ~ "10",
          Result <= 20 ~ "20",
          Result <= 30 ~ "30",
          Result <= 40 ~ "40",
          Result <= 50 ~ "50",
          Result <= 60 ~ "60",
          Result <= 70 ~ "70",
          Result <=
            80 ~ "80",
          Result <= 90 ~ "90",
          Result > 90 ~ "100"
        ),
        bin_25_range = case_when(
          bin_25 ==
            "25" ~ "0-25",
          bin_25 == "50" ~ "26-50",
          bin_25 == "75" ~ "51-75",
          bin_25 ==
            "90" ~ "76-90",
          bin_25 == "100" ~ "90-100"
        ),
        bin_10_range = case_when(
          bin_10 ==
            "10" ~ "0-10",
          bin_10 == "20" ~ "11-20",
          bin_10 == "30" ~ "21-30",
          bin_10 ==
            "40" ~ "31-40",
          bin_10 == "50" ~ "41-50",
          bin_10 == "60" ~ "51-60",
          bin_10 ==
            "70" ~ "61-70",
          bin_10 == "80" ~ "71-80",
          bin_10 == "90" ~ "81-90",
          bin_10 ==
            "100" ~ "91-100"
        )
      ) %>%
      arrange(desc(Result), SYMBOL) %>%
      distinct(SYMBOL, .keep_all = T) %>%
      filter(Just_Kinase != kinasefilter)
    
    # check for C-MER (one-off situation)
    if (any(grepl(
      x = kinome_data$Just_Kinase,
      pattern = "c-mer",
      ignore.case = T
    ))) {
      kinome_data <- kinome_data %>%
        mutate(
          SYMBOL = case_when(Just_Kinase == "C-MER" ~ "MERTK", TRUE ~ SYMBOL),
          HGNC_SYMBOL = SYMBOL
        )
      
    }
    
    # write.csv(df, '/Users/spencer.trinhkinnate.com/Downloads/output.csv')
    return(kinome_data)
  }


# HANDLE MUTANT KINASES WITH '( )'
#---------------------------------------------------------------------------

# helper function to recursively regex find alternate kinase names
regex_alias <- function(the_string) {
  rtn_c <- c("NA", "NA")
  if (the_string == "NA") {
    return(rtn_c)
  }
  pattern <- paste0("(?<![A-Za-z])", the_string, "(?![A-Za-z])")
  colnames <-
    c("SYMBOL",
      "ALIAS_SYMBOL",
      "PREV_SYMBOL",
      "NAME",
      "PREV_NAME",
      "ALIAS_NAME")
  nbr_rows <- -1
  cnt <- 1
  nbr_of_fields <- length(colnames)
  
  while (nbr_rows != 1) {
    hgnc_df <- HGNC %>%
      filter(grepl(
        x = !!as.name(colnames[cnt]),
        pattern = pattern,
        perl = T,
        ignore.case = T
      ))
    
    nbr_rows <- nrow(hgnc_df)
    if (cnt == nbr_of_fields) {
      break
    }
    cnt <- cnt + 1
  }
  
  
  if (nrow(hgnc_df) != 0) {
    rtn_c <- c(hgnc_df$SYMBOL, hgnc_df$HGNC_ID)
  }
  return(rtn_c)
}



# find all mutant names and clean up data
clean_mutkinase_data <-
  function(HGNC,
           kinasedata,
           manual_map,
           cutoff = 50,
           kinasefilter = "") {
    kinasedata <- kinasedata %>%
      filter(grepl(KINASE, pattern = "[_-]"),!is.na(KINASE)) %>%
      separate(
        KINASE,
        into = c("KINASE_SP1", "MUTANT_SP2", "XTRA_SP3"),
        remove = F,
        extra = "drop"
      ) %>%
      rename(Result = PCT_INHIBITION_AVG) %>%
      mutate(
        KINASE_NAME = case_when(
          nchar(KINASE_SP1) < 2 ~ stringr::str_to_upper(MUTANT_SP2),
          TRUE ~ KINASE_SP1
        ),
        KINASE_NAME = case_when(
          grepl(
            x = KINASE_NAME,
            pattern = "aurora",
            ignore.case = T
          ) ~ paste0(KINASE_NAME, ".{1,}", MUTANT_SP2),
          grepl(
            x = KINASE_NAME,
            pattern = "p38a",
            ignore.case = T
          ) ~ "MAPK14",
          grepl(
            x = KINASE_NAME,
            pattern = "CK1epsilon",
            ignore.case = T
          ) ~ "CSNK1E",
          TRUE ~ KINASE_NAME
        )
      ) %>%
      merge(
        reactive_data$ht,
        by.x = "KINASE_NAME",
        by.y = "SYMBOL_UPPER",
        all.x = TRUE,
        all.y = FALSE
      ) %>%
      filter(Result >= as.integer(cutoff)) %>%
      arrange(KINASE)
    
    if (nrow(kinasedata) < 1) {
      return()
    }
    
    dtmp <- kinasedata %>%
      mutate(KINASE_CHECK = case_when(is.na(HGNC_ID) ~ KINASE_NAME, TRUE ~ "NA")) %>%
      select(KINASE_CHECK)
    
    # print(paste('DTMP', paste0(rep('-',20), collapse=''))) print(dtmp)
    
    kinase_check <- lapply(dtmp$KINASE_CHECK, regex_alias)
    kinasedata <- kinasedata %>%
      mutate(KINASE_CHECK = kinase_check)
    
    transpose_colm <- t(data.frame(kinasedata$KINASE_CHECK))
    
    rename_col1 <- "1"
    rename_col2 <- "2"
    kinasedata <- cbind(kinasedata, transpose_colm)
    
    if (any(grepl(
      pattern = "V\\d{1}",
      x = colnames(kinasedata),
      perl = T
    ))) {
      rename_col1 <- "V1"
      rename_col2 <- "V2"
    }
    
    kinasedata <- kinasedata %>%
      rename(
        SYMBOL_CHECK = !!as.name(rename_col1),
        HGNC_ID_CHECK = !!as.name(rename_col2)
      ) %>%
      mutate(
        bin_25 = case_when(
          Result <= 25 ~ "25",
          Result <= 50 ~ "50",
          Result <=
            75 ~ "75",
          Result <= 90 ~ "90",
          Result > 90 ~ "100"
        ),
        bin_10 = case_when(
          Result <=
            10 ~ "10",
          Result <= 20 ~ "20",
          Result <= 30 ~ "30",
          Result <= 40 ~ "40",
          Result <= 50 ~ "50",
          Result <= 60 ~ "60",
          Result <= 70 ~ "70",
          Result <=
            80 ~ "80",
          Result <= 90 ~ "90",
          Result > 90 ~ "100"
        )
      ) %>%
      arrange(desc(Result), SYMBOL) %>%
      filter(!grepl(x = KINASE, pattern = kinasefilter)) %>%
      mutate(
        HGNC_ID = case_when(is.na(HGNC_ID) ~ HGNC_ID_CHECK, TRUE ~ HGNC_ID),
        SYMBOL = case_when(is.na(SYMBOL) ~ SYMBOL_CHECK, TRUE ~ SYMBOL)
      )
    
    kinasedata <- kinasedata %>%
      filter(!is.na(HGNC_ID)) %>%
      arrange(desc(Result))
    
    missing_kinasedata <- kinasedata %>%
      filter(is.na(HGNC_ID)) %>%
      arrange(desc(Result))
    
    print(paste("KINASEDATA DATAFRAME", paste0(rep("-", 20), collapse = "")))
    print(head(kinasedata))
    # print(head(missing_kinasedata))
    
    return(list(kinasedata = kinasedata, missing_kinasedata = missing_kinasedata))
  }
