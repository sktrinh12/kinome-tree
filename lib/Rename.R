library(readxl)
library(writexl)
library(data.table)
library(stringr)
library(dplyr)
library(textclean)
library(tidyr)

clean_kinase_data <- function(HGNC, kinasedata, manual_map) {
  
  HGNC[, SYMBOL_UPPER := stringr::str_to_upper(SYMBOL)]
  

  setkey(kinasedata, "Kinase")
  
  # If the Kinase does not have an underscore append an "_Km" to the end.
  kinasedata[str_count(Kinase, "_")==0, Kinase := paste0(Kinase,"_Km")]
  
  # Remove rows with a bracket ( [ ),forward slash ( / ) or hyphen (-).
  kinasedata <- dplyr::filter(kinasedata, !grepl("\\[", Kinase))
  kinasedata <- dplyr::filter(kinasedata, !grepl("/", Kinase))
  kinasedata <- dplyr::filter(kinasedata, !grepl("-", Kinase))
  
  # Replace greek symbols with their upper case ASCII equivalent.
  kinasedata[, No_Greek := str_replace(Kinase, "α", "A")]
  kinasedata[, No_Greek := str_replace(No_Greek, "β", "B")]
  kinasedata[, No_Greek := str_replace(No_Greek, "γ", "G")]
  kinasedata[, No_Greek := str_replace(No_Greek, "δ", "D")]
  kinasedata[, No_Greek := str_replace(No_Greek, "ε", "E")]
  kinasedata[, No_Greek := str_replace(No_Greek, "ζ", "Z")]
  kinasedata[, No_Greek := str_replace(No_Greek, "η", "H")]
  kinasedata[, No_Greek := str_replace(No_Greek, "θ", "Q")]
  kinasedata[, No_Greek := str_replace(No_Greek, "ι", "I")]
  
  kinasedata[, No_space := str_replace(No_Greek, " ", "_")]
  
  ### get the Kinase from the front.
  kinasedata[, Just_Kinase := sub("_.*", "", No_space)]
  
  ### Get ATP value from the end.
  kinasedata[, ATP := stringi::stri_reverse(sub("_.*", "", stringi::stri_reverse(No_space)))]
  
  # First round of trying to match the CRO kinase names to the HGNC name. Match to the HGNC Symbol
  kinasedata[, UPPER := stringr::str_to_upper(Just_Kinase)]
  KIN04_symbol_match <- merge(kinasedata, HGNC[,.(SYMBOL_UPPER, HGNC_ID, SYMBOL)], by.x = 'UPPER', by.y = 'SYMBOL_UPPER', all.x = TRUE, all.y = FALSE)
  KIN04_symbol_nomatch <- KIN04_symbol_match[is.na(HGNC_ID)]
  KIN04_symbol_match <- na.omit(KIN04_symbol_match, cols = "HGNC_ID")
  
  # Second round of trying to match the unmatched CRO kinase names to the HGNC name. Match to any HGNC Alias Symbol.
  TALL_HGNC <- HGNC %>% separate_rows(ALIAS_SYMBOL, sep = "\\|")
  TALL_HGNC <- TALL_HGNC %>% separate_rows(PREV_SYMBOL, sep = "\\|")
  setDT(TALL_HGNC)
  TALL_HGNC[, ALIAS_UPPER := stringr::str_to_upper(ALIAS_SYMBOL)]
  TALL_HGNC[, PREV_UPPER := stringr::str_to_upper(PREV_SYMBOL)]
  KIN04_symbol_nomatch[,HGNC_ID:=NULL]
  KIN04_symbol_nomatch[,SYMBOL:=NULL]
  KIN04_alias_match <- merge(KIN04_symbol_nomatch, TALL_HGNC[,.(ALIAS_UPPER, HGNC_ID, SYMBOL)], by.x = 'UPPER', by.y = 'ALIAS_UPPER', all.x = TRUE, all.y = FALSE)
  KIN04_alias_nomatch <- KIN04_alias_match[is.na(HGNC_ID)]
  KIN04_alias_match <- na.omit(KIN04_alias_match, cols = "HGNC_ID")
  
  # Third round of trying to match the unmatched CRO kinase names to the HGNC name. Match to any HGNC Previous Symbol.
  KIN04_alias_nomatch[,HGNC_ID:=NULL]
  KIN04_alias_nomatch[,SYMBOL:=NULL]
  KIN04_prev_match <- merge(KIN04_alias_nomatch, TALL_HGNC[,.(PREV_UPPER, HGNC_ID, SYMBOL)], by.x = 'UPPER', by.y = 'PREV_UPPER', all.x = TRUE, all.y = FALSE)
  KIN04_prev_nomatch <- KIN04_prev_match[is.na(HGNC_ID)]
  KIN04_prev_match <- na.omit(KIN04_prev_match, cols = "HGNC_ID")
  
  # Manually mapped the left over unmatched.  TODO:(This needs to be improved upon.)
  KIN04_prev_nomatch[,HGNC_ID:=NULL]
  KIN04_prev_nomatch[,SYMBOL:=NULL]
  KIN04_manual_match <- merge(KIN04_prev_nomatch, manual_map[,.(UPPER_KINASE, HGNC_ID, SYMBOL=GENE_SYMBOL)], by.x = 'UPPER', by.y = 'UPPER_KINASE', all.x = TRUE, all.y = FALSE)
  
  # Stack the matched table back into one.
  l = list(KIN04_symbol_match, KIN04_alias_match, KIN04_prev_match, KIN04_manual_match)
  matches <- rbindlist(l, use.names=TRUE, fill = TRUE)
  setkey(matches, "HGNC_ID")
  renamed <- unique(matches)

  # Keep just a few columns.
  kinome_data <- renamed[, .(HGNC_Symbol=SYMBOL, CRO_Kinase=Just_Kinase, ATP, Result)]
  
  # Order the table by the kinase symbol.
  setorder(kinome_data, HGNC_Symbol)
  
  # Tag each result with a bin value.
  kinome_data <- kinome_data %>% mutate(bin_25 = case_when(Result <= 25 ~ "25", Result <=  50 ~ "50", Result <= 75 ~ "75", Result <= 90 ~ "90", Result > 90 ~ "100"))
  kinome_data <- kinome_data %>% mutate(bin_10 = case_when(Result <= 10 ~ "10", Result <=  20 ~ "20", Result <= 30 ~ "30", 
                                                           Result <= 40 ~ "40", Result <=  50 ~ "50", Result <= 60 ~ "60", 
                                                           Result <= 70 ~ "70", Result <=  80 ~ "80", Result <= 90 ~ "90", 
                                                           Result > 90 ~ "100"))
  return(data.table(kinome_data))

}

