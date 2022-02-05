library(readxl)
library(writexl)
library(data.table)
library(stringr)
library(dplyr)
library(textclean)
library(tidyr)

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
                       ".*", sp2, ")"
                     )
    match_indices <- grep(pattern = pattern,
                          ignore.case = F, 
                          x = match_values 
                         )
    # print(paste(i, ":", match_indices))
    return(match_indices)
    }

# main call to loop thru indices for a 1 index match
map_regex_match <- function(df, HGNC, i) {
    ls_sp1 <- df$sp1
    ls_sp2 <- df$sp2
    name_match_indices <- get_match_indices(ls_sp1, ls_sp2, HGNC$NAME, i)
    if (length(name_match_indices) == 1) {
        return(name_match_indices)
    } else {
        if (length(name_match_indices > 1)) {
            for (j in name_match_indices) {
                # print(j)
                # print(HGNC$NAME[j])
                alias_match_indices <- get_match_indices(ls_sp1, ls_sp2, HGNC$ALIAS_NAME[j], i)
                if (length(alias_match_indices) == 1) {
                    return(j)
                } else {
                    prev_match_indices <- get_match_indices(ls_sp1, ls_sp2, HGNC$PREV_NAME[j], i)
                    if (length(prev_match_indices) == 1) {
                        return(j)
                    }
                }
            }
        }
    }
    return(NA)
}


clean_kinase_data <- function(HGNC, kinasedata, manual_map) {

				ht <- HGNC %>%
						select(SYMBOL_UPPER, HGNC_ID, SYMBOL) 

				df <- kinasedata %>% 
						arrange(Kinase) %>%
						filter(!grepl(pattern = "\\[", x = Kinase), 
									 !grepl(pattern = "/", x = Kinase),
									 !grepl(pattern = "-", x = Kinase)) %>%
						mutate(No_Greek =  case_when(
							Kinase == "α" ~ "A",
							Kinase == "β" ~ "B",
							Kinase == "γ" ~ "G",
							Kinase == "δ" ~ "D",
							Kinase == "ε" ~ "E",
							Kinase == "ζ" ~ "Z",
							Kinase == "η" ~ "H", 
							Kinase == "θ" ~ "Q",
							Kinase == "ι" ~ "I",
							TRUE ~ Kinase
								)              
						) %>%
						mutate(No_Space = str_replace(No_Greek, " ", "_"),
									 Just_Kinase = sub("_.*", "", No_Space),
									 Just_Kinase = stringr::str_to_upper(Just_Kinase)) %>%
						separate(No_Space, into = c('sp1', 'sp2', 'sp3'), extra = 'drop', remove = FALSE, fill="right") %>%
						merge(ht, by.x = 'Just_Kinase', by.y = 'SYMBOL_UPPER', all.x = TRUE, all.y = FALSE)

				# assign Aurora A/B/C given that it has 2 elements after splitting, i.e. Aurora A
				dt <- df %>% filter(is.na(SYMBOL), !is.na(sp2), nchar(sp2) < 2) #, !grepl("\\(", Kinase)) 
				loop_num <- nrow(dt)
				for (i in seq(1, loop_num)) {
						idx <- map_regex_match(dt, HGNC[, .(NAME, ALIAS_NAME, PREV_NAME)], i)
						if (!is.na(idx)) {
								df <- df %>% mutate(HGNC_ID = replace(HGNC_ID, 
																											Result == dt$Result[i], 
																											HGNC$HGNC_ID[idx]),
																		SYMBOL = replace(SYMBOL,
																										 Result == dt$Result[i],
																										 HGNC$SYMBOL[idx])
																	 ) 
								}
				}

				df_sym_nomatch <- df %>% 
						filter(is.na(HGNC_ID))

				df_sym_match <- df %>% 
						filter(!is.na(HGNC_ID))  

				SEC_HGNC <- HGNC %>% separate_rows(ALIAS_SYMBOL, sep = "\\|") %>% 
														separate_rows(PREV_SYMBOL, sep = "\\|") %>%
														mutate(ALIAS_UPPER = stringr::str_to_upper(ALIAS_SYMBOL), 
																	 PREV_UPPER = stringr::str_to_upper(PREV_SYMBOL))

				df_sym_nomatch <- df_sym_nomatch %>%
												mutate(HGNC_ID = NULL,
															 SYMBOL = NULL)

				df_alias_match <- merge(df_sym_nomatch, 
																SEC_HGNC, 
																by.x = 'Just_Kinase', 
																by.y = 'ALIAS_UPPER', 
																all.x = T, 
																all.y = F
																)

				df_alias_nomatch <- df_alias_match %>%
                    filter(is.na(HGNC_ID)) %>%
                    mutate(HGNC_ID = NULL, 
                           SYMBOL = NULL)

				df_alias_match <- df_alias_match %>%
														filter(!is.na(HGNC_ID))

				df_prev_match <- merge(df_alias_nomatch,
                    SEC_HGNC, 
                    by.x = 'Just_Kinase', 
                    by.y = 'PREV_UPPER', 
                    all.x = T, 
                    all.y = F
                    )
				
				df_prev_nomatch <- df_prev_match %>%
                        filter(is.na(HGNC_ID)) %>%
                        mutate(HGNC_ID = NULL, 
                               SYMBOL = NULL)
                               
				df_prev_match <- df_prev_match %>%
																filter(!is.na(HGNC_ID))
				
				df_mnl_match <- merge(df_prev_nomatch, 
                      manual_map[,.(UPPER_KINASE, HGNC_ID, SYMBOL=GENE_SYMBOL)], 
                      by.x = 'Just_Kinase', 
                      by.y = 'UPPER_KINASE', 
                      all.x = TRUE, 
                      all.y = FALSE
                     )

				# Stack the matched table back into one.
				l = list(df_sym_match, df_alias_match, df_prev_match, df_mnl_match)
				matches <- rbindlist(l, use.names=TRUE, fill = TRUE) %>%
												arrange(HGNC_ID)
				renamed <- unique(matches)

				# Keep just a few columns.
				kinome_data <- renamed %>%
                    select(SYMBOL, Just_Kinase, Result) %>%
                    mutate(HGNC_SYMBOL=SYMBOL, CRO_Kinase=Just_Kinase) %>%
                    arrange(HGNC_SYMBOL)

				kinome_data <- kinome_data %>%
										arrange(desc(Result))

				# Tag each result with a bin value.
				kinome_data <- kinome_data %>% mutate(bin_25 = case_when(Result <= 25 ~ "25", Result <=  50 ~ "50", Result <= 75 ~ "75", Result <= 90 ~ "90", Result > 90 ~ "100"))
				kinome_data <- kinome_data %>% mutate(bin_10 = case_when(Result <= 10 ~ "10", Result <=  20 ~ "20", Result <= 30 ~ "30", 
																																	 Result <= 40 ~ "40", Result <=  50 ~ "50", Result <= 60 ~ "60", 
																																	 Result <= 70 ~ "70", Result <=  80 ~ "80", Result <= 90 ~ "90", 
																																	 Result > 90 ~ "100"))
				kinome_data <- kinome_data %>% 
												arrange(desc(Result), SYMBOL) %>%
												distinct(SYMBOL, .keep_all = T)
												
				print(head(kinome_data))

				return(kinome_data)
}
