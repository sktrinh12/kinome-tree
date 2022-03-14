# use of JDBC to connect to Oracle database, ensure odjc.jar exists
# also ensure access parameter file exists

library(rJava)
library(RJDBC)


username <- Sys.info()[["effective_user"]]
root_path <- "/Users"

if (Sys.info()[["sysname"]] == "Linux") {root_path <- "/home"}

ojdbc_fp <- file.path(root_path, username, "lib/ojdbc8")
ojdbc_jarfile <- file.path(ojdbc_fp, "ojdbc8.jar")

# parse access param file
access_file <- file.path(ojdbc_fp, "oracle")
access_file <- file(access_file, open = "r")

read_acess_values <- function(access_file) {
  ls_vars <- list()
  lines = readLines(access_file)
  for (i in 1:length(lines)) {
    splt_str <- strsplit(x = lines[i], split = ",")[[1]]
    var_name <- splt_str[1]
    var_value <- splt_str[2]
    ls_vars[[var_name]] <- var_value
  }
  close(access_file)
  return(ls_vars)
}

jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", 
                   classPath=ojdbc_jarfile)

ls_vars <- read_acess_values(access_file)

connect_string <- paste0("jdbc:oracle:thin:@",
                         ls_vars$host,
                         ":", 
                         ls_vars$port, 
                         ":", 
                         ls_vars$sid
                         )

print(connect_string)

# userdata schema connection
conn <- dbConnect(jdbcDriver, 
                 connect_string,
                 ls_vars$username_userdata,
                 ls_vars$password_userdata
								)

# sql query string to get kinase panel data
kin_panel_qstr <- "SELECT * FROM (
			SELECT  
				BATCH_ID,
				REGEXP_SUBSTR (BATCH_ID, '[^-]+', 1, 2)    AS BATCH_NUM,
				REGEXP_SUBSTR (BATCH_ID, '[^-]+', 1, 1)    AS COMPD_ID,
				CONC,
				EXPERIMENT_ID,
				KINASE,
				PCT_INHIBITION_AVG,
				TECHNOLOGY
			FROM    FT_KINASE_PANEL
			) WHERE COMPD_ID = '"

# generic helper function to grab data from oracle
fetch_data <- function(conn, query_str) {
  query_str <- strwrap(query_str, 
												width= 10000, 
												simplify = TRUE
											)
  d <- dbGetQuery(conn, query_str)
  return(d)
}

# last function that is called when experiment id and optionally technology is passed
fetch_f_kdata<- function(compd_id, exp_id, tech_id=NA) {
  exp_id <- gsub(pattern = " \\(.*$",
								 x = exp_id, 
								 replacement = ""
								)	
	query_str <- paste0(kin_panel_qstr,
								compd_id, 
								"' AND EXPERIMENT_ID = '",
							  exp_id, "'"
							 )
  if (!is.na(tech_id)) {
				query_str <- paste0(query_str, " AND TECHNOLOGY = '", 
											tech_id,"'" 
											)
  }
  d <- fetch_data(conn, query_str) %>%
								arrange(desc(PCT_INHIBITION_AVG))
  return(d)
}

# unfiltered dataframe that has all experiment ids
fetch_uf_kdata <- function(compd_id) {
  query_str <- paste0(kin_panel_qstr, compd_id, "'") 
  d <- fetch_data(conn, query_str)
  return(d)
}

# get meta data from TEST MANAGER EXPERIMENTAL TABLE DATA (CRO)
fetch_exp_mdata <- function(exp_id) {
  query_str <- paste0("SELECT * FROM TM_PROT_EXP_FIELDS_VALUES 
         WHERE EXPERIMENT_ID = '", exp_id, "' 
         AND PROPERTY_NAME = 'CRO'")
  d <- fetch_data(conn, query_str)
  query_str <- paste0("SELECT CONC, ORDER_NUMBER FROM FT_KINASE_PANEL 
         WHERE EXPERIMENT_ID = '", exp_id, "' 
         FETCH NEXT 1 ROWS ONLY")
	# get concentration from kinase panel table
  d_conc_ordernbr <- fetch_data(conn, query_str)
  d$CONC <- d_conc_ordernbr$CONC
  d$ORDER_NUM <- d_conc_ordernbr$ORDER_NUMBER
  return(d)
}

# get unique compound ids
fetch_unq_cmpd_ids <- function() {
				query_str <- "SELECT DISTINCT FORMATTED_ID 
											FROM C$PINPOINT.REG_DATA 
											WHERE FORMATTED_ID LIKE 'FT%'"
				cmpd_ids <- fetch_data(conn, query_str) %>% 
										select(FORMATTED_ID) %>% 
										pull()
				return(cmpd_ids)
}

# get the count of unique compound ids
fetch_len_cmpd_ids <- function() {
				query_str <- "SELECT COUNT(DISTINCT FORMATTED_ID)
										  	FROM C$PINPOINT.REG_DATA
											  WHERE FORMATTED_ID LIKE 'FT%'"
				counts <- fetch_data(conn, query_str) %>% pull()
				return(as.integer(counts))
}
