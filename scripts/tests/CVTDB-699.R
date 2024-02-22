connect_to_db <- function(con_type){
  switch(con_type,
         "postgres" = dbConnect(RPostgreSQL::PostgreSQL(), 
                                user = Sys.getenv("user"), 
                                password = Sys.getenv("pass"), #
                                host = Sys.getenv("host"), #
                                dbname = Sys.getenv("dbname")),
         "mysql" = dbConnect(RMySQL::MySQL(), #Connect to database with .Renviron parameters
                             username = Sys.getenv("user"), 
                             password = Sys.getenv("pass"),
                             host = Sys.getenv("host"), 
                             port = 3306,
                             dbname = Sys.getenv("dbname")),
         'sqlite' = dbConnect(RSQLite::SQLite(), paste0(Sys.getenv("sqlite_dbname"), ".sqlite"))
  ) %>% return()
}

query_db <- function(query=NULL, con_type, schema){
  if(is.null(query)) return(cat("\nMust provide a query to send"))
  con = connect_to_db(con_type)
  
  query_result = tryCatch({
    if(con_type == "postgres"){# Add schema tag
      return(dbGetQuery(con, query %>% 
                          gsub("FROM ", paste0("FROM ",schema,"."), .) %>%
                          gsub("JOIN ", paste0("JOIN ",schema,"."), .)
      ))
    } else {
      return(dbGetQuery(con, query))
    }
  },
  error=function(cond){ cat(paste0("\nError message: ", cond)); return(NULL) },
  finally={ dbDisconnect(con) })
  
  return(query_result)
}

query <- "SELECT distinct b.id,
  a.conc_units_original, a.fk_conc_medium_id,
  b.conc_original, b.conc, b.conc_sd_original, b.conc_lower_bound_original, b.conc_upper_bound_original,
  c.chemical_name_original, c.chemical_name_secondary_original, c.casrn_original, c.dsstox_substance_id,
  d.conc_medium_normalized
  FROM series a
  LEFT JOIN conc_time_values b
  ON a.id = b.fk_series_id
  LEFT JOIN chemicals c
  ON c.id = a.fk_analyzed_chemical_id
  LEFT JOIN conc_medium_dict d
  ON d.id = a.fk_conc_medium_id
  LIMIT 50000" # XLSX Error: Error in libxlsxwriter: 'String exceeds Excel's limit of 32,767 characters.

df_raw <- query_db(query = query, 
                   con_type = "postgres", 
                   schema = "cvt")

# Preserve previous conversion value
df_raw$conc_old <- df_raw$conc
# Normalization requires conc_medium field
df_raw$conc_medium <- df_raw$conc_medium_normalized

# Normalization does not run with NA conc_original values
df_raw <- df_raw %>% filter(!is.na(conc_original))

# Run "normalize_conc" on the data
log_path <- "output/template_normalization_log.xlsx"
df_normalized <- normalize_conc(df_raw, "test", log_path)

# Filter to those where "conc_old" does not equal the new "conc"
df <- df_normalized %>% filter(xor(is.na(conc_old), is.na(conc))|conc_old != conc)

# TODO Push updated values to Conc_Time_Values sheet based on "id" field