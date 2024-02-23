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

# TODO Check for normalize_conc_units, normalize_boolean
# TODO Update all functions to find the best place to return out$raw, before its NULLed
debug_list = list(
    normalize_weight = paste0("SELECT distinct weight, weight_units, weight_kg ",
                                "FROM subjects WHERE weight_kg is null"),
    normalize_height = paste0("SELECT distinct height, height_units ",
                                "FROM subjects WHERE height_cm is null"),
    #normalize_age = paste0("SELECT distinct age, age_units ",
    #                       "FROM subjects WHERE age_normalized is null"),
    normalize_time = paste0("SELECT distinct a.time_original, b.time_units_original ",
                            "FROM conc_time_values a
                             LEFT JOIN series b
                             ON b.id = a.fk_series_id
                             WHERE a.time_hr is null"),
    #normalize_dose = paste0("SELECT distinct dose_level_original, dose_level_original_units ",
    #                        "FROM studies WHERE dose_level_normalized is null"),
    normalize_conc = paste0("SELECT distinct b.id,
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
                              WHERE conc is null")
  )

f = "debug_file"
log_path = "output/debug_log.xlsx"

out_list = list()

# Loop through each module
for(module in names(debug_list)){
  if (is.na(debug_list[[module]])){
    next
  }
  in_data = query_db(query = debug_list[[module]], 
                         con_type = "postgres", 
                         schema = "cvt")
  
  # TODO Return the out$raw "unhandled" cases
  module_fun = match.fun(module)
  out = module_fun(raw=in_data, f=f, log_path=log_path, debug=TRUE)
  
  # TODO Generate a summary by module with unhandled units
  
  # TODO Store out for later XLSX output
}

# TODO output XLSX from dataframe list
# writexl::write_xlsx()