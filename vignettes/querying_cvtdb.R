## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----out.height = "400px", out.width = "400px", echo = FALSE------------------
knitr::include_graphics("images/CvTdb_EER.jpg")

## -----------------------------------------------------------------------------
library(DBI)
library(dplyr)
library(dbplyr)

## -----------------------------------------------------------------------------
#'@description A function to make a connection to the database
#'@param con_type Whether to connect to postgres, mysql, or sqlite version
#'@import DBI RMySQL RSQLite
#'@return Database connection pointer object
connect_to_db <- function(con_type){
  switch(con_type,
         "postgres" = dbConnect(RPostgres::PostgreSQL(), 
                                user = Sys.getenv("postgres_user"), 
                                password = Sys.getenv("postgres_pass"), #
                                host = Sys.getenv("postgres_host"), #
                                dbname = Sys.getenv("postgres_dbname")),
         "mysql" = dbConnect(RMySQL::MySQL(), #Connect to database with .Renviron parameters
                             username = Sys.getenv("mysql_user"), 
                             password = Sys.getenv("mysql_pass"),
                             host = Sys.getenv("mysql_host"), 
                             port = 3306,
                             dbname = Sys.getenv("mysql_dbname")),
         'sqlite' = dbConnect(RSQLite::SQLite(), paste0(Sys.getenv("sqlite_dbname"), ".sqlite"))
  ) %>% return()
}

#'@description A helper function to query database and receive the results. 
#'Handles errors/warnings with tryCatch.
#'@param query A SQL query string to query the database with
#'@param con_type Whether to connect to postgres, mysql, or sqlite version
#'@param schema The schema name to use if using a postgresql connection
#'@import DBI dplyr
#'@return Dataframe of database query results
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

compare_sql_dbplyr <- function(){
  if(exists("df_sql") && exists("df_dbplyr")){
    print(ifelse(identical(df_sql, df_dbplyr), 
                 "SQL and dplyr versions are identical", 
                 "SQL and dplyr versions are NOT identical"))
    }
}

## -----------------------------------------------------------------------------
query <- "SELECT pmid, year, first_author, title FROM documents LIMIT 10"
df_sql <- query_db(query = query, 
               con_type = "postgres", 
               schema = "cvt")
head(df_sql)

## -----------------------------------------------------------------------------

con <- connect_to_db(con_type = "postgres")

# Wrapped in a tryCatch so the connection is always closed in case of error
df_dbplyr <- tryCatch(
  {
    tbl(con, in_schema("cvt", "documents")) %>%
      head(10) %>%
      select(pmid, year, first_author, title) %>%
      collect() %>%
      as.data.frame()
  },
  error=function(e) {
    message(e)
    return(NULL)
  },
  finally={
    dbDisconnect(con)
  }
)

head(df_dbplyr)

## -----------------------------------------------------------------------------
compare_sql_dbplyr()

## -----------------------------------------------------------------------------
# PMID list of interest
pmid_list <- c('43370', '422270', '422271', '538758', '629888', '731724', '734422', '843463', '843464', '926203')
                # Select columns of interest
query <- paste0("SELECT distinct e.pmid, b.analyte_name_original, c.dose_level_original, c.dose_level_original_units, ",
                "d.species, b.conc_medium_original, a.time_original, b.time_units_original, a.conc_original, b.conc_units_original ",
                "FROM conc_time_values a ",
                # Join to series table by series ID
                "LEFT JOIN series b on b.id = a.fk_series_id ",
                # Join to studies table by study ID
                "LEFT JOIN studies c on c.id = b.fk_study_id ",
                # Join to subjects table by subject ID
                "LEFT JOIN subjects d on d.id = b.fk_subject_id ",
                # Join to documents table by extraction document ID
                "LEFT JOIN documents e on c.fk_extraction_document_id = e.id ",
                # Filter to PMID of interest
                "WHERE e.pmid in ('",
                # Using paste instead of toString() for cases where identifiers
                # are alphanumeric strings
                paste0(pmid_list, collapse="', '"),
                "')")
df_sql <- query_db(query = query, 
               con_type = "postgres", 
               schema = "cvt")
head(df_sql)

## -----------------------------------------------------------------------------
con <- connect_to_db(con_type = "postgres")

# Wrapped in a tryCatch so the connection is always closed in case of error
df_dbplyr <- tryCatch(
  {
    # Store table pointers with column name prefixes
    conc = tbl(con, in_schema("cvt", "conc_time_values")) %>% rename_all(function(x) paste0("a.", x))
    series = tbl(con, in_schema("cvt", "series")) %>% rename_all(function(x) paste0("b.", x))
    studies = tbl(con, in_schema("cvt", "studies")) %>% rename_all(function(x) paste0("c.", x))
    subjects = tbl(con, in_schema("cvt", "subjects")) %>% rename_all(function(x) paste0("d.", x))
    docs = tbl(con, in_schema("cvt", "documents")) %>% rename_all(function(x) paste0("e.", x))
    
    out <- conc %>%
      left_join(series, 
                by=c("a.fk_series_id"="b.id")) %>%
      left_join(studies, 
                by=c("b.fk_study_id"="c.id")) %>%
      left_join(subjects, 
                by=c("b.fk_subject_id"="d.id")) %>%
      left_join(docs, 
                by=c("c.fk_extraction_document_id"="e.id")) %>%
      filter(e.pmid %in% pmid_list) %>%
      select(e.pmid, b.analyte_name_original, c.dose_level_original, 
             c.dose_level_original_units, d.species, b.conc_medium_original, 
             a.time_original, b.time_units_original, a.conc_original, b.conc_units_original) %>%
      distinct() %>%
      collect() %>%
      as.data.frame()
    
    # Remove column prefixes
    colnames(out) = sub('.*\\.', '', colnames(out))
    
    out
  
  },
  error=function(e) {
    message(e)
    return(NULL)
  },
  finally={
    dbDisconnect(con)
  }
)

head(df_dbplyr)

## -----------------------------------------------------------------------------
compare_sql_dbplyr()

## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
compare_sql_dbplyr()

