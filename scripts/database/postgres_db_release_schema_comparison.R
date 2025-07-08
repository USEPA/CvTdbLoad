#-------------------------------------------------------------------------------------
#' Function to generate general summaries of the database for a given release version
#' @param in.db The version of db to summarize
#' @param in.host The host of the database to summarize
#' @param compare.db The version of db to compare with the input db version
#' @param compare.host The host of the comparison database
#' @return None. Exported files are generated in a summary folder.
#-------------------------------------------------------------------------------------
postgres_db_release_schema_comparison <- function(in.db, in.host, compare.db, compare.host, in.schema, compare.schema){
  
  # Helper function to check if any field names match sql keywords
  check_keywords <- function(database, schema, words_file, outDir){
    keywords <- stringi::stri_trans_tolower(readLines(words_file, warn=FALSE))
    field_list <- db_query_cvt(paste0("SELECT table_schema, table_catalog, table_name, column_name, udt_name as column_type ",
                                      "FROM INFORMATION_SCHEMA.COLUMNS WHERE table_schema = '",
                                  schema, "' AND table_catalog = '", database, "'"))
    
    occurrences <- field_list %>%
      # Check for exact matches
      dplyr::filter(tolower(column_name) %in% keywords) %>%
      dplyr::mutate(keyword = tolower(column_name))
    
    # Logic for checking for contained keywords (not exact matches)
    #dplyr::mutate(keyword = stringr::str_extract_all(stringi::stri_trans_tolower(column_name), paste(keywords, collapse="|"))) %>%
    #tidyr::unnest(keyword) %>%
    #dplyr::filter(!is.na(keyword))
    
    words_file <- gsub("\\.txt", "", basename(words_file))
    out_file <- paste0(database,"_",words_file,"_keyword_occurrences.xlsx")
    
    writexl::write_xlsx(occurrences, file.path(outDir, out_file))
  }
  
  db_server_list = list(in.db, compare.db)
  names(db_server_list) = c(in.host, compare.host)
  
  db_schema_list = list(in.schema, compare.schema)
  names(db_schema_list) = c(in.db, compare.db)
  
  # Loop through input databases
  for(i in seq_len(length(db_server_list))){
    # Set server host
    db = db_server_list[[i]]
    schema = db_schema_list[[i]]
    
    Sys.setenv(host = names(db_server_list)[i],
               dbname = db)
    
    message("Summarizing '", db, "' from ", Sys.getenv("db_server"))
    
    # Check if input databases exist on server
    tbl_check <- db_query_cvt("select distinct table_catalog from information_schema.columns") %>%
      dplyr::filter(table_catalog %in% db)
    
    if(!db %in% tbl_check$table_catalog){
      stop("Input database does not exist on server...")
    }
    
    # Create subfolders to store outputs by db version
    outDir <- file.path("output/db_release_schema_comparison", Sys.getenv("db_server"), db)
    if(!dir.exists(outDir)) {
      dir.create(outDir, recursive = TRUE)
      # dir.create(file.path(outDir, "DDL"))
    } else {
      # Clear out old files
      unlink(outDir, recursive = TRUE)
      dir.create(outDir, recursive = TRUE)
      # dir.create(file.path(outDir, "DDL"))
    }
    
    reserved_words_file = "output/db_release_schema_comparison/mysql_reserved_terms.txt"
    nonreserved_words_file = "output/db_release_schema_comparison/mysql_non_reserved_terms.txt"
    
    # database, words_file, outDir
    check_keywords(database=db, schema=schema, words_file=reserved_words_file, outDir=outDir)
    check_keywords(database=db, schema=schema, words_file=nonreserved_words_file, outDir=outDir)
    
    # List of tables, fields, and field types
    field_list <- db_query_cvt(paste0("SELECT table_schema, table_catalog, table_name, column_name, column_default, ",
                                      "is_nullable, udt_name as column_type, collation_name ",
                                  "FROM INFORMATION_SCHEMA.COLUMNS WHERE table_schema = '",
                                  schema, "' AND table_catalog = '", db, "'"))
    # Record counts
    # Estimate row count from information schema
    n_tbl_row_est <- db_query_cvt(paste0(
      "SELECT relname as table_name, n_live_tup as n_tbl_row_est ",
      "FROM pg_stat_user_tables ",
      "WHERE schemaname = '", schema,"'"
    ))
    
    # Loop through tables
    n_tbl_row <- lapply(unique(field_list$table_name), function(tbl_n){
      message("...Pulling row counts for '", tbl_n, "'")
      
      # Get accurate row count
      db_query_cvt(paste0("SELECT count(*) as n_tbl_row FROM ", schema, ".", tbl_n)) %>%
        mutate(table_name = tbl_n) %>%
        return()
    }) %>%
      dplyr::bind_rows()
    
    # Combine output
    out = list(
      tbl_list = field_list %>%
        dplyr::group_by(table_schema, table_name) %>%
        dplyr::summarise(n_tbl_columns = dplyr::n(),
                         .groups = "keep") %>%
        dplyr::ungroup() %>%
        dplyr::left_join(n_tbl_row_est,
                         by="table_name") %>%
        dplyr::left_join(n_tbl_row,
                         by="table_name"),
      field_list = field_list
    )
    
    # Export output
    writexl::write_xlsx(out, paste0(outDir, "/", db, "_release_summary_", Sys.Date(), ".xlsx"))
  }
  
  # Perform comparison of table fields
  if(is.null(compare.db)){
    message("No comparison database input...returning...Done...")
  }
  
  #'@description Helper function to read all Excel file sheets
  read_excel_allsheets <- function(filename) {
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    names(x) <- sheets
    return(x)
  }
  
  old <- read_excel_allsheets(paste0(file.path("output/db_release_schema_comparison", compare.host, compare.db),
                                     "/", compare.db, "_release_summary_", Sys.Date(), ".xlsx"))
  new <- read_excel_allsheets(paste0(file.path("output/db_release_schema_comparison", in.host, in.db),
                                     "/", in.db, "_release_summary_", Sys.Date(), ".xlsx"))
  
  comparison <- list(
    # Tables in new, not in old
    new_tables = new$tbl_list %>%
      dplyr::filter(!table_name %in% old$tbl_list$table_name),
    # Tables in old, not in new
    deleted_tables = old$tbl_list %>%
      dplyr::filter(!table_name %in% new$tbl_list$table_name),
    # Fields in new, not in old
    new_fields = new$field_list %>%
      dplyr::filter(!column_name %in% old$field_list$column_name),
    # Fields in old, not in new
    deleted_fields = old$field_list %>%
      dplyr::filter(!column_name %in% new$field_list$column_name),
    # Same field name, different type
    updated_fields = new$field_list %>%
      dplyr::select(table_name, column_name, column_type) %>%
      dplyr::distinct() %>%
      dplyr::left_join(old$field_list %>%
                         dplyr::select(table_name, column_name, old_column_type = column_type) %>%
                         dplyr::distinct(),
                       by=c("table_name", "column_name")) %>%
      dplyr::filter(!is.na(old_column_type),
                    column_type != old_column_type) %>%
      dplyr::distinct(),
    # Record count differences for table overlap
    row_count_diff = new$tbl_list %>%
      select(-table_schema) %>%
      dplyr::left_join(old$tbl_list %>%
                         select(-table_schema) %>%
                         dplyr::rename(old_n_tbl_columns=n_tbl_columns,
                                       old_n_tbl_row_est=n_tbl_row_est,
                                       old_n_tbl_row=n_tbl_row),
                       by="table_name") %>%
      dplyr::mutate(n_tbl_column_diff = round(n_tbl_columns-old_n_tbl_columns, 3),
                    n_tbl_row_est_diff = round(n_tbl_row_est-old_n_tbl_row_est, 3),
                    n_tbl_row_diff = round(n_tbl_row-old_n_tbl_row, 3))
  )
  
  # Shorten hostname for filepath length sake
  out.compare.host = compare.host
  out.in.host = in.host
  if(grepl("aws", out.compare.host)) out.compare.host = "aws"
  if(grepl("aws", out.in.host)) out.in.host = "aws"
  
  writexl::write_xlsx(comparison,
                      paste0("output/db_release_schema_comparison/",
                             out.in.host, "_", in.db, "_", out.compare.host, "_", compare.db, "_release_comparison_", Sys.Date(), ".xlsx"))
  message("Done comparing...")
}
