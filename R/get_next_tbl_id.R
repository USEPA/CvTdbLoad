#' @title get_next_tbl_id
#' @description Function to get the next ID increment value for schema tables
#' @param schema PostgreSQL database schema
get_next_tbl_id <- function(schema){
  tbl_list = db_query_cvt(paste0("SELECT DISTINCT table_name ",
                                 "FROM information_schema.columns ",
                                 "WHERE table_schema='", schema,"'")) %>%
    dplyr::pull()
  
  # Query max ID and add 1 to return the next ID for each table
  lapply(tbl_list, function(tbl){
    db_query_cvt(paste0("SELECT max(id) + 1 FROM ", schema, ".", tbl))[[1]]
  }) %T>% {
    names(.) <- tbl_list
  } %>%
    return()
}
