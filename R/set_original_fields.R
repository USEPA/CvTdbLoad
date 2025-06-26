#' @title set_original_fields
#' @description Pull dictionary of original fields from database tables and rename in sheet_list
#' @param sheet_list Dataframe list to rename with original columns
#' @param schema String for the PostgreSQL schema information to pull.
set_original_fields <- function(sheet_list, schema){
  # Pull all tables/field information from input schema
  db_f_list = db_query_cvt(paste0("SELECT * FROM information_schema.columns WHERE table_schema = '",schema,"'")) %>%
    # Filter to fields wtih "original" suffix
    dplyr::filter(grepl("_original", column_name)) %>%
    dplyr::select(column_name) %>%
    dplyr::mutate(template_field = column_name %>%
                    gsub("_original$", "", .)) %>%
    dplyr::distinct()
  
  # Create named list for renaming
  db_f_list = db_f_list$template_field %>%
    unlist() %T>% {
      names(.) <- db_f_list$column_name
    }
  
  # Rename sheet_list fields
  sheet_list = lapply(sheet_list, function(df){
    df %>%
      dplyr::rename(dplyr::any_of(db_f_list))
  }) %>%
    return()
}
