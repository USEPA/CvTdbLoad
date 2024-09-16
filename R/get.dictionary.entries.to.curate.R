#' @title get.dictionary.entries.to.curate
#' @description Function to generate file of dictionary entries to curate
#' normalized field values. Includes foreign key identifiers to help query database
#' @param schema Database schema for PostgreSQL
#' @export
#' @return Dataframe list of dictionaries
get.dictionary.entries.to.curate <- function(schema){
  
  # Map of dictionary tables to their foreign key tables/fields
  fk_list = list(
    administration_form_dict = c("studies", "fk_administration_form_id"),
    administration_method_dict  = c("studies", "fk_administration_method_id"),
    administration_route_dict  = c("studies", "fk_administration_form_id"),
    conc_medium_dict  = c("series", "fk_conc_medium_id"),
    dose_frequency_dict  = c("studies", "fk_dose_frequency_id")
  )
  
  # List of dictionary tables
  dict_list = db_query_cvt(paste0("SELECT table_name FROM information_schema.tables ",
                                  "WHERE table_schema = '", schema, "'",
                                  "AND table_name like '%_dict%'"
  ))
  
  # Loop through dict tables
  missing = lapply(dict_list$table_name, function(d_tbl){
    # Pull dictionary entries with a NULL normalized field
    in_data = db_query_cvt(paste0("SELECT * FROM ", schema, ".", d_tbl ,
                        " WHERE ", gsub("_dict", "_normalized ", d_tbl), 
                        "IS NULL"))
    
    # Pull associated record in foreign key table
    fk_tbl = fk_list[[d_tbl]]
    fk_data = db_query_cvt(paste0("SELECT * FROM ", schema, ".", fk_tbl[1], 
                                  " WHERE ", fk_tbl[2], " IN (", toString(in_data$id), ")"))
    
    # Return as named list of dataframes
    list(in_data, fk_data) %T>% {
      names(.) <- c(d_tbl, paste0("fk_", d_tbl))
    } %>%
      return()
  }) %>%
    purrr::flatten() %T>% {
      writexl::write_xlsx(., path = paste0("output/missing_dict_entries_", Sys.Date(), ".xlsx"))
    } %>%
    return()
}