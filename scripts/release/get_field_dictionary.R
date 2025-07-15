#' @title Get field dictionary
#' @description Get database table and field dictionary. Optionally combine old dictionary
#' to fill in fields with previous definitions.
#' @param schema Database schema name.
#' @param old_dict Optional filepath to old dictionary to join previous definitions.
#' @return None. XLSX file is saved to output/release folder
#' @seealso 
#'  \code{\link[tools]{fileutils}}
#'  \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[writexl]{write_xlsx}}
#' @rdname get_field_dictionary
#' @export 
#' @importFrom tools file_ext
#' @importFrom dplyr left_join mutate across
#' @importFrom readxl read_xlsx
#' @importFrom writexl write_xlsx
get_field_dictionary <- function(schema, old_dict){
  
  # Pull dataframe of table and field names in schema
  field_list = db_query_cvt(paste0("SELECT table_name, column_name ",
                                   "FROM information_schema.columns ",
                                   "WHERE table_schema = '", schema, "' ",
                                   "ORDER BY table_name, column_name"))
  
  # If provided, combine with old dictionary
  if(!is.null(old_dict) && !is.na(old_dict)){
    if(file.exists(old_dict) && tools::file_ext(old_dict) == "xlsx"){
      field_list = field_list %>%
        dplyr::left_join(readxl::read_xlsx(old_dict),
                         by = c("table_name", "column_name")) %>%
        # Add ending period if needed for select columns
        dplyr::mutate(dplyr::across(c("short_description", "notes"),
                                    ~ sub("([^.])$", "\\1.", .)))
    }
  }
  
  # Export diction to file
  writexl::write_xlsx(field_list, paste0("output/release/cvtdb_field_dictionary_toupdate.xlsx"))
}
