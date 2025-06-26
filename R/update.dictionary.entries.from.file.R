#' @title update.dictionary.entries.from.file
#' @description Function to update dictionary entries from file generated from 
#' get.dictionary.entries.to.curate().
#' @param schema Database schema for PostgreSQL.
#' @param in_file Input file with dictionary updates to process.
#' @export 
#' @return Dataframe list of dictionaries.
#' @seealso 
#'  \code{\link[readxl]{excel_sheets}}
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{reexports}}
#' @rdname update.dictionary.entries.from.file
#' @importFrom readxl excel_sheets
#' @importFrom dplyr select any_of
update.dictionary.entries.from.file <- function(schema, in_file){
  # Load input file if it exists
  if(file.exists(in_file)){
    message("Loading input file...")
    # Get sheets to load
    s_list = readxl::excel_sheets(in_file) %>%
      # Filter out helper fk_* sheets
      .[!grepl("^fk", .)]
    # Load into named dataframe list
    dict_updates = lapply(s_list, read_xlsx, path=in_file) %T>% { names(.) <- s_list }
  }
  
  message("Pushing dictionary udpates...")
  for(dict in names(dict_updates)){
    db_update_tbl(df = dict_updates[[dict]] %>%
                    dplyr::select(-dplyr::any_of(c("created_by", "rec_create_dt"))), 
                  tblName = dict)
  }
  message("Done.")
}
