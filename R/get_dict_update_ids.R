#' @title get_dict_update_ids
#' @description Update database dictionaries and match foreign keys
#' @param sheet_list Dataframe list to rename with original columns
#' @param schema String for the PostgreSQL schema information to pull.
get_dict_update_ids <- function(sheet_list, schema){
  # Upload new dictionary entries
  dict_list = db_query_cvt(paste0("SELECT table_name, column_name FROM information_schema.columns WHERE table_schema = '",schema,"' ",
                                  "AND table_name like '%_dict' and column_name NOT in ('id')"))
  
  for(dict_tbl in unique(dict_list$table_name)){
    message("...updating ", dict_tbl)
    # Pull current dictionary
    dict = db_query_cvt(paste0("SELECT * FROM ", schema, ".", dict_tbl)) %>%
      # Rename ID column as foreign key
      dplyr::rename(
        c("id") %T>% {names(.) <- paste0("fk_", gsub("_dict", "", dict_tbl), "_id")}
      )
    # Go through every template sheet to update dictionaries
    for(sheet_n in names(sheet_list)){
      sheet = sheet_list[[sheet_n]]
      # Check if sheet contains dictionary column
      if(any(names(sheet) %in% names(dict))){
        # Get dictionary column of interest
        dict_col = names(sheet)[names(sheet) %in% names(dict)]
        new = sheet %>%
          dplyr::filter(!(!!as.symbol(dict_col) %in% dict[[dict_col]])) %>%
          dplyr::select(any_of(dict_col)) %>%
          dplyr::distinct() %>%
          dplyr::mutate(dplyr::across(!where(is.character), as.character))
        # Check for new entries
        if(nrow(new)){
          # Push new entries to database
          db_push_tbl_to_db(dat=new,# %>% dplyr::select(-id),
                            tblName=dict_tbl,
                            overwrite=FALSE, append=TRUE)
        }  
        
        # Pull updated dictionary
        dict = db_query_cvt(paste0("SELECT * FROM ", schema, ".", dict_tbl)) %>%
          dplyr::select(dplyr::any_of(c("id", dict_col))) %>%
          # Rename ID column as foreign key
          dplyr::rename(
            c("id") %T>% {names(.) <- paste0("fk_", gsub("_dict", "", dict_tbl), "_id")}
          )
        
        # Match dictionary ID values with updated dictionary
        sheet_list[[sheet_n]] = sheet %>%
          dplyr::mutate(!!dict_col := as.character(!!as.symbol(dict_col))) %>%
          dplyr::left_join(dict,
                           by=dict_col)
      }
    }
  }
  # Return updated sheet_list with foreign key matches
  return(sheet_list)
}

