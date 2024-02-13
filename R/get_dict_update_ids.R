#' @title get_dict_update_ids
#' @description Update database dictionaries and match foreign keys
#' @param sheet_list Dataframe list to rename with original columns
#' @param schema String for the PostgreSQL schema information to pull.
get_dict_update_ids <- function(sheet_list, schema){
  chemical_dict_rename_full = data.frame(to = c("chemical_name_original", 
                                           "chemical_name_secondary_original",
                                           "casrn_original"),
                                    from = c("test_substance_name_original",
                                             "test_substance_name_secondary_original",
                                             "test_substance_casrn_original",
                                             "analyte_name_original",
                                             "analyte_name_secondary_original",
                                             "analyte_casrn_original")
                                    )
                            
  # Upload new dictionary entries
  dict_list = db_query_cvt(paste0("SELECT table_name, column_name FROM information_schema.columns WHERE table_schema = '",schema,"' ",
                                  "AND (table_name like '%_dict' or table_name = 'chemicals') and column_name NOT in ('id')"))
  
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
      if(dict_tbl == "chemicals"){
        # Select fields to rename
        chemical_dict_rename = chemical_dict_rename_full %>%
          dplyr::filter(from %in% names(sheet))
        chemical_dict_rename = chemical_dict_rename$from %T>% {
          names(.) <- chemical_dict_rename$to
        }
        # Rename chemical fields
        sheet = sheet %>%
          dplyr::rename(dplyr::any_of(chemical_dict_rename))
      }
      # Check if sheet contains dictionary column
      if(any(names(sheet) %in% names(dict))){
        # Get dictionary column of interest (_original field)
        dict_col = names(sheet)[names(sheet) %in% names(dict)[grepl("_original$", names(dict))]]
        # Handle chemicals dictionary differently
        if(dict_tbl == "chemicals"){
          # Create index column from dictionary fields
          dict = dict %>%
            tidyr::unite(dplyr::any_of(dict_col), col="dict_index", sep="_", remove=FALSE)
          # Get what's new for the dictionary
          new = sheet %>%
            tidyr::unite(dplyr::any_of(dict_col), col="dict_index", sep="_", remove=FALSE) %>%
            filter(!dict_index %in% dict$dict_index) %>%
            dplyr::select(any_of(dict_col)) %>%
            dplyr::distinct() %>%
            dplyr::mutate(dplyr::across(!where(is.character), as.character))  
        } else {
          # Get what's new for the dictionary
          new = sheet %>%
            dplyr::filter(!(!!as.symbol(dict_col) %in% dict[[dict_col]])) %>%
            dplyr::select(any_of(dict_col)) %>%
            dplyr::distinct() %>%
            dplyr::mutate(dplyr::across(!where(is.character), as.character))  
        }

        # Check for new entries
        if(nrow(new)){
          # Add ID (issues with database auto increment)
          max_id = db_query_cvt(paste0("SELECT MAX(id) FROM ", schema, ".", dict_tbl))[,1]
          if(is.na(max_id)) max_id = 0
          # Sequence of ID values
          new$id = (max_id+1):(nrow(new)+max_id)
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
        
        # Rejoin to match foreign key values
        sheet_list[[sheet_n]] = sheet %>%
          dplyr::mutate(dplyr::across(dplyr::any_of(dict_col), ~as.character(.))) %>%
          dplyr::left_join(dict,
                           by=dict_col)
        
        # Undo chemicals dictionary renaming
        if(dict_tbl == "chemicals"){
          sheet_list[[sheet_n]] = sheet_list[[sheet_n]] %>%
            dplyr::rename(names(chemical_dict_rename) %T>% {
              names(.) <- chemical_dict_rename %>% unname()
            })
        }
      }
    }
  }
  # Return updated sheet_list with foreign key matches
  return(sheet_list)
}

