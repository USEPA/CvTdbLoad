#' @title qc_check_foreign_keys
#' @description Script to check dict table foreign key mappings.
#' @return Dataframe list of checked foreign keys with conflicts.
#' @seealso 
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{count}}
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[purrr]{flatten}}
#'  \code{\link[writexl]{write_xlsx}}
#' @rdname qa_check_foreign_keys
#' @export 
#' @importFrom dplyr rename group_by across mutate ungroup distinct select all_of left_join case_when filter count
#' @importFrom stringr str_squish
#' @importFrom purrr flatten
#' @importFrom writexl write_xlsx
qc_check_foreign_keys <- function(){

  # List of tables and dictionary fields to check/join with
  check_list = list(
    series = list(
      conc_medium_dict = c("fk_conc_medium_id", "conc_medium_original"),
      chemicals = c("fk_analyzed_chemical_id", "analyte_name_original", "analyte_name_secondary_original", "analyte_casrn_original")
    ),
    
    studies = list(administration_route_dict = c("fk_administration_route_id", "administration_route_original"),
                   administration_method_dict = c("fk_administration_method_id", "administration_method_original"),
                   administration_form_dict = c("fk_administration_form_id", "administration_form_original"),
                   dose_frequency_dict = c("fk_dose_frequency_id", "dose_frequency_original"),
                   chemicals = c("fk_dosed_chemical_id", "test_substance_name_original", "test_substance_name_secondary_original", "test_substance_casrn_original")
    )
  )
  
  # chemicals table columns to use for joins
  chem_cols = c("chemical_name_original", "chemical_name_secondary_original", "casrn_original")
  
  # Loop through each table
  dict_checks = lapply(names(check_list), function(tbl_n){
    message("Checking table '", tbl_n, "'")
    # Select table relevant dictionary checks
    checks = check_list[[tbl_n]]
    
    # Loop through each dictionary table check
    dict_checks_n = lapply(names(checks), function(c_item){
      message("...checking dict '", c_item, "'")
      # Select columns for dictionary check
      in_cols = checks[[c_item]]
      # Pull table data for select dictionary columns
      in_data = db_query_cvt(paste0("SELECT distinct id, ", toString(in_cols), 
                                    " FROM cvt.", tbl_n))    
      # Pull dictionary fields
      dict = db_query_cvt(paste0("SELECT id, ", 
                                 # Handle speciesl chemicals table columns pull
                                 ifelse(c_item == "chemicals", 
                                        toString(chem_cols), 
                                        toString(in_cols[!grepl("^fk_", in_cols)])),
                                 " FROM cvt.", c_item)) %>%
        dplyr::rename(suggested_id = id) %>%
        # Collapse values by id (helps identify duplicate dictionary entries)
        dplyr::group_by(dplyr::across(c(-suggested_id))) %>%
        dplyr::mutate(suggested_id = toString(suggested_id)) %>%
        dplyr::ungroup() %>%
        dplyr::distinct()
      
      # Get join "by" columns
      if(c_item == "chemicals"){
        # Named vector for chemicals table join
        by_cols = setNames(chem_cols, in_cols[grepl("_original$", in_cols)])  
      } else {
        by_cols = in_cols[grepl("_original$", in_cols)]
      }
      
      # Join table data to dictionary table by "original" fields
      out = in_data %>%
        dplyr::select(dplyr::all_of(c("id", checks[[c_item]]))) %>%
        dplyr::left_join(dict,
                         by = by_cols
        ) %>%
        dplyr::distinct() %>%
        # Compare currently assigned foreign key to dictionary id the "original" fields map to
        dplyr::mutate(compare = dplyr::case_when(
          # No dictionary mapping found
          is.na(!!as.name(in_cols[grepl("^fk_", in_cols)])) & is.na(suggested_id) ~ "Missing",
          # No previous dictionary mapping
          is.na(!!as.name(in_cols[grepl("^fk_", in_cols)])) & !is.na(suggested_id) ~ "Missing Old",
          # No current dictionary mapping
          !is.na(!!as.name(in_cols[grepl("^fk_", in_cols)])) & is.na(suggested_id) ~ "Missing New",
          # If the previous foreign key matches the current
          TRUE ~ as.character(!!as.name(in_cols[grepl("^fk_", in_cols)]) != suggested_id)
        ) %>% 
          stringr::str_squish()) %>%
        # Filter out any foreign key mappings that matched
        dplyr::filter(!compare %in% c("FALSE"))
      
      # Print out a summary by comparison types
      print(out %>% dplyr::count(compare))
      
      return(out)
    }) %T>% {
      # Name by table and dictionary
      names(.) <- paste0(tbl_n, "_", names(checks)) %>%
        gsub("_dict", "", .)
    }
  }) %>%
    # Combine lists and flatten
    purrr::flatten()
  
  # Export report
  writexl::write_xlsx(dict_checks, "output/fk_dict_mapping_checks.xlsx")

  # Return report
  return(dict_checks)  
  
  # Utility functions to help explore data and select datasets to use for updates
  # out %>% dplyr::select(-id) %>% dplyr::filter(compare == "TRUE") %>% dplyr::distinct() %>% View()
  # 
  # missing = out %>%
  #   dplyr::filter(compare == "Missing New") %>%
  #   dplyr::select(analyte_name_original, analyte_name_secondary_original, analyte_casrn_original) %>%
  #   dplyr::distinct()
  # 
  # update_df = out %>%
  #   dplyr::filter(compare == "TRUE") %>%
  #   dplyr::select(id, fk_analyzed_chemical_id = suggested_id)
  # 
  # # db_update_tbl(df=NULL, tblName=NULL)
  
}

