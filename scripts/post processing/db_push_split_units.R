#' @title Database Push Split Units
#' @description Function to load log of split units to push to database for updates.
#' Input log is the output from pp_split_units.R, manually curated with a "confirmed"
#' field of 0 (No) or 1 (Yes) for confirmed unit splits.
#' @param split_file Relative path to split units log.
#' @return None. SQL updates performed.
db_push_split_units <- function(split_file){
  # Pull log of split units to push for updates
  split_units = readxl::excel_sheets(split_file) %>%
    lapply(., function(s){
      if(s == "unhandled") return(data.frame())
      readxl::read_xlsx(split_file,
                        sheet=s) %>%
        dplyr::filter(confirmed == 1) %>%
        dplyr::select(id, split_value, split_units, curator_comment) %>%
        dplyr::mutate(case_name = s) %>%
        tidyr::separate(case_name, into=c("table_name", "case_name"), sep = "_", extra = "merge")
    }) %>%
    dplyr::bind_rows()
  
  # Loop through each table for updates
  for(tbl_n in unique(split_units$table_name)){
    updated_data = split_units %>%
      dplyr::filter(table_name == tbl_n) %>%
      tidyr::pivot_longer(cols = c(split_value, split_units)) %>%
      tidyr::unite(col="name", case_name, name, sep="_") %>%
      dplyr::mutate(name = name %>%
                      gsub("split_|value", "", .) %>%
                      gsub("_$", "", .)) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(curator_comment = toString(unique(curator_comment)) %>%
                      dplyr::na_if("NA")) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(id_cols = c("id", curator_comment))
    View(updated_data)
    message("Push update? - Select continue...")
    browser()
    # Update database entry for document
    db_update_tbl(df=updated_data,
                  tblName = tbl_n)
  }
}
