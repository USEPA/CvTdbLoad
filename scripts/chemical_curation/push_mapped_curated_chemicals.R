#' @title push_mapped_curated_chemicals
#' @description Orchestrates the push of mapped chemical information to the database.
#' Uses the map_curated_chemicals() helper function to generate mapped input.
#' @param source.index The source chemical index. Can be full or just numeric
#' @param curated.path Input path to the folder directory with expected subdirectories of #' 'BIN Files', 'DSSTox Files', and 'jira_chemical_files'
#' @param ignore.curation.dups Boolean whether to match with any curated records flagged as "unresolved duplicates" (Default TRUE)
#' @return None. Update SQL statements are executed.
#' @import RMySQL dplyr readxl magrittr
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  toxval.source_push_mapped_chemicals(source.index="cvtdb20240215", curated.path = "output/chemical_mapping/DSSTOX_1333", ignore.curation.dups=FALSE)
#'  }
#' }
#' @seealso 
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{bind}}
#' @rdname push_mapped_curated_chemicals
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom dplyr rename distinct mutate select left_join filter bind_rows
push_mapped_curated_chemicals <- function(source.index, curated.path,
                                                ignore.curation.dups = FALSE){
  
  message("Pushing mapped chemicals for chemical_source_index: ", source.index)
  # Map chemical information from curated files and select source.index
  out = map_curated_chemicals(source.index=source.index, curated.path=curated.path,
                              ignore.curation.dups=ignore.curation.dups) %>%
    dplyr::group_by(chemical_id) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(names(.)[!names(.) %in% c("chemical_id")]),
                                # Ensure unique entries in alphabetic order
                                # Collapse with unique delimiter
                                ~paste0(sort(unique(.[!is.na(.)])), collapse="|::|") %>%
                                  dplyr::na_if("NA") %>%
                                  dplyr::na_if("")
    )) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()
  
  # Check De-duping collapse (only expect flags field to collapse)
  dup_collapsed_fields = lapply(names(out), function(f){
    if(sum(stringr::str_detect(out[[f]], '\\|::\\|'), na.rm = TRUE) > 0){
      return(f)
    }
  }) %>%
    purrr::compact() %>%
    unlist()
  
  # Error if any other field collapsed besides flags
  if(any(names(out)[!names(out) %in% c("flags")] %in% dup_collapsed_fields)){
    message("Duplicate entries found/collapsed beyond 'flags' field...need to resolve...")
    browser()
    stop("Duplicate entries found/collapsed beyond 'flags' field...need to resolve...")
  }
  
  out = out %>%
    # Replace unique delimiter with standard delimiter after checking passed
    dplyr::mutate(flags = flags %>%
                    gsub("|::|", "; ", ., fixed=TRUE))  
    
    
  out = out %>%
    tidyr::separate(col=chemical_id, 
                    into = c("id", "name_type"),
                    sep="_", 
                    extra="merge") %>%
    # Filter out entries without a mapped DTXSID value
    dplyr::filter(!is.na(dtxsid))
  
  # Type 1: Only 1 mapped
  mapped_singles = out %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n == 1)
    
  # Rule #1: If primary and secondary map to same, use primary
  mapped_doubles_match = out %>%
    dplyr::filter(!id %in% mapped_singles$id) %>%
    dplyr::group_by(id, dtxsid) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n == 2)
    
  # Rule #2: If both map, but different, flag but don't push
  mapped_doubles_differ = out %>%
    dplyr::filter(!id %in% c(mapped_singles$id, mapped_doubles_match$id)) %>%
    dplyr::group_by(id, dtxsid) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n == 1)
  
  # Flag differ for manual review
  writexl::write_xlsx(out %>%
                        dplyr::filter(id %in% mapped_doubles_differ$id),
                      paste0(curated.path, "/flagged_curation_diffs.xlsx"))
    
  # Filter to IDs we want to push
  push_chems = out %>%
    dplyr::filter(id %in% c(mapped_singles$id, mapped_doubles_match$id)) %>%
    dplyr::mutate(chemistry_team_mapping = 1) %>%
    # Rename/select columns to push
    dplyr::select(id, 
                  dsstox_substance_id = dtxsid, 
                  dsstox_casrn = `Top Hit Casrn`,
                  preferred_name = `Top Hit Name`,
                  chemistry_team_mapping,
                  curation_notes = flags
                  ) %>%
    dplyr::distinct()
  
  if(!nrow(push_chems)){
    message("...no new chemical maps to push. Set reset.mapping to TRUE to reset mappings")
    return()
  }

  # Update chemicals table entries
  db_update_tbl(df = push_chems,
                tblName = "chemicals")
  
}
