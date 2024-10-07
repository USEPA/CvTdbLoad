#-------------------------------------------------------------------------------------
#' @title toxval.source_push_mapped_chemicals
#' @description Orchestrates the push of mapped chemical information to the selected toxval_source database.
#' Uses the map_curated_chemicals() helper function to generate mapped input.
#' @param source.index The source chemical index. Can be full or just numeric (ex. ToxVal00001 vs. 00001)
#' @param curated.path Input path to the folder directory with expected subdirectories of #' 'BIN Files', 'DSSTox Files', and 'jira_chemical_files'
#' @param ignore.curation.dups Boolean whether to match with any curated records flagged as "unresolved duplicates" (Default TRUE)
#' @return None. Update SQL statements are executed.
#' @import RMySQL dplyr readxl magrittr
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{bind}}
#' @rdname toxval.source_push_mapped_chemicals
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr rename distinct mutate select left_join filter bind_rows
#' @example toxval.source_push_mapped_chemicals(source.index="cvtdb20240215", curated.path = "output/chemical_mapping/DSSTOX_1333", ignore.curation.dups=FALSE)
#--------------------------------------------------------------------------------------
toxval.source_push_mapped_chemicals <- function(source.index, curated.path,
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
    dplyr::summarise(n=n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n == 1)
    
  # Rule #1: If primary and secondary map to same, use primary
  mapped_doubles_match = out %>%
    dplyr::filter(!id %in% mapped_singles$id) %>%
    dplyr::group_by(id, dtxsid) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n == 2)
    
  # Rule #2: If both map, but different, flag but don't push
  mapped_doubles_differ = out %>%
    dplyr::filter(!id %in% c(mapped_singles$id, mapped_doubles_match$id)) %>%
    dplyr::group_by(id, dtxsid) %>%
    dplyr::summarise(n=n()) %>%
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


#-------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
map_curated_chemicals <- function(source.index, curated.path, ignore.curation.dups = TRUE){
  # curated.path = "Repo\\chemical_mapping\\DSSTOX_1142"
  if(is.null(source.index) || is.na(source.index)) return("Must provide 'source.index'...")
  # Clean up source.index to only the numeric value
  # source.index = gsub("[^0-9.-]", "", source.index)
  
  c_dirs = list.dirs(curated.path, recursive = FALSE)
  curated_list = lapply(c_dirs, function(d){
    tmp = list.files(d, pattern=".xlsx") %>%
      .[grepl(source.index, .)]
    # Remove Windows temp files that start with "~"
    return(tmp[!grepl("^~", tmp)])
  }) %T>% { names(.) <- basename(c_dirs) }
  
  out = lapply(curated_list$`DSSTox Files`, function(c_list){
    message("...mapping file: ", c_list)
    # Get specific sub source.index (e.g., _a, _b, _c)
    curated_source.index = source.index
    # Load required files from curation
    c_files = list(orig_file = curated_list$jira_chemical_files[grepl(curated_source.index, curated_list$jira_chemical_files)] %>%
                     paste0(curated.path, "/jira_chemical_files/", .),
                   b_file = curated_list$`BIN Files`[grepl(curated_source.index, curated_list$`BIN Files`)] %>%
                     paste0(curated.path, "/BIN Files/", .),
                   d_file = curated_list$`DSSTox Files`[grepl(curated_source.index, curated_list$`DSSTox Files`)] %>%
                     paste0(curated.path, "/DSSTox Files/", .))
    
    # Check files exist (need all 3)
    if(!all(lapply(c_files, file.exists) %>% unlist())) return("Missing input file...")
    # Load input files
    c_files = lapply(c_files, readxl::read_xlsx) %T>%
      { names(.) <- names(c_files) }
    
    # Select and rename columns
    c_files$b_file = c_files$b_file %>%
      dplyr::rename(flags=`Lookup Result`,
                    #name=`Query Name`,
                    #casrn=`Query Casrn`,
                    dtxsid=`Top HIT DSSTox_Substance_Id`) %>%
      dplyr::distinct() %>%
      dplyr::mutate(`Query Name` = gsub("\"", "", `Query Name`))
    # Select and rename columns
    c_files$d_file = c_files$d_file %>%
      dplyr::select(chemical_id = Extenal_ID,
                    dtxsid = DSSTox_Substance_Id,
                    dtxrid = DSSTox_Source_Record_Id,
                    quality=`DSSTox_QC-Level`) %>%
      dplyr::distinct()
    
    # --- Collect matches ---
    out = data.frame()
    
    if("external_id" %in% names(c_files$orig_file)){
      c_files$orig_file = c_files$orig_file %>%
        dplyr::rename(chemical_id = external_id)
    }
    tmp = c_files$orig_file %>%
      # dplyr::select(chemical_id, raw_name, raw_casrn, cleaned_name) %>%
      dplyr::left_join(c_files$d_file,
                       by="chemical_id") %>%
      # Filter out ones that didn't map
      dplyr::filter(!is.na(dtxrid)) %>%
      dplyr::left_join(c_files$b_file %>%
                         # dplyr::select(dtxsid, flags, `Top Hit Name`, `Top Hit Casrn`) %>%
                         dplyr::distinct(),
                       # by="dtxsid"
                       by=c("cleaned_name"="Query Name", "cleaned_casrn"="Query Casrn", "dtxsid")
      ) %>%
      # Filter out ones that didn't map
      dplyr::filter(!is.na(dtxrid) | !is.na(flags))
    
    # Store matches
    out = dplyr::bind_rows(out, tmp)
    # Remove previous matches
    c_files$orig_file = c_files$orig_file %>%
      dplyr::filter(!chemical_id %in% out$chemical_id)
    
    # Filter out duplicate curation records if desired
    if(ignore.curation.dups){
      out = out %>%
        dplyr::filter(flags != "Unresolved Duplicates")
    }
    
    # Rename/select fields and return
    out %>%
      dplyr::select(chemical_id,
                    raw_name,
                    raw_casrn,
                    cleaned_name,
                    cleaned_casrn,
                    checksum_pass,
                    #`Query Name`,
                    #`Query Casrn`,
                    `Top Hit Casrn`,
                    `Top Hit Name`,
                    dtxsid,
                    dtxrid,
                    quality,
                    flags) %>%
      dplyr::distinct() %>%
      return()
  }) %>%
    # Combine all subsets of a given source.index curation effort
    dplyr::bind_rows()
  message("...returning...")
  return(out)
}
