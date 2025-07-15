#' @title map_curated_chemicals
#' @description Maps chemical identifiers to DTXSID identifiers and other chemical curation metadata.
#' @param source.index The source chemical index. Can be full or just numeric
#' @param curated.path Input path to the folder directory with expected subdirectories of #' 'BIN Files', 'DSSTox Files', and 'jira_chemical_files'
#' @param ignore.curation.dups Boolean whether to match with any curated records flagged as "unresolved duplicates" (Default TRUE)
#' @return Dataframe of chemical identifiers mapped to DTXSID identifiers and other chemical curation metadata.
#' @seealso 
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{rename}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{bind_rows}}
#' @rdname map_curated_chemicals
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom dplyr rename distinct mutate select left_join filter bind_rows
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
