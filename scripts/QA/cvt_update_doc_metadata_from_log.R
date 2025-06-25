#' @title cvt_update_doc_metadata_from_log
#' @description Parse metadata logs to fill in NULL values from CvTdb documents table records.

#' @return None. SQL update statements are executed.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{group_split}}
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_split}}
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{unite}}, \code{\link[tidyr]{pivot_wider}}, \code{\link[tidyr]{drop_na}}
#' @rdname cvt_update_doc_metadata_from_log
#' @export 
#' @importFrom dplyr bind_rows mutate distinct across filter group_by everything ungroup case_when count select left_join group_split all_of
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_squish str_split
#' @importFrom tidyr pivot_longer unite pivot_wider drop_na
cvt_update_doc_metadata_from_log <- function(){
  
  # Load metadata files
  meta = dplyr::bind_rows(
    readxl::read_xlsx("metadata_from_pmid_shermeli_20250513.xlsx",
                      col_types = "text") %>%
      dplyr::mutate(list_name = "pmid"),
    readxl::read_xlsx("metadata_from_hero_shermeli_20250513.xlsx",
                      col_types = "text") %>%
      dplyr::mutate(list_name = "hero")
  ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      # Remove excess whitespace
      dplyr::across(where(is.character), ~ stringr::str_squish(.)),
      # Clean up URL field (quirks of the log generation and PubMed API)
      url = url %>%
        gsub("0pubmed$", "", .) %>%
        gsub("\\.0\\/$", "/", .) %>%
        stringr::str_squish()) %>%
    # Pivot to compare to the database field values next
    tidyr::pivot_longer(
      cols = -c("id", "list_name"),
      names_to = "field_name"
    ) %>%
    # Remove NA field values
    dplyr::filter(!is.na(value)) %>%
    tidyr::unite(
      col = "field_key",
      id, field_name,
      sep = "_",
      remove = FALSE
    )
  
  # Pull database records
  cvt_docs = db_query_cvt(paste0("SELECT DISTINCT ",
                                 "id, pmid, other_study_identifier, title, first_author, doi, year, url ",
                                 "FROM cvt.documents ",
                                 "WHERE id in (", toString(unique(meta$id)), ")")) %>%
    tidyr::pivot_longer(
      cols = -c("id"),
      names_to = "field_name",
      values_transform = as.character
    ) %>%
    # Filter to those with NULL values
    dplyr::filter(is.na(value)) %>%
    tidyr::unite(
      col = "field_key",
      id, field_name,
      sep = "_",
      remove = FALSE
    )
  
  meta_out = meta %>%
    # Filter to metadata with a new value to fill in
    dplyr::filter(field_key %in% cvt_docs$field_key) %>%
    # Collapse conflicting duplicate log entries for field values
    dplyr::group_by(field_key) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~paste0(unique(.), collapse = " <::> "))) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    # Select from the pmid list if there is a conflict
    dplyr::mutate(
      value = dplyr::case_when(
        grepl("<::>", list_name, fixed = TRUE) &
          grepl("<::>", value, fixed = TRUE) ~ gsub('<::>.*', "", value) %>%
          stringr::str_squish(),
        TRUE ~ value
      ))
  
  # Check for duplicates
  meta_out %>%
    dplyr::count(field_key) %>%
    dplyr::filter(n > 1)
  
  # If there is new metadata to add, continue
  if(nrow(meta_out)){

    # Generate field groups to only push updates to fields with a new value
    meta_update_groups = meta_out %>%
      dplyr::select(id, field_name) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(field_name_group = toString(sort(unique(field_name)))) %>%
      dplyr::ungroup() %>%
      dplyr::distinct() %>%
      dplyr::select(-field_name) %>%
      dplyr::distinct()
    
    meta_out = meta_out %>%
      tidyr::pivot_wider(
        id_cols = c(id),
        names_from = "field_name"
      ) %>%
      # Join to get metadata field groups
      dplyr::left_join(meta_update_groups,
                       by = "id") %>%
      # Split by group
      dplyr::group_split(field_name_group)
    
    # Loop through the metadata update field groups (only update fields that have new values to push)
    for(df in meta_out){
      # Get vector of metadata fields
      metadata_fields = unique(df$field_name_group) %>%
        stringr::str_split(", ") %>% 
        .[[1]] %>%
        stringr::str_squish()
      
      update_df = df %>%
        # Select metadata fields
        dplyr::select(dplyr::all_of(c("id", metadata_fields))) %>%
        # Drop NA values
        tidyr::drop_na(dplyr::all_of(metadata_fields)) %>%
        # Add qc_notes of what fields are being filled
        dplyr::mutate(qc_notes = paste0("Filled in fields: ", toString(metadata_fields)))
      
      # Push update (uncomment db_update_tbl call when ready)
      if(nrow(update_df)){
        # db_update_tbl(df = update_df,
        #               tblName = "documents")
      }
    }
  }  
}
