#' @title get_chems_for_curation
#' @description Pull list of chemical identifiers to submit for chemical curation.
#' @return Output XLSX file generated with chemical identifiers for curation.
#' @seealso 
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{unite}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{bind_cols}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{na_if}}
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[writexl]{write_xlsx}}
#' @rdname get_chems_for_curation
#' @export 
#' @importFrom tidyr pivot_longer unite
#' @importFrom dplyr filter select bind_rows pull bind_cols mutate case_when across na_if
#' @importFrom readxl read_xlsx
#' @importFrom writexl write_xlsx
get_chems_for_curation <- function(){
  chem_curation_scripts = list.files("scripts/chemical_curation", full.names = TRUE)
  invisible(sapply(chem_curation_scripts, source,.GlobalEnv))
  
  in_data = db_query_cvt(paste0("SELECT id, chemical_name_original, ",
                                "chemical_name_secondary_original, ",
                                "casrn_original as casrn ",
                                "FROM cvt.chemicals WHERE dsstox_substance_id IS NULL")) %>%
    tidyr::pivot_longer(cols=c(-id, -casrn), values_to="name", names_to = "name_type") %>%
    tidyr::unite(id, name_type, col = "external_id", sep = "_") %>%
    tidyr::unite(name, casrn, col = "filter_key", sep = "_", remove=FALSE) %>%
    dplyr::filter(filter_key != "NA_NA") %>%
    dplyr::select(-filter_key)
  
  # Read in previous attempts to ignore
  previous_map_attempt = list.files("output/chemical_mapping", 
                                    recursive = TRUE, full.names = TRUE) %>%
    .[grepl("jira_chemical_files", .)] %>%
    lapply(., readxl::read_xlsx) %>%
    dplyr::bind_rows() %>%
    dplyr::pull(external_id) %>%
    unique()
  
  in_data = in_data %>%
    dplyr::filter(!external_id %in% previous_map_attempt)
  
  out = chem.check.v2(res0 = in_data)
  
  # Export raw and cleaned chems
  chems = in_data %>%
    dplyr::select(external_id, raw_name = name, raw_casrn = casrn) %>%
    dplyr::bind_cols(out$res0 %>%
                       dplyr::mutate(casrn = dplyr::case_when(
                         cs == FALSE ~ NA,
                         TRUE ~ casrn
                       )) %>%
                       dplyr::select(cleaned_name = name, cleaned_casrn = casrn, checksum_pass = cs)
                     ) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~dplyr::na_if(., "NA")),
                  cleaned_casrn = dplyr::case_when(
                    checksum_pass %in% c(0) ~ NA,
                    TRUE ~ checksum_pass
                  )) %>% 
    # Filter out any without cleaned information
    tidyr::unite(cleaned_name, cleaned_casrn, col = "filter_key", sep = "_", remove=FALSE) %>%
    dplyr::filter(filter_key != "NA_NA") %>%
    dplyr::select(-filter_key) %T>%
    writexl::write_xlsx(., paste0("output/chemical_mapping/cvtdb_chems_to_curate_",Sys.Date(),".xlsx"))
    
    return(chems)
}
