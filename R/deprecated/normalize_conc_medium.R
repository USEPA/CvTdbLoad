#' @title normalize_conc_medium
#' @description Function to use input dictionary to map normalized concentraiton medium.
#' @param raw Input dataframe of data with data to normalize.
#' @param f Optional filename for logging purposes.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [read_xlsx][readxl::read_xlsx]
#'  [mutate][dplyr::mutate], [rename][dplyr::rename], [select][dplyr::select], [left_join][dplyr::left_join], [filter][dplyr::filter]
#' @rdname normalize_conc_medium
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate rename select left_join filter
normalize_conc_medium <- function(raw, f){
  # Concentration Normalization Dictionary
  conc_dict = readxl::read_xlsx("input/dictionaries/conc_medium_dict.xlsx") %>%
    dplyr::mutate(conc_medium_original = tolower(conc_medium_original)) %>%
    dplyr::rename(conc_medium_id = id) %>%
    dplyr::select(-units)
  # Match conc_medium
  out = raw %>%
    dplyr::mutate(conc_medium_original = trimws(tolower(conc_medium))) %>%
    dplyr::left_join(conc_dict, by="conc_medium_original")
  # Check for unmatched conc_medium
  unmatched = out %>%
    dplyr::filter(is.na(conc_medium_normalized))
  # Flag unmatched
  if(nrow(unmatched)){
    message("...conc_meduium need curation: ", paste0(unique(unmatched$conc_medium_original), collapse="; "))
    log_CvT_doc_load(f=f, m="curate_conc_medium")
  }
  return(out)
}
