#' @description Helper function to check for missing metric and attemp to exrapolate values.
#' Eventually make this a generic function for all norm extrapolation processes...
#' @param x Input list of datasets being normalized
#' @param f Filename for flagging purposes
#' @param extrap_type The type of extrapolation being performed (weight is the only accepted form at this time). #'
#' @param log_path File path where to save the log file.
#' @return Modified version of the input `x` parameter
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [filter][dplyr::filter], [mutate][dplyr::mutate], [across][dplyr::across], [group_by][dplyr::group_by], [summarise][dplyr::summarise], [select][dplyr::select], [left_join][dplyr::left_join], [distinct][dplyr::distinct], [rename][dplyr::rename]
#' @rdname norm_extrapolate
#' @export 
#' @importFrom dplyr filter mutate across group_by summarise select left_join distinct rename
norm_extrapolate <- function(x, f, extrap_type, log_path){
  #weight Group that needs extrapolation based on similar species/subtype
  x$extrapolate = x$raw %>% dplyr::filter(is.na(!!as.symbol(extrap_type)))
  x$raw = x$raw %>% dplyr::filter(!tempID %in% x$extrapolate$tempID)
  
  #Extrapolate weights
  if(nrow(x$extrapolate)){
    if(extrap_type == "weight"){
      message("...extrapolating ", extrap_type)
      #Extrapolate
      #Average of species and subtype match or just species
      map_spec_sub = db_query_cvt("SELECT DISTINCT species, subtype, weight_kg FROM cvt.subjects") %>%
        dplyr::mutate(dplyr::across(c(species, subtype), ~tolower(trimws(.)))) %>%
        dplyr::group_by(species, subtype) %>%
        dplyr::summarise(avg_weight_kg = mean(weight_kg, na.rm=TRUE))
      map_spec = db_query_cvt("SELECT DISTINCT species, subtype, weight_kg FROM cvt.subjects") %>%
        dplyr::mutate(dplyr::across(c(species, subtype), ~tolower(trimws(.)))) %>%
        dplyr::group_by(species) %>%
        dplyr::summarise(avg_weight_kg = mean(weight_kg, na.rm=TRUE))
      #20
      x$extrapolate = x$extrapolate %>% dplyr::mutate(dplyr::across(c(species, subtype), ~tolower(trimws(.))))
      x$extrap_spec_sub = x$extrapolate %>%
        dplyr::select(-weight_kg) %>%
        dplyr::left_join(map_spec_sub, by=c("species", "subtype")) %>%
        dplyr::filter(!is.na(avg_weight_kg)) %>%
        dplyr::distinct() %>%
        dplyr::mutate(weight_estimated = 1) %>%
        dplyr::rename(weight_kg = avg_weight_kg)
      x$extrapolate = x$extrapolate %>% dplyr::filter(!tempID %in% x$extrap_spec_sub$tempID)
      x$extrap_spec = x$extrapolate %>%
        dplyr::select(-weight_kg) %>%
        dplyr::left_join(map_spec, by=c("species")) %>%
        dplyr::filter(!is.na(avg_weight_kg)) %>%
        dplyr::distinct() %>%
        dplyr::mutate(weight_estimated = 1) %>%
        dplyr::rename(weight_kg = avg_weight_kg)
      x$extrapolate = x$extrapolate %>% dplyr::filter(!tempID %in% x$extrap_spec$tempID)
      #Always end up with a weight of some kind, then weight_bool it
      if(nrow(x$extrapolate)){
        message(paste0("...Unhandled extrapolation cases for: ", extrap_type))
        log_CvT_doc_load(f=f, m=paste0(extrap_type,"_extrapolation_attempt_failed"),
                         log_path=log_path)
      }
    }
  }
  return(x)
}
