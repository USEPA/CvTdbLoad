#' @description A helper function to match a new study entry to existing studies entries (if available).
#' In development, waiting to figure out bset way to match given string entries.
#' @title FUNCTION_TITLE
#' @param df PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [select][dplyr::select]
#' @rdname match_subject_fk
#' @export 
#' @importFrom dplyr select
match_subject_fk <- function(df){
  
  subjects = db_query_cvt("SELECT * FROM subjects") %>%
    dplyr::select(-source_system, -rec_update_dt, -rec_create_dt)
}
