#' @description Function to return a dataframe of files ready to push due to all
#' flags being '0'.
#' @title FUNCTION_TITLE
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
#'  [filter][dplyr::filter], [across][dplyr::across]
#' @rdname get_cvt_push_ready
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter across
get_cvt_push_ready <- function(){
  readxl::read_xlsx("output\\template_normalization_log.xlsx") %>% 
    dplyr::filter(dplyr::across(.cols=names(.)[!names(.) %in% c("filename", "timestamp")], 
                  .fns = ~. == 0)) %>%
    return()
}
