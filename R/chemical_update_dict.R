#' @title chemical_update_dict
#' @description FUNCTION_DESCRIPTION
#' @param dict_file PARAM_DESCRIPTION, Default: NULL
#' @param dat PARAM_DESCRIPTION, Default: NULL
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
#' @rdname chemical_update_dict
#' @export 
#' @importFrom readxl read_xlsx
chemical_update_dict <- function(dict_file=NULL, dat=NULL){
  if(!is.null(dict_file)){
    if(file.exists(dict_file)){
      db_push_tbl_to_db(dat=readxl::read_xlsx(dict_file),
                     tblName="cvt.chemicals",
                     append=TRUE)
    } else {
      message("...input dict_file does not exist...cannot push dictionary")
    } 
  }
  
  if(!is.null(dat)){
    db_push_tbl_to_db(dat=dat,
                   tblName="cvt.chemicals",
                   overwrite=FALSE, append=TRUE)
  }
}
