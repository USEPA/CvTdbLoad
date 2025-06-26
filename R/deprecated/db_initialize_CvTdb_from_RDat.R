#' @description Function to write empty tables for CvTdb SQLite file from template docs.
#' @title FUNCTION_TITLE
#' @param file PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname db_initialize_CvTdb_from_RDat
#' @export 
db_initialize_CvTdb_from_RDat <- function(file=NULL){
  load(file)
  for(t in names(cvt_dat)){
    db_push_tbl_to_db(dat=cvt_dat[[t]],
                   tblName=t,
                   overwrite=TRUE,
                   fieldTypes = NULL)  
  }
}
