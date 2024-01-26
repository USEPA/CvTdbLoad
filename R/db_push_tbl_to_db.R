#' @title db_push_tbl_to_db
#' @description FUNCTION_DESCRIPTION
#' @param dat PARAM_DESCRIPTION, Default: NULL
#' @param tblName PARAM_DESCRIPTION, Default: NULL
#' @param fieldTypes PARAM_DESCRIPTION, Default: NULL
#' @param overwrite PARAM_DESCRIPTION, Default: FALSE
#' @param customSQL PARAM_DESCRIPTION, Default: NULL
#' @param append PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [dbWriteTable][RPostgreSQL::dbWriteTable], [dbSendQuery][RPostgreSQL::dbSendQuery], [dbDisconnect][RPostgreSQL::dbDisconnect]
#' @rdname db_push_tbl_to_db
#' @export 
#' @importFrom RPostgreSQL dbWriteTable dbSendQuery dbDisconnect
db_push_tbl_to_db <- function(dat=NULL, tblName=NULL, fieldTypes=NULL, overwrite=FALSE,
                           customSQL=NULL, append=FALSE){
  if(is.null(dat)) stop("...Error: User must provide data to write to database")
  if(is.null(tblName)) stop("...Error: User must provide a name for the database table")
  
  con = db_connect_to_CvT()
  # DBI Issues with schema references
  # https://github.com/r-dbi/odbc/issues/140
  DBI::dbExecute(con, "SET search_path = cvt")
  
  out <- tryCatch({
    message("...Trying to write, '", tblName, "' to CvTdb")
    RPostgreSQL::dbWriteTable(con, value=dat, name=tblName, overwrite=overwrite,
                 field.types=fieldTypes, row.names=FALSE, append=append)
    if(!is.null(customSQL)) { RPostgreSQL::dbSendQuery(con, customSQL) } #Send custom SQL statement
  },
  error=function(cond) { 
    message("...Error message for ",tblName,": ", cond); return(NA)
    },
  warning=function(cond) { 
    message("...Warning message for ",tblName,": ", cond); return(NULL)
    },
  finally={ RPostgreSQL::dbDisconnect(con)
  })
  return(0)
}
