#' @title db_push_tbl_to_db
#' @description A function to push a dataframe to a specified table in the database.
#' @param dat A dataframe to write to the database, Default: NULL
#' @param tblName Name of database table to create or append and fill with input `dat` dataframe data, Default: NULL
#' @param fieldTypes Named list of field types for columns, Default: NULL
#' @param overwrite Boolean of whether to overwrite the `tblName` table with input `dat` dataframe data, Default: FALSE
#' @param customSQL Optional custom SQL statement to push, Default: NULL
#' @param append Boolean of whether to append the `tblName` table with input `dat` dataframe data, Default: FALSE
#' @return None. Updates are pushed to the database.
#' @seealso 
#'  [dbWriteTable][DBI::dbWriteTable], [dbSendQuery][DBI::dbSendQuery], [dbDisconnect][DBI::dbDisconnect]
#' @rdname db_push_tbl_to_db
#' @export 
#' @importFrom DBI dbWriteTable dbSendQuery dbDisconnect
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
    DBI::dbWriteTable(con, value=dat, name=tblName, overwrite=overwrite,
                 field.types=fieldTypes, row.names=FALSE, append=append)
    if(!is.null(customSQL)) { DBI::dbSendQuery(con, customSQL) } #Send custom SQL statement
  },
  error=function(cond) { 
    message("...Error message for ",tblName,": ", cond); return(NA)
    },
  warning=function(cond) { 
    message("...Warning message for ",tblName,": ", cond); return(NULL)
    },
  finally={ DBI::dbDisconnect(con)
  })
  return(0)
}
