#' @#'@description A function to pull table fk identification from a specified table
#'by a SQL filter statement, or the entire table.
#'@param tblName The name of the table to pull the ID from
#'@param idFilter A SQL WHERE statement to filter idName column to. If empty, pulls all data.
#'@import DBI
#'@return A list of ID values from the specified database table
#' @description FUNCTION_DESCRIPTION
#' @title FUNCTION_TITLE
#' @param tblName PARAM_DESCRIPTION, Default: NULL
#' @param idFilter PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [dbSendQuery][RPostgres::dbSendQuery], [dbClearResult][RPostgres::dbClearResult], [dbDisconnect][RPostgres::dbDisconnect]
#'  [dbFetch][DBI::dbFetch]
#' @rdname db_get_tbl_id
#' @export 
#' @importFrom RPostgres dbSendQuery dbClearResult dbDisconnect
#' @importFrom DBI dbFetch
db_get_tbl_id <- function(tblName=NULL, idFilter=NULL){
  if(is.null(tblName)) stop("Must provide database table name to write to")
  if(is.null(idFilter)) stop("Must provide an idFilter value to filter ID table by")
  #Remove ID column because it'll be auto assigned in the push
  tryCatch({
    con = db_connect_to_CvT()
    RPostgres::dbSendQuery(con, paste0("SELECT * FROM cvt.", tblName, " ", idFilter)) %T>%
      { DBI::dbFetch(.) ->> tmp } %>%
      RPostgres::dbClearResult()
    return(tmp)
  },
  error=function(cond){ message("Error message: ", cond); return(NA) },
  warning=function(cond){ message("Warning message: ", cond); return(NULL) },
  finally={ RPostgres::dbDisconnect(con) }
  )
}
