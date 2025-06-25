#' @description A function to push a dataframe to a specified table in a database
#' @param df A dataframe to write to the database
#' @param tblName The name of the table to write the df data to
#' @import DBI
#' @return None
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [dbWriteTable][DBI::dbWriteTable], [dbClearResult][DBI::dbClearResult], [dbDisconnect][DBI::dbDisconnect]
#'  [dbSendStatement][DBI::dbSendStatement]
#' @rdname db_push_to_CvT
#' @export 
#' @importFrom DBI dbSendStatement dbWriteTable dbClearResult dbDisconnect
db_push_to_CvT <- function(df=NULL, tblName=NULL){
  
  stop("Deprecated function in favor of db_push_tbl_to_db...")
  return()
  
  if(is.null(df)) stop("Must provide a dataframe to push to database")
  if(is.null(tblName)) stop("Must provide database table name to write to")
  #Remove ID column because it'll be auto assigned in the push
  if(!nrow(df)){
    message("No data passed to push to CvT...returning")
    return()
  }
  df = df[!names(df) %in% c("id")]
  # Filter out NA fields (which will automatically be NULL in the database)
  df = df[ , colSums(is.na(df)) < nrow(df)]
  tryCatch({
    con = db_connect_to_CvT()
    # DBI Issues with schema references
    # https://github.com/r-dbi/odbc/issues/140
    DBI::dbExecute(con, "SET search_path = cvt")
    DBI::dbWriteTable(con, value = df, name="temp_tbl", overwrite=TRUE, row.names=FALSE)  
    
    DBI::dbSendStatement(con, paste0("INSERT INTO cvt.", tblName, " (", paste0(names(df), collapse=','),
                                ") SELECT ", paste0(names(df), collapse=','), " FROM cvt.temp_tbl")) %T>% 
      DBI::dbClearResult()
    
    DBI::dbSendStatement(con, "DROP TABLE cvt.temp_tbl") %T>% 
      DBI::dbClearResult() #Drop temporary table
  },
  error=function(cond){ message("Error message: ", cond); return(NA) },
  warning=function(cond){ message("Warning message: ", cond); return(NULL) },
  finally={ DBI::dbDisconnect(con) }
  )
}
