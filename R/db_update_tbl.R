#' @description A function to update database table entries based on dataframe
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
#'  [dbWriteTable][RPostgreSQL::dbWriteTable], [dbClearResult][RPostgreSQL::dbClearResult], [dbDisconnect][RPostgreSQL::dbDisconnect]
#'  [dbSendStatement][DBI::dbSendStatement]
#' @rdname db_push_to_CvT
#' @export 
#' @importFrom RPostgreSQL dbWriteTable dbClearResult dbDisconnect
#' @importFrom DBI dbSendStatement
db_update_tbl <- function(df=NULL, tblName=NULL){
  # Check input params
  if(is.null(df)) stop("Must provide a dataframe to push to database")
  if(is.null(tblName)) stop("Must provide database table name to write to")
  #Remove ID column because it'll be auto assigned in the push
  if(!nrow(df)){
    message("No data passed to push to CvT...returning")
    return()
  }
  # Filter out NA fields (which will automatically be NULL in the database)
  df = df[ , colSums(is.na(df)) < nrow(df)]
  tryCatch({
    # Drop temp table if exists
    db_query_cvt("DROP TABLE IF EXISTS cvt.z_updated_df")
    # Create temp table to store data for update
    db_query_cvt(paste0("CREATE TABLE cvt.z_updated_df (LIKE cvt.",tblName," INCLUDING ALL)"))
    
    con = db_connect_to_CvT()
    # DBI Issues with schema references
    # https://github.com/r-dbi/odbc/issues/140
    DBI::dbExecute(con, "SET search_path = cvt")
    RPostgreSQL::dbWriteTable(con, value = df, name="z_updated_df", append=TRUE, row.names=FALSE)  
    # 
    # updateQuery = paste0("UPDATE cvt.", tblName, " a INNER JOIN cvt.z_updated_df b ",
    #                      "ON a.id = b.id SET ",
    #                      paste0("a.", names(df)[!names(df) %in% c("id")],
    #                             " = b.", names(df)[!names(df) %in% c("id")],
    #                             collapse = ", "),
    #                      " WHERE a.id in (", toString(df$id),")"
    #                      )
    # 
    updateQuery = paste0("UPDATE cvt.", tblName, " AS a SET ",
                         paste0(names(df)[!names(df) %in% c("id")],
                                " = b.", names(df)[!names(df) %in% c("id")],
                                collapse = ", "),
                         " FROM cvt.z_updated_df AS b ",
                         "WHERE a.id = b.id AND a.id in (", toString(df$id),")"
    )
    
    DBI::dbSendStatement(con, updateQuery) %T>% 
      RPostgreSQL::dbClearResult()
    
    DBI::dbSendStatement(con, "DROP TABLE cvt.z_updated_df") %T>% 
      RPostgreSQL::dbClearResult() #Drop temporary table
  },
  error=function(cond){ message("Error message: ", cond); return(NA) },
  warning=function(cond){ message("Warning message: ", cond); return(NULL) },
  finally={ RPostgreSQL::dbDisconnect(con) }
  )
}
