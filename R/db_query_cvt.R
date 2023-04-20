#' @description A helper function to query cvt and receive the results. 
#' Handles errors/warnings with tryCatch.
#' @param query A SQL query string to query the database with
#' @import DBI dplyr magrittr
#' @return Dataframe of database query results
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [dbGetQuery][RPostgreSQL::dbGetQuery], [dbDisconnect][RPostgreSQL::dbDisconnect]
#' @rdname db_query_cvt
#' @export 
#' @importFrom RPostgreSQL dbGetQuery dbDisconnect
db_query_cvt <- function(query=NULL){
  if(is.null(query)) return(message("...Must provide a query to send"))
  con = db_connect_to_CvT()
  query_result = tryCatch({
    RPostgreSQL::dbGetQuery(con, query)
    #dbSendQuery(con, query) %T>% #run query
    #{ dbFetch(.) ->> tmp } %>% #save intermediate variable, critical tee-operator
    #  dbClearResult() #clear result
    #tmp #return query results
  },
  error=function(cond){ message("...Error message: ", cond); return(NA) },
  warning=function(cond){ message("...Warning message: ", cond); return(NULL) },
  finally={ RPostgreSQL::dbDisconnect(con) })
  return(query_result)
}
