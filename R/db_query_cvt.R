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
#'  [dbGetQuery][RPostgres::dbGetQuery], [dbDisconnect][RPostgres::dbDisconnect]
#' @rdname db_query_cvt
#' @export 
#' @importFrom RPostgres dbGetQuery dbDisconnect
db_query_cvt <- function(query=NULL, query_type = "query"){
  if(is.null(query)) return(message("...Must provide a query to send"))
  con = db_connect_to_CvT()
  query_result = tryCatch({
    if(query_type == "query"){
      RPostgres::dbGetQuery(con, query)  
    } else if (query_type == "statement"){
      rs = RPostgres::dbSendStatement(con, query)
      dbClearResult(rs)
    } else {
      stop("Unknown query_type '", query_type, "'")
    }
    
    
    #dbSendQuery(con, query) %T>% #run query
    #{ dbFetch(.) ->> tmp } %>% #save intermediate variable, critical tee-operator
    #  dbClearResult() #clear result
    #tmp #return query results
  },
  error=function(cond){ message("...Error message: ", cond); return(NA) },
  warning=function(cond){ message("...Warning message: ", cond); return(NULL) },
  finally={ RPostgres::dbDisconnect(con) })
  return(query_result)
}
