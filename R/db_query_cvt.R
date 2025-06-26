#' @title db_query_cvt
#' @description Function to query or send statements to the database. Handles errors/warnings with tryCatch.
#' @param query A SQL query string.
#' @param query_type Input string specifying if the query is a SQL "query" (retrieve data) or "statement" (update database). Default: 'query'.
#' @return Return objects from DBI `dbGetQuery()` and `dbSendStatement()` functions.
#' @seealso 
#'  [dbGetQuery][DBI::dbGetQuery], [dbSendStatement][DBI::dbSendStatement], [dbDisconnect][DBI::dbDisconnect]
#' @rdname db_query_cvt
#' @export 
#' @importFrom DBI dbGetQuery dbDisconnect
db_query_cvt <- function(query=NULL, query_type = "query"){
  if(is.null(query)) return(message("...Must provide a query to send"))
  con = db_connect_to_CvT()
  query_result = tryCatch({
    if(query_type == "query"){
      DBI::dbGetQuery(con, query)  
    } else if (query_type == "statement"){
      rs = DBI::dbSendStatement(con, query)
      dbClearResult(rs)
    } else {
      stop("Unknown query_type '", query_type, "'")
    }
  },
  error=function(cond){ message("...Error message: ", cond); return(NA) },
  warning=function(cond){ message("...Warning message: ", cond); return(NULL) },
  finally={ DBI::dbDisconnect(con) })
  return(query_result)
}
