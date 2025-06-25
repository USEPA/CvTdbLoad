#' @description A function to create a connection to the CvT database
#' @import DBI
#' @return A database connection object
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [dbConnect][DBI::dbConnect], [PostgreSQL][RPostgres::Postgres]
#' @rdname db_connect_to_CvT
#' @export 
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect
db_connect_to_CvT <- function(){
  DBI::dbConnect(RPostgres::Postgres(), 
            user = Sys.getenv("user"), 
            password = Sys.getenv("pass"), #
            host = Sys.getenv("host"), #
            dbname = Sys.getenv("dbname")) %>%
    return()
}
