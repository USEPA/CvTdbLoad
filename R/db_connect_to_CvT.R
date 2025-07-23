#' @title db_connect_to_CvT
#' @description A function to create a connection to the CvT database using the .Renviron file parameters.
#' @return A database connection object.
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
