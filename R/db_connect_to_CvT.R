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
#'  [dbConnect][RPostgreSQL::dbConnect], [PostgreSQL][RPostgreSQL::PostgreSQL]
#' @rdname db_connect_to_CvT
#' @export 
#' @importFrom RPostgreSQL dbConnect PostgreSQL
db_connect_to_CvT <- function(){
  # dbConnect(RSQLite::SQLite(), 
  #           "L:\\Lab\\HEM\\T_Wall_Projects_FY20\\CvT Database\\input\\sql dump\\CvTdb_20210825.sqlite") %>%
  #   return()#"CvTdb_20210408.sqlite"))
  RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), 
            user = Sys.getenv("user"), 
            password = Sys.getenv("pass"), #
            host = Sys.getenv("host"), #
            dbname = Sys.getenv("dbname")) %>%
    return()
}
