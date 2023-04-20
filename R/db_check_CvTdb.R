#' @description A quick function to pull all database data into a dataframe list by table name.
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [dbListTables][RPostgreSQL::dbListTables], [dbDisconnect][RPostgreSQL::dbDisconnect]
#'  [tbl][dplyr::tbl], [collect][dplyr::collect]
#' @rdname db_check_CvTdb
#' @export 
#' @importFrom RPostgreSQL dbListTables dbDisconnect
#' @importFrom dplyr tbl collect
db_check_CvTdb <- function(){
  con = db_connect_to_CvT()
  t_list = RPostgreSQL::dbListTables(con)
  df = lapply(t_list, function(x){
    dplyr::tbl(con, x) %>% dplyr::collect()
  }) %T>% { names(.) <- t_list }
  RPostgreSQL::dbDisconnect(con)
  return(df)
}
