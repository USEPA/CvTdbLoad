#' @title get_cvtdb_sqlite
#' @description Pull CvTdb tables and write sqlite file.
#' @param schema Database postgreSQL schema.
#' @param outdir String for desired output directory.
#' @return None. SQLite file is written to `outdir` folder.
#' @seealso 
#'  \code{\link[dplyr]{pull}}
#'  \code{\link[DBI]{dbConnect}}, \code{\link[DBI]{dbWriteTable}}, \code{\link[DBI]{dbDisconnect}}
#'  \code{\link[RSQLite]{SQLite}}
#' @rdname get_cvtdb_sqlite
#' @export 
#' @importFrom dplyr pull
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom RSQLite SQLite
get_cvtdb_sqlite <- function(schema, outdir){
  message("Getting CvT sqlite...")
  n_file = file.path(outdir, "res_cvtdb.sqlite")
  
  if(file.exists(n_file)) file.remove(n_file)
  
  table_list = db_query_cvt(paste0("SELECT distinct table_name FROM information_schema.tables ",
                                   "WHERE table_schema = '", schema, "'")) %>%
    dplyr::pull(table_name)
  
  con = DBI::dbConnect(RSQLite::SQLite(), dbname = n_file)
  
  for(tbl in table_list){
    message("...Writing table ", tbl)
    DBI::dbWriteTable(conn = con,
                     name = tbl,
                     value = db_query_cvt(paste0("SELECT * FROM ", schema, ".", tbl)))
  }
  DBI::dbDisconnect(con)
  message("Done.")
}
