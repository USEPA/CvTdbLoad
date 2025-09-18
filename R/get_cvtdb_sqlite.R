#' @title get_cvtdb_sqlite
#' @description Pull CvTdb tables and write sqlite file.
#' @param schema Database postgreSQL schema.
#' @param outdir String for desired output directory.
#' @param exclude_audit Boolean whether to exclude the "cvt_audit" table from the SQLite file due to size. Default is False.
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
get_cvtdb_sqlite <- function(schema, outdir, exclude_audit = FALSE){
  message("Getting CvT sqlite...")
  n_file = file.path(outdir, "res_cvtdb.sqlite")
  
  if(exclude_audit){
    n_file = file.path(outdir, "res_cvtdb_no_audit.sqlite")
    audit_file = file.path(outdir, "audit.parquet")
  }
  
  if(file.exists(n_file)) file.remove(n_file)
  
  table_list = db_query_cvt(paste0("SELECT distinct table_name FROM information_schema.tables ",
                                   "WHERE table_schema = '", schema, "'")) %>%
    dplyr::pull(table_name)
  
  con = DBI::dbConnect(RSQLite::SQLite(), dbname = n_file)
  
  for(tbl in table_list){
    tbl_df = db_query_cvt(paste0("SELECT * FROM ", schema, ".", tbl))
    # If exclude_audit, store as arrow file
    if(tbl == "cvt_audit" & exclude_audit){
      message("...Writing table ", tbl, " to parquet file...")
      arrow::write_parquet(tbl_df, audit_file)
      # Skip adding to SQLite file
      next
    }
    message("...Writing table ", tbl, " to sqlite file...")
    DBI::dbWriteTable(conn = con,
                     name = tbl,
                     value = tbl_df)
  }
  DBI::dbDisconnect(con)
  message("Done.")
}
