#' @title Get CvTdb SQLite
#' @description Pull CvTdb tables and write sqlite file
#' @param schema Database postgreSQL schema
#' @return None. SQLite file is written to output/release folder
get_cvtdb_sqlite <- function(schema){
  
  if(file.exists("output/release/res_cvtdb.sqlite")) file.remove("output/release/res_cvtdb.sqlite")
  
  table_list = db_query_cvt(paste0("SELECT distinct table_name FROM information_schema.tables ",
                                   "WHERE table_schema = '", schema, "'")) %>%
    dplyr::pull(table_name)
  
  con = DBI::dbConnect(RSQLite::SQLite(), dbname = "output/release/res_cvtdb.sqlite")
  
  for(tbl in table_list){
    message("Writing table ", tbl)
    DBI::dbWriteTable(conn = con,
                     name = tbl,
                     value = db_query_cvt(paste0("SELECT * FROM ", schema, ".", tbl)))
  }
  DBI::dbDisconnect(con)
}