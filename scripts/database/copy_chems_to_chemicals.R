#' @title copy_chems_to_chemicals
#' @description Utility function used to create the chemicals table from series and studies table chemical identifier fields.
#' @return None. SQL queries and statements are executed.
#' @seealso 
#'  \code{\link[DBI]{dbSendStatement}}, \code{\link[DBI]{dbDisconnect}}
#' @rdname copy_chems_to_chemicals
#' @export 
#' @importFrom DBI dbSendStatement dbDisconnect
copy_chems_to_chemicals <- function(){
  schema_list = db_query_cvt(paste0("SELECT * FROM information_schema.columns WHERE table_schema = 'cvt'"))
  
  chem_dat = db_query_cvt("SELECT distinct fk_dosed_chemical_id, test_substance_name_original, test_substance_name_secondary_original, test_substance_casrn_original FROM cvt.studies")
  
  # Create temp table to store chemical information to copy
  db_query_cvt("CREATE TABLE cvt.zzz_tmp AS SELECT distinct fk_dosed_chemical_id, test_substance_name_original, test_substance_name_secondary_original, test_substance_casrn_original FROM cvt.studies")
  # Check data
  tmp = db_query_cvt("SELECT * FROM cvt.zzz_tmp")
  
  con = db_connect_to_CvT()
  updateQuery = paste0("UPDATE cvt.chemicals ",
                       "SET chemical_name_original = test_substance_name_original ",
                       "FROM cvt.zzz_tmp ",
                       "WHERE id = fk_dosed_chemical_id"
  )
  # Send update
  DBI::dbSendStatement(con, updateQuery)
  updateQuery = paste0("UPDATE cvt.chemicals ",
                       "SET chemical_name_secondary_original = test_substance_name_secondary_original ", 
                       "FROM cvt.zzz_tmp ",
                       "WHERE id = fk_dosed_chemical_id"
  )
  # Send update
  DBI::dbSendStatement(con, updateQuery)
  updateQuery = paste0("UPDATE cvt.chemicals ",
                       "SET casrn_original = test_substance_casrn_original ",
                       "FROM cvt.zzz_tmp ",
                       "WHERE id = fk_dosed_chemical_id"
  )
  # Send update
  DBI::dbSendStatement(con, updateQuery)
  # Drop temp table
  DBI::dbSendStatement(con, "DROP TABLE IF EXISTS cvt.zzz_tmp")
  DBI::dbDisconnect(con)
  
  tmp = db_query_cvt("SELECT * FROM cvt.chemicals LIMIT 1")
  tmp = db_query_cvt("SELECT * FROM cvt.chemicals where chemical_name_original is not null")
  
  ################################################################################
  ### Perform same with series table chemical values
  ################################################################################
  chem_dat = db_query_cvt("SELECT distinct fk_analyzed_chemical_id, analyte_name_original, analyte_name_secondary_original, analyte_casrn FROM cvt.series")
  
  # Create temp table to store chemical information to copy
  db_query_cvt("CREATE TABLE cvt.zzz_tmp AS SELECT distinct fk_analyzed_chemical_id, analyte_name_original, analyte_name_secondary_original, analyte_casrn FROM cvt.series")
  # Check data
  tmp = db_query_cvt("SELECT * FROM cvt.zzz_tmp")
  
  con = db_connect_to_CvT()
  updateQuery = paste0("UPDATE cvt.chemicals ",
                       "SET chemical_name_original = analyte_name_original ",
                       "FROM cvt.zzz_tmp ",
                       "WHERE id = fk_analyzed_chemical_id"
  )
  # Send update
  DBI::dbSendStatement(con, updateQuery)
  updateQuery = paste0("UPDATE cvt.chemicals ",
                       "SET chemical_name_secondary_original = analyte_name_secondary_original ", 
                       "FROM cvt.zzz_tmp ",
                       "WHERE id = fk_analyzed_chemical_id"
  )
  # Send update
  DBI::dbSendStatement(con, updateQuery)
  updateQuery = paste0("UPDATE cvt.chemicals ",
                       "SET casrn_original = analyte_casrn ",
                       "FROM cvt.zzz_tmp ",
                       "WHERE id = fk_analyzed_chemical_id"
  )
  # Send update
  DBI::dbSendStatement(con, updateQuery)
  # Drop temp table
  DBI::dbSendStatement(con, "DROP TABLE IF EXISTS cvt.zzz_tmp")
  DBI::dbDisconnect(con)
  
  tmp = db_query_cvt("SELECT * FROM cvt.chemicals LIMIT 1")
  tmp = db_query_cvt("SELECT * FROM cvt.chemicals where chemical_name_original is not null")
  
}
