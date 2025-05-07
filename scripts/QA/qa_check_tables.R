#Script to check cvtdb table linkages and identify any missing links
#Created by: Jonathan Taylor Wall
#Created Date: 2022-02-08
#Load packages
require(DBI); require(dplyr); require(magrittr); require(tidyr); require(readxl)
#Load R Scripts
#Load R Scripts
file_source = list.files("scripts/initial processing", pattern="utils_", full.names = TRUE)
invisible(sapply(file_source, source,.GlobalEnv))

#Conc without series link
conc_missing_series = db_query_cvt("SELECT * FROM cvt.conc_time_values WHERE fk_series_id IS NULL")
#Get list of series linked to conc_time_values table
conc_series = db_query_cvt("SELECT DISTINCT fk_series_id FROM cvt.conc_time_values WHERE fk_series_id IS NOT NULL")
#Series without Conc Information
no_conc_series = db_query_cvt(paste0("SELECT id, fk_study_id, fk_subject_id FROM cvt.series WHERE id not in (",
                                  toString(conc_series$fk_series_id),")"))
# Series without conc Information but with tk_params information
no_conc_series_tk_params = db_query_cvt(paste0("SELECT id, fk_study_id, fk_subject_id FROM cvt.series WHERE id not in (",
                                     toString(conc_series$fk_series_id),") and ",
                                     "id in (SELECT fk_series_id FROm cvt.tk_parameters)"))
#Series without study information
no_study_series = db_query_cvt("SELECT * FROM cvt.series where fk_study_id IS NULL")
#Series without subject information
no_subject_series = db_query_cvt("SELECT * FROM cvt.series where fk_subject_id IS NULL")
#Studies not connected to a document
no_doc_study = db_query_cvt("SELECT * FROM cvt.studies WHERE fk_extraction_document_id IS NULL AND fk_reference_document_id IS NULL")
no_ext_doc_study = db_query_cvt("SELECT * FROM cvt.studies WHERE fk_extraction_document_id IS NULL")
no_ref_doc_study = db_query_cvt("SELECT * FROM cvt.studies WHERE fk_reference_document_id IS NULL")
#Studies not connected to a series
no_series_study = db_query_cvt("SELECT * FROM cvt.studies WHERE id NOT in (SELECT DISTINCT fk_study_id FROM cvt.series)")
#Subjects not connected to a series
no_series_subject = db_query_cvt("SELECT * FROM cvt.subjects WHERE id NOT in (SELECT DISTINCT fk_subject_id FROM cvt.series)")

cat("---Report Start ---\n",
  paste0(nrow(conc_missing_series), " conc entries without a series link\n"),
  nrow(no_conc_series)," series without conc data connected to ", 
        length(unique(no_conc_series$fk_study_id)), " studies and ", 
        length(unique(no_conc_series$fk_subject_id))," subjects\n",
  nrow(no_conc_series_tk_params), " series without conc data but tk_parameters connection for ",  
        length(unique(no_conc_series_tk_params$fk_study_id)), " studies and ", 
        length(unique(no_conc_series_tk_params$fk_subject_id))," subjects\n",
  nrow(no_study_series), " series without a study link\n",
  nrow(no_study_series), " series without a subject link\n",
  nrow(no_doc_study), " studies without any document link\n",
  nrow(no_ref_doc_study), " studies without a reference document link\n",
  nrow(no_ext_doc_study), " studies without an extraction document link\n",
  nrow(no_series_study), " studies without a series link\n",
  nrow(no_series_subject), " subjects without a series link\n",
  "--- Report End ---")
