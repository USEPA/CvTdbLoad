#' update_tk_params
#' @param tk_input String filepath to the input file used for tk params
#' @param tk_output String filepath to the output file of tk params
#' @return None. Data is processed and database updates are made.
update_tk_params <- function(tk_input="input\\tk params\\cvtdb_invivopkfit_04262022-2.csv",#"input\\tk params\\SupTable-CvTData.txt", 
                 tk_output="input\\tk params\\SupTable-TKFits.txt",
                 invivopkfit_version = "TBD_default"){
  # Load the input data
  if(grepl(".txt", tk_input)){
    tk_input = readr::read_delim(tk_input, show_col_types = FALSE)  
  } else if(grepl(".csv", tk_input)){
    tk_input = readr::read_csv(tk_input, col_types = readr::cols())
  } else { 
    return("Unsupported tk_input file type...")
  }
  
  # Load the output data
  tk_output = readr::read_delim(tk_output, show_col_types = FALSE) %>% 
    select(DTXSID, Compound, CAS, Species, Reference, tk_model = Model, halflife) %>%
    distinct() %>%
    dplyr::mutate(tk_param_id_tmp = 1:n(),
                  invivopkfit_version = invivopkfit_version,
                  version = 1,
                  omit_from_ccd = 0)
  
  if(!"rmse" %in% names(tk_output)){
    tk_output$rmse = "missing_rmse"
  }
  
  message("Code using bad assumption for now to join input and output by chemical information")
  warning("Code using bad assumption for now to join input and output by chemical information")
  
  out = tk_input %>%
    left_join(tk_output, 
              by=c("dsstox_substance_id"="DTXSID", "dsstox_casrn"="CAS", "species"="Species")
              ) %>%
    filter(!is.na(Model))
  
  # Get tk_params table insert
  tk_params = out %>%
    select(tk_param_id_tmp, tk_model, rmse, invivopkfit_version, version, omit_from_ccd) %>%
    distinct()
  
  tk_parameters_series = out %>%
    select(fk_tk_parameters_id = tk_param_id_tmp, fk_series_id, version) %>%
    distinct()
  
  tk_parameters_studies = out %>%
    select(fk_tk_parameters_id = tk_param_id_tmp, fk_study_id, version) %>%
    distinct()
  
  # Upload tk-params table to database
  
  # Pull newly generated tk-params ID values
  
  # Map/substitute new tk-params ID values for tk_param_id_tmp
  
  # Push to tk_parameters_series table
  
  # Push to tk_parameters_studies table
}



