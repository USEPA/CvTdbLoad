#' @title update_tk_params
#' @description Push tk_parameters table updates.
#' @param tk_input String filepath to the input file used for tk params
#' @param tk_output String filepath to the output file of tk params
#' @param invivopkfit_version Version of invivopkfit package, Default: 'TBD_default'
#' @return None. Data is processed and database updates are made.
#' @seealso 
#'  \code{\link[readr]{read_delim}}, \code{\link[readr]{cols}}
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{filter}}
#' @rdname update_tk_params
#' @export 
#' @importFrom readr read_delim read_csv cols
#' @importFrom dplyr select distinct mutate n left_join filter
update_tk_params <- function(tk_input="input\\tk params\\cvtdb_invivopkfit_04262022-2.csv",#"input\\tk params\\SupTable-CvTData.txt", 
                 tk_output="input\\tk params\\SupTable-TKFits.txt",
                 invivopkfit_version = "TBD_default"){
  
  stop("Function still under development...")
  
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
    dplyr::select(DTXSID, Compound, CAS, Species, Reference, tk_model = Model, halflife) %>%
    dplyr::distinct() %>%
    dplyr::mutate(tk_param_id_tmp = 1:dplyr::n(),
                  invivopkfit_version = invivopkfit_version,
                  version = 1,
                  omit_from_ccd = 0)
  
  if(!"rmse" %in% names(tk_output)){
    tk_output$rmse = "missing_rmse"
  }
  
  message("Code using bad assumption for now to join input and output by chemical information")
  warning("Code using bad assumption for now to join input and output by chemical information")
  
  out = tk_input %>%
    dplyr::left_join(tk_output, 
              by=c("dsstox_substance_id"="DTXSID", "dsstox_casrn"="CAS", "species"="Species")
              ) %>%
    dplyr::filter(!is.na(Model))
  
  # Get tk_params table insert
  tk_params = out %>%
    dplyr::select(tk_param_id_tmp, tk_model, rmse, invivopkfit_version, version, omit_from_ccd) %>%
    dplyr::distinct()
  
  tk_parameters_series = out %>%
    dplyr::select(fk_tk_parameters_id = tk_param_id_tmp, fk_series_id, version) %>%
    dplyr::distinct()
  
  tk_parameters_studies = out %>%
    dplyr::select(fk_tk_parameters_id = tk_param_id_tmp, fk_study_id, version) %>%
    dplyr::distinct()
  
  # Upload tk-params table to database
  
  # Pull newly generated tk-params ID values
  
  # Map/substitute new tk-params ID values for tk_param_id_tmp
  
  # Push to tk_parameters_series table
  
  # Push to tk_parameters_studies table
}



