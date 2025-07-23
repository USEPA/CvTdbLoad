#' @title qa_intercurator_uncertainty
#' @description QA function to compare the extracted Conc_Time_Values for Series entries by curators.
#' @param curator_1 File path or template DataFrame list for an extraction template for one curator
#' @param curator_2 File path or template DataFrame list for an extraction template for another curator
#' @return A list of dataframes of input, intermediates, and output
#' @seealso 
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}
#'  \code{\link[pracma]{trapz}}
#' @rdname qa_intercurator_uncertainty
#' @export 
#' @importFrom dplyr filter mutate across
#' @importFrom pracma trapz
#' @importFrom purrr compact
qa_intercurator_uncertainty <- function(curator_1, curator_2){
  # Check if inputs are NULL
  if(is.null(curator_1) | is.null(curator_2)){
    stop("Parameter curator_1 and curator_2 cannot be NULL")
  }
  
  # Check if input is a string or data.frame
  if(!is.character(curator_1) & !is.data.frame(curator_1)){
    stop("Parameter curator_1 must be a filepath or dataframe")
  }
  # Check if input is a string or data.frame
  if(!is.character(curator_2) & !is.data.frame(curator_2)){
    stop("Parameter curator_1 must be a filepath or dataframe")
  }
  
  # Load curator data
  curator_1_data <- get_intercurator_data(curator_1)
  curator_2_data <- get_intercurator_data(curator_2)
  
  # Calculate AUC and plotArea per series for curator_1
  curator_1_out <- lapply(unique(curator_1_data$Conc_Time_Values$fk_series_id), function(series_id){
    # Filter to series
    tmp <- curator_1_data$Conc_Time_Values %>%
      dplyr::filter(fk_series_id == series_id) %>%
      dplyr::mutate(dplyr::across(c(time, conc), ~as.numeric(.)))
    
    tmp2 <- curator_1_data$Series %>%
      dplyr::filter(id == series_id) %>%
      dplyr::mutate(dplyr::across(c(x_min, x_max, y_min, y_max), ~as.numeric(.)))
    
    list(auc = pracma::trapz(tmp$time, tmp$conc),
         plotArea = (tmp2$x_max-tmp2$x_min)*(tmp2$y_max-tmp2$y_min)
    )
  }) %T>% {
    names(.) <- unique(curator_1_data$Conc_Time_Values$fk_series_id)
  }
  
  # Calculate AUC and plotArea per series for curator_2
  curator_2_out <- lapply(unique(curator_2_data$Conc_Time_Values$fk_series_id), function(series_id){
    # Filter to series
    tmp <- curator_2_data$Conc_Time_Values %>%
      dplyr::filter(fk_series_id == series_id) %>%
      dplyr::mutate(dplyr::across(c(time, conc), ~as.numeric(.)))
    
    tmp2 <- curator_2_data$Series %>%
      dplyr::filter(id == series_id) %>%
      dplyr::mutate(dplyr::across(c(x_min, x_max, y_min, y_max), ~as.numeric(.)))
    
    list(auc = pracma::trapz(tmp$time, tmp$conc),
         plotArea = (tmp2$x_max-tmp2$x_min)*(tmp2$y_max-tmp2$y_min)
    )
  }) %T>% {
    names(.) <- unique(curator_2_data$Conc_Time_Values$fk_series_id)
  }
  
  # Compare each series AUC and normalize by plotArea
  out <- lapply(names(curator_1_out), function(series_id){
    if(series_id %in% names(curator_2_out)){
      if(curator_1_out[[series_id]]$plotArea == curator_2_out[[series_id]]$plotArea){
        data.frame(
          series_id = series_id,
          curator_1_auc = curator_1_out[[series_id]]$auc,
          curator_2_auc = curator_2_out[[series_id]]$auc,
          plotArea = curator_1_out[[series_id]]$plotArea,
          AUCFracVar = round((curator_1_out[[series_id]]$auc - curator_2_out[[series_id]]$auc) / curator_1_out[[series_id]]$plotArea, 3)
        ) %>%
          dplyr::mutate(AUCFracVar_perc = AUCFracVar * 100) %>%
          return()
      } else {
        message("Skipping comparison of series '", series_id, "' due to incompatible plotArea values...")
        return(NULL)
      }
    }
  }) %>%
    # Remove NULL values
    purrr::compact() %>%
    # Combine into dataframe
    dplyr::bind_rows()
  
  # Return all input, intermediates, and output
  return(list(curator_1_data = curator_1_data,
              curator_2_data = curator_2_data,
              curator_1_out = curator_1_out,
              curator_2_out = curator_2_out,
              out=out))
}

#' @title get_intercurator_data
#' @description Function to validate the input intercurator params and load data as necessar.
#' @param in_data Input param for \code{qa_intercurator_uncertainty} function
#' @return Loaded input data in a named list of CvT template sheet dataframes
#' @rdname qa_intercurator_uncertainty
#' @export 
get_intercurator_data <- function(in_data){
  
  # Check if input filepath exists if not dataframe
  if(!is.list(in_data)){
    if(!file.exists(in_data)){
      stop("File path '", in_data,"' does not exist...")
    }
    # Load input, must be an XLSX file
    if(strsplit(basename(in_data), split="\\.")[[1]][2] != "xlsx"){
      stop("Expected input file type of XLSX...")
    }
    in_data <- load_sheet_group(in_data)
  }
  
  # Check of input list of dataframes, must contain Series and Conc_Time_Values named elements
  if(!all(c("Series", "Conc_Time_Values") %in% names(in_data))){
    stop("Input dataframe list must contain named 'Series' and 'Conc_Time_Values' list elements")
  }
  
  # Return dataframe
  return(in_data)
}