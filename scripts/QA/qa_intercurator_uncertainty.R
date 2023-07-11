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
  
  # Set seed for reproducibility
  set.seed(1)
  
  # Calculate AUC per series for curator_1
  curator_1_out <- lapply(unique(curator_1_data$Conc_Time_Values$fk_series_id), function(series_id){
    tmp <- curator_1_data$Conc_Time_Values %>%
      filter(fk_series_id == series_id) %>%
      dplyr::mutate(dplyr::across(c(time, conc), ~as.numeric(.)))
    
    tmp2 <- curator_1_data$Series %>%
      dplyr::filter(id == series_id) %>%
      dplyr::mutate(dplyr::across(c(x_min, x_max, y_min, y_max), ~as.numeric(.)))
    
    list(auc = colMeans(caTools::colAUC(tmp$time, tmp$conc)),
         plotArea = (tmp2$x_max-tmp2$x_min)*(tmp2$y_max-tmp2$y_min)
    )
  }) %T>% {
    names(.) <- unique(curator_1_data$Conc_Time_Values$fk_series_id)
  }
  
  # Calculate AUC per series for curator_2
  curator_2_out <- lapply(unique(curator_2_data$Conc_Time_Values$fk_series_id), function(series_id){
    tmp <- curator_2_data$Conc_Time_Values %>%
      filter(fk_series_id == series_id) %>%
      dplyr::mutate(dplyr::across(c(time, conc), ~as.numeric(.)))
    
    tmp2 <- curator_2_data$Series %>%
      dplyr::filter(id == series_id) %>%
      dplyr::mutate(dplyr::across(c(x_min, x_max, y_min, y_max), ~as.numeric(.)))
    
    list(auc = colMeans(caTools::colAUC(tmp$time, tmp$conc)),
         plotArea = (tmp2$x_max-tmp2$x_min)*(tmp2$y_max-tmp2$y_min)
    )
  }) %T>% {
    names(.) <- unique(curator_2_data$Conc_Time_Values$fk_series_id)
  }
  
  # Compare each series AUC and normalize by plotArea
  out <- data.frame()
  for(series_id in names(curator_1_out)){
    if(series_id %in% names(curator_2_out)){
      if(curator_1_out[[series_id]]$plotArea == curator_2_out[[series_id]]$plotArea){
       out = data.frame(
          series_id = series_id,
          curator_1_auc = curator_1_out[[series_id]]$auc,
          curator_2_auc = curator_2_out[[series_id]]$auc,
          plotArea = curator_1_out[[series_id]]$plotArea,
          AUCFracVar = round((curator_1_out[[series_id]]$auc - curator_2_out[[series_id]]$auc) / curator_1_out[[series_id]]$plotArea, 3)
        ) %>%
          rbind(out, .)
      } else {
        message("Skipping comparison of series '", series_id, "' due to incompatible plotArea values...")
      }
    }
  }
  
  return(list(curator_1_data = curator_1_data,
              curator_2_data = curator_2_data,
              curator_1_out = curator_1_out,
              curator_2_out = curator_2_out,
              out=out))
}

get_intercurator_data <- function(in_data){
  
  # Check if input filepath exists if not dataframe
  if(!is.list(in_data)){
    if(!file.exists(in_data)){
      stop("File path '", in_data,"' does not exist...")
    }
    # Load input
    if(strsplit(basename(in_data), split="\\.")[[1]][2] != "xlsx"){
      stop("Expected input file type of XLSX...")
    }
    in_data <- load_sheet_group(in_data)
  }
  
  if(!all(c("Series", "Conc_Time_Values") %in% names(in_data))){
    stop("Input dataframe list must contain named 'Series' and 'Conc_Time_Values' list elements")
  }
  
  # Return dataframe
  return(in_data)
}
