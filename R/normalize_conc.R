#' @description A helper function to normalize concentration units.
#' @param raw A dataframe of weight information to normalize
#' @param f The file name of the template being processed. Used for error logging. #'
#' @return Normalized version of the input `raw` parameter.
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [mutate][dplyr::mutate], [filter][dplyr::filter], [across][dplyr::across], [bind_rows][dplyr::bind_rows], [arrange][dplyr::arrange]
#'  [POST][httr::POST]
#' @rdname normalize_conc
#' @export 
#' @importFrom dplyr mutate filter across bind_rows arrange
#' @importFrom httk get_physchem_param
normalize_conc <- function(raw, f, log_path, debug = FALSE){
  message("...normalizing conc...")
  # tmp = lapply(fileList, function(f){
  #   s_list = load_sheet_group(fileName = f, template_path = template_path)
  #   s_list$Series %>%
  #     left_join(s_list$Subjects %>%
  #                 select(id, species), by=c("fk_study_id"="id")) %>%
  #     left_join(s_list$Conc_Time_Values, by=c("id"="fk_series_id")) %>%
  #     select(id, species, conc_medium, analyte_name, analyte_name_secondary, analyte_casrn,
  #            conc_original=conc, conc_units_original=conc_units,
  #            conc_sd_original=conc_sd, conc_lower_bound_original=conc_lower_bound,
  #            conc_upper_bound_original=conc_upper_bound) %>%
  #     mutate(doc = f) %>%
  #     distinct()
  # }) %>%
  #   bind_rows()
  # tmp$species = normalize_species(x=tmp$species)
  # tmp = normalize_conc_medium(raw=tmp, f=f)
  if(!nrow(raw)){#Empty dataframe
    message("...normalize_conc dataframe empty...returning...")
    return(raw)
  }
  #List of dataframe subsets
  out = list()
  out$raw = normalization_prep(x=raw, newcols=c())
  #Fix missing conc - https://stackoverflow.com/questions/25768305/r-replace-multiple-values-in-multiple-columns-of-dataframes-with-na
  conc_cols = c("conc_original", "conc_sd_original", 
                "conc_lower_bound_original", "conc_upper_bound_original")
  out$raw[conc_cols] <- lapply(out$raw[conc_cols], 
                               function(x) replace(x, x %in% c("missing", "NA", "n/a"), NA))
  
  out$raw = out$raw %>%
    #Remove evaluation symbols
    dplyr::mutate(conc_original=gsub(">|<|at least", "", conc_original),
           #Remove conc_medium tags and other extraneous terms in units
           conc_units_original = gsub(paste0(c("tissue concentration", "[()]", unique(out$raw$conc_medium)), collapse="|"), "", 
                                      conc_units_original) %>%
             trimws(.))
  
  #Create normalization columns                                                                       
  out$raw = out$raw %>% dplyr::mutate(conc=conc_original,
                               conc_sd=conc_sd_original,
                               conc_lower_bound=conc_lower_bound_original,
                               conc_upper_bound=conc_upper_bound_original)
  #Missing concentration values
  out = check_missing(x=out, miss_col = "conc_original", f=f, flag=TRUE, log_path=log_path)
  
  #Get ND and NQ concentrations
  out$ND = out$raw %>% dplyr::filter(conc_original %in% c("ND", "NQ"))
  out$raw = out$raw %>% dplyr::filter(!tempID %in% unique(out$ND$tempID))
  #Missing units
  out = check_missing_units(x=out, f=f, units_col="conc_units_original", log_path=log_path)
  #Normalize units
  out$raw$conc_units_original = normalize_conc_units(out$raw$conc_units_original)
  #Percentage units flag
  out$percentage = out$raw %>% dplyr::filter(grepl("%|percent*|perc_dose", conc_units_original))
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$percentage$tempID)
  if(nrow(out$percentage)){
    log_CvT_doc_load(f=f, m="conc_conversion_needed_percentage", log_path=log_path, val=out$percentage$id)
  }
  #Radioactive units flag
  out$radioactive = out$raw %>% dplyr::filter(grepl("MBq|bq/", conc_units_original))
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$radioactive$tempID)
  if(nrow(out$radioactive)){
    log_CvT_doc_load(f=f, m="conc_conversion_needed_radioactive", log_path=log_path, val=out$radioactive$id)
  }
  #Rate units flag
  out$rate_units = out$raw %>% dplyr::filter(grepl("/hour|/day|/minute|/second|/hr|/min|/s|/h|*h/", conc_units_original))
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$rate_units$tempID)
  if(nrow(out$rate_units)){
    log_CvT_doc_load(f=f, m="conc_conversion_needed_rate", log_path=log_path, val=out$rate_units$id)
  }
  #Non-numerics
  out = check_non_numeric(x=out, f=f, col="conc_original", log_path=log_path)
  #Prep for conversion
  out$convert_ready = out$raw %>% dplyr::mutate(conc = as.numeric(conc_original))
  
  if (isTRUE(debug)) {
    return(out)
  }
  
  out$raw = NULL

  #Conc needs liquid portion (doesn't have / units)
  out$need_per_liquid = out$convert_ready %>% dplyr::filter(!grepl("/|per|ppm|ppb|ppmv|ppbv", conc_units_original),
                                                            # Filter out tissue measures to httk Density conversion attempts
                                                            !conc_units_original %in% c("ug"))
  out$convert_ready = out$convert_ready %>% dplyr::filter(!tempID %in% out$need_per_liquid$tempID)
  #Is per weight
  out$per_weight = out$convert_ready %>% 
    dplyr::filter(grepl("/kg|/g|/mg|/ng|/ug", conc_units_original)) %>%
    convert_mass_per_mass()
  out$convert_ready = out$convert_ready %>% dplyr::filter(!tempID %in% out$per_weight$tempID)
  #Try to normalize
  #Filter out ones that didn't get normalized
  #Add them back to the convert_ready list
  #Filer to convert_ready and convert to numeric
  out$convert_ready = out$convert_ready %>% dplyr::filter(!tempID %in% out$per_weight$tempID) %>%
    dplyr::mutate(dplyr::across(c(conc, conc_sd, conc_lower_bound, conc_upper_bound), as.numeric))
  #remaining logic TBD
  #message("...Conc_units conversion logic TBD")
  #warning("...Conc_units conversion logic TBD")
  
  # Map to chemical entries for DTXSID
  chem_map = db_query_cvt(paste0("SELECT id as fk_analyzed_chemical_id, dsstox_substance_id ",
                                 "FROM cvt.chemicals WHERE id in (", 
                                 toString(unique(out$convert_ready$fk_analyzed_chemical_id)), ") ",
                                 "AND dsstox_substance_id IS NOT NULL"))
  
  out$convert_ready = out$convert_ready %>%
    dplyr::left_join(chem_map, by="fk_analyzed_chemical_id")
  
  # Pull MW dictionary
  if(any(grepl("mol/", out$convert_ready$conc_units_original))){
    # Check environment variable for api_key
    if(!exists("API_AUTH")){
      stop("Need API key for CCTE Chemicals API...")
    }
    MW_dict = get_mw_chemicals_api(dtxsid_list=unique(out$convert_ready$dsstox_substance_id),
                                   api_key=API_AUTH)
  }
  
  # TODO Split up between routes as well (ug/mL tissue and ug/m3 breath)
  for(t in c("conc", "conc_sd", "conc_lower_bound", "conc_upper_bound")){
    message("...normalizing ", t)
    for(i in seq_len(nrow(out$convert_ready))){
      # Molecular Weight conversion (have to find MW first)
      # Units must be mol and dsstox_substance_id must be present
      MW=NA
      if(grepl("mol/", out$convert_ready[i,]$conc_units_original) && !is.na(out$convert_ready[i,]$dsstox_substance_id)){
        # Pull MW from dictionary by DTXSID
        MW <- MW_dict$mw[MW_dict$dtxsid == out$convert_ready[i,]$dsstox_substance_id]
      }
      # Tissue density lookup/conversion attempt
      if(out$convert_ready[i,]$conc_units_original %in% c("ug")){
        out$convert_ready[i,]$conc_units_original = paste0(out$convert_ready[i,]$conc_units_original, " tissue conc")
        MW <- httk::tissue.data[httk::tissue.data$variable == "Density (g/cm^3)",] %>%
          dplyr::filter(Tissue == out$convert_ready[i,]$conc_medium) %>%
          dplyr::mutate(Species = tolower(Species)) %>%
          dplyr::filter(Species == out$convert_ready[i,]$species) %>%
          dplyr::pull(value)
        # If not matching tissue density, set NA
        if(!length(MW)) MW = NA
      }
      out$convert_ready[i,] = convert_units(x=out$convert_ready[i,], 
                                            num=t, 
                                            units="conc_units_original", desired="ug/ml",
                                            overwrite_units = FALSE,
                                            conv_factor=MW)
    }
  }
  
  # Convert Failed
  out = check_convert_failed(x=out, f=f, col="conc", log_path=log_path)
  
  #Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  #Convert to NA for all lists that were not normalized
  out = lapply(names(out), function(n){
    if(n %in% c("ND", "convert_ready", "per_weight")){
      return(out[[n]])
    } else{
      convert_cols_to_NA(out[[n]], col_list=c("conc", "conc_sd", 
                                              "conc_lower_bound", "conc_upper_bound")) %>%
        return()
    }
  })
  
  # Convert all back to character to maintain non_numeric columns that weren't converted
  out = lapply(out, function(n){
    n %>%
      dplyr::mutate(dplyr::across(c(conc, conc_sd, conc_lower_bound, conc_upper_bound), 
                                  suppressWarnings(as.character)))  
  })
  
  # Recombine 
  out = out %>%
    dplyr::bind_rows()
  
  for(col in c("conc", "conc_sd", "conc_lower_bound", "conc_upper_bound")){
    if(any(out[[col]] < 0, na.rm=TRUE)){
      log_CvT_doc_load(f=f, m=paste0("negative_", col,"_values"), 
                       log_path=log_path, 
                       val = out$id[out[[col]] < 0] %>% 
                         # Remove NA matches
                         .[!is.na(.)])
    }
  }
  
  # Reorder
  out = out %>%
    dplyr::arrange(tempID)
  
  return(out)
}
