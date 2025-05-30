#' @description A helper function to normalize concentration units.
#' @param raw A dataframe of weight information to normalize
#' @param f The file name of the template being processed. Used for error logging.
#' @return Normalized version of the input `raw` parameter.
#' @title FUNCTION_TITLE
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  # EXAMPLE1
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
  if(!nrow(raw)){# Empty dataframe
    message("...normalize_conc dataframe empty...returning...")
    return(raw)
  }
  # List of dataframe subsets
  out = list()
  out$raw = normalization_prep(x=raw, newcols=c())
  # Fix missing conc - https://stackoverflow.com/questions/25768305/r-replace-multiple-values-in-multiple-columns-of-dataframes-with-na
  conc_cols = c("conc_original", "conc_sd_original", 
                "conc_lower_bound_original", "conc_upper_bound_original")
  out$raw[conc_cols] <- lapply(out$raw[conc_cols], 
                               function(x) replace(x, x %in% c("missing", "NA", "n/a"), NA))
  
  out$raw = out$raw %>%
    # Remove evaluation symbols
    dplyr::mutate(conc_original=gsub(">|<|at least", "", conc_original),
                  # Remove conc_medium tags and other extraneous terms in units
                  conc_units_original = gsub(paste0(c("tissue concentration", "[()]", unique(out$raw$conc_medium)), collapse="|"), "", 
                                             conc_units_original) %>%
                    trimws(.))
  
  # Create normalization columns                                                                       
  out$raw = out$raw %>% dplyr::mutate(conc=conc_original,
                                      conc_sd=conc_sd_original,
                                      conc_lower_bound=conc_lower_bound_original,
                                      conc_upper_bound=conc_upper_bound_original)
  # Get ND and NQ concentrations
  out$ND = out$raw %>% dplyr::filter(conc_original %in% c("ND", "NQ", "NE", "NS", "."))
  out$raw = out$raw %>% dplyr::filter(!tempID %in% unique(out$ND$tempID))
  
  # Missing concentration values
  out = check_missing(x=out, miss_col = "conc_original", f=f, flag=TRUE, log_path=log_path)
  
  
  # Missing units
  out = check_missing_units(x=out, f=f, units_col="conc_units_original", log_path=log_path)
  # Normalize units
  out$raw$conc_units_original = normalize_conc_units(out$raw$conc_units_original)
  # Percentage units flag
  out$percentage = out$raw %>% dplyr::filter(grepl("%|percent*|perc_dose", conc_units_original))
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$percentage$tempID)
  if(nrow(out$percentage)){
    log_CvT_doc_load(f=f, m="conc_conversion_needed_percentage", log_path=log_path, val=out$percentage$id)
  }
  # Invalid units list
  out$invalid_units = out$raw %>% dplyr::filter(grepl("ms peak area|MS Peak Area", conc_units_original))
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$invalid_units$tempID)
  if(nrow(out$invalid_units)){
    log_CvT_doc_load(f=f, m="conc_invalid_units", log_path=log_path, val=out$invalid_units$id)
  }
  # Radioactive units flag
  out$radioactive = out$raw %>% dplyr::filter(grepl("MBq|bq\\/|^dpm|eq/|equiv/|equivalent", conc_units_original))
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$radioactive$tempID)
  if(nrow(out$radioactive)){
    log_CvT_doc_load(f=f, m="conc_conversion_needed_radioactive", log_path=log_path, val=out$radioactive$id)
  }
  # Rate units flag
  out$rate_units = out$raw %>% dplyr::filter(grepl("/hour|/day|/minute|/second|/hr|/min|/s|/h|*h/|/24h", conc_units_original))
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$rate_units$tempID)
  if(nrow(out$rate_units)){
    log_CvT_doc_load(f=f, m="conc_conversion_needed_rate", log_path=log_path, val=out$rate_units$id)
  }
  
  # Conc needs volume (doesn't have / units)
  out$need_volume = out$raw %>% dplyr::filter(grepl("^nmol$", conc_units_original))
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$need_volume$tempID)
  
  # Misc. fixes
  out$raw = out$raw %>%
    dplyr::mutate(conc_original = conc_original %>%
                    # Fix double decimals
                    gsub("..", ".", ., fixed = TRUE) %>%
                    # Ends in S, should be 5
                    gsub("S$", "5", .) %>%
                    # Remove ending lowercase letters
                    gsub("[a-z]+$", "", .),
                  conc_units_original = conc_units_original %>%
                    # Change x 10^-3 M to x 10^-3 mol/L so it's not removed with need_per_liquid
                    gsub("x 10^-3 M", "x 10^-3 mol/L", ., fixed = TRUE) %>%
                    # Remove tissue qualifiers
                    gsub("tissue|wet tissue", "", .) %>%
                    # Replace unicode micro
                    gsub("\u00b5", "u", .) %>%
                    stringr::str_squish())
  
  # Non-numerics
  out = check_non_numeric(x=out, f=f, col="conc_original", log_path=log_path)
  # Prep for conversion
  out$convert_ready = out$raw %>% dplyr::mutate(conc = as.numeric(conc_original))
  
  if (isTRUE(debug)) {
    return(out)
  }
  
  out$raw = NULL
  
  # Conc needs liquid portion (doesn't have / units)
  out$need_per_liquid = out$convert_ready %>% dplyr::filter(!grepl("/|per|ppm|ppb|ppmv|ppbv", conc_units_original),
                                                            # Filter out tissue measures to httk Density conversion attempts
                                                            !conc_units_original %in% c("ug"))
  out$convert_ready = out$convert_ready %>% dplyr::filter(!tempID %in% out$need_per_liquid$tempID)
  # # Is per weight - Now handled with httk tissue density attempt
  # out$per_weight = out$convert_ready %>% 
  #   dplyr::filter(grepl("/kg|/g|/mg|/ng|/ug", conc_units_original)) %>%
  #   convert_mass_per_mass()
  # out$convert_ready = out$convert_ready %>% dplyr::filter(!tempID %in% out$per_weight$tempID)
  
  # Try to normalize
  # Filter out ones that didn't get normalized
  # Add them back to the convert_ready list
  # Filter to convert_ready and convert to numeric
  # out$convert_ready = out$convert_ready %>% dplyr::filter(!tempID %in% out$per_weight$tempID)
  # remaining logic TBD
  # message("...Conc_units conversion logic TBD")
  # warning("...Conc_units conversion logic TBD")
  
  # Set as numeric for processing
  out$convert_ready = out$convert_ready %>%
    dplyr::mutate(dplyr::across(c(conc, conc_sd, conc_lower_bound, conc_upper_bound), as.numeric))
  
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
  
  tissue_density_dict = httk::tissue.data %>%
    dplyr::filter(variable == "Density (g/cm^3)",
                  Tissue %in% unique(out$convert_ready$conc_medium)) %>%
    dplyr::mutate(Species = tolower(Species)) %>%
    dplyr::filter(Species %in% unique(out$convert_ready$species)) %>%
    dplyr::select(conc_medium = Tissue, species = Species, tissue_density = value)
  
  out$convert_ready = out$convert_ready %>%
    # Lowercase for conversion
    dplyr::mutate(conc_units_original = tolower(conc_units_original))
  
  # Prep conversion factor ahead of time
  out$convert_ready = out$convert_ready %>%
    dplyr::left_join(MW_dict,
                     by = c("dsstox_substance_id"="dtxsid")) %>%
    dplyr::left_join(tissue_density_dict,
                     by = c("species", "conc_medium")) %>%
    dplyr::mutate(
      # Add mass vs. volume designation to ppm and ppb
      conc_units_original = dplyr::case_when(
        # Assumes expired air is a volume
        conc_units_original %in% c("ppm", "ppb") & conc_medium %in% c("expired air") ~ paste0(conc_units_original, "v"),
        # Assumes not expired air is a mass
        conc_units_original %in% c("ppm", "ppb") & !conc_medium %in% c("expired air") ~ paste0(conc_units_original, "m"),
        TRUE ~ conc_units_original
      ),
      conversion_factor_type = dplyr::case_when(
        # Mass/Mass tissue density with molar units - multiply the molecular weight and density
        grepl("nmol/g|pmol/g|umol/kg", conc_units_original) ~ "mw_tissue",
        # Molar units, needs molecular weight
        grepl("mol/", conc_units_original) ~ "mw_only",
        # Mass, use tissue density conversion factor
        grepl("/kg|/g|/mg|/ng|/ug", conc_units_original) ~ "tissue",
        conc_units_original %in% c("ppbv","ppmv") & desired_units == "ug/m3" ~ "ppb_ppm_v",
        TRUE ~ NA
      ),
      # Calculate Liters per mole constant fpr ppmv and ppbv (only assume STP if no test environment temp provided)
      # TODO insert logic to calculate based on PV = nRT --> V/n = RT/P (R = 0.0821, P = 1atm, T = Kelvin)
      # TODO normalized study temperature field and convert to Kelvin
      # TODO set liters_per_mol_gas_const as case_when if temperature is available
      liters_per_mol_gas_const = 22.4,
      conv_factor = dplyr::case_when(
        # Mass/Mass tissue density with molar units - multiply the molecular weight and density
        conversion_factor_type == "mw_tissue" ~ mw * tissue_density,
        # Molar units, needs molecular weight
        conversion_factor_type == "mw_only" ~ mw,
        # Mass, use tissue density conversion factor
        conversion_factor_type == "tissue" ~ tissue_density,
        # Needs molecular weight to convert ppm/ppb to ug/m3
        conversion_factor_type == "ppb_ppm_v" ~ mw/liters_per_mol_gas_const,
        # Default 1 conversion factor, because it doesn't change anything
        TRUE ~ NA
      ),
      # Append "tissue conc" to differentiate tissue density conversions
      conc_units_original = dplyr::case_when(
        # Mass, use tissue density conversion factor
        grepl("tissue", conversion_factor_type) ~ paste0(conc_units_original, " tissue conc"),
        desired_units == "ug/m3" ~ paste0(conc_units_original, " air conc"),
        TRUE ~ conc_units_original
      ),
      # Handle desired_units for equivalent concentrations
      desired_units = dplyr::case_when(
        # Units contain "eq", "equiv", "equivalent"
        grepl("eq", conc_units_original) & desired_units == "ug/ml" ~ "ugEq/ml",
        grepl("eq", conc_units_original) & desired_units == "ug/m3" ~ "ugEq/m3",
        # Analytes flagged as radiolabeled
        radiolabeled == 1 & desired_units == "ug/m3" ~ "ugEq/m3",
        radiolabeled == 1 & desired_units == "ug/m3" ~ "ugEq/m3",
        TRUE ~ desired_units
      )
    )
  
  # Get generic full conv list to see if conversion factors are missing
  conv_list_full = convert_get_conversion_factor(conv_factor = 1)
  
  message("...Converting conc values...")
  # Get conversion equation
  out$convert_ready = out$convert_ready %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      conv_equ_raw = ifelse(is.null(conv_list_full[[conc_units_original]][[desired_units]]), 
                            paste0("No conversion for: `", conc_units_original, "` = list(`", desired_units, '`=""),'), 
                            convert_get_conversion_factor(conv_factor)[[conc_units_original]][[desired_units]]
      ),
      conv_equ = dplyr::case_when(
        grepl("No conversion for", conv_equ_raw, fixed = TRUE) ~ "*NA",
        TRUE ~ conv_equ_raw
      )
    ) %>%
    dplyr::ungroup()
  
  # Check missing conversions
  if(any(grepl("No conversion for", out$convert_ready$conv_equ_raw))){
    out$convert_ready %>% 
      dplyr::filter(grepl("No conversion for", conv_equ_raw)) %>%
      dplyr::pull(conv_equ_raw) %>%
      unique() %>%
      cat(sep = "\n")
    browser()
  }
  
  # Calculate conversion across conc columns using conv_equ
  out$convert_ready = out$convert_ready %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(c("conc", "conc_sd", "conc_lower_bound", "conc_upper_bound")),
                    ~ parse(text=paste0(., conv_equ)) %>% # parse the string
                      eval() %>% # evaluate the string equation
                      round(5))
    ) %>%
    dplyr::ungroup()
  
  # for(t in c("conc", "conc_sd", "conc_lower_bound", "conc_upper_bound")){
  #   message("...normalizing ", t)
  #   for(i in seq_len(nrow(out$convert_ready))){
  #     out$convert_ready[i,] = convert_units(x = out$convert_ready[i,],
  #                                           num = t,
  #                                           units = "conc_units_original",
  #                                           desired = out$convert_ready$desired_units[i],
  #                                           overwrite_units = FALSE,
  #                                           conv_factor = out$convert_ready$conv_factor[i])
  #   }
  # }
  
  # Convert Failed
  out = check_convert_failed(x=out, f=f, col="conc", log_path=log_path)
  
  # Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  # Convert to NA for all lists that were not normalized
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
