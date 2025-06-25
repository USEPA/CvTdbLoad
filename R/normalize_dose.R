
#' @description Doses are stored in mg/kg. In cases where the mass of the subject was not reported, and the dose was
#' administered as a simple mass and not a body weight proportion, a mg/kg dose was calculated using the average
#' mass for all subjects of that type.
#' @param raw A dataframe of weight information to normalize
#' @param f The file name of the template being processed. Used for error logging.
#' @param log_path File path where to save the log file.
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
#'  [filter][dplyr::filter], [mutate][dplyr::mutate], [bind_rows][dplyr::bind_rows], [arrange][dplyr::arrange], [select][dplyr::select]
#'  [get_physchem_param][httk::get_physchem_param]
#' @rdname normalize_dose
#' @export 
#' @importFrom dplyr filter mutate bind_rows arrange select
#' @importFrom httk get_physchem_param
normalize_dose <- function(raw, f, log_path, debug = FALSE){
  message("...normalizing dose...")
  #message("Normalize_dose is still in development...")
  # tmp = lapply(fileList, function(f){
  #   s_list = load_sheet_group(fileName = f, template_path = template_path)
  #   s_list$Series %>%
  #     left_join(s_list$Studies, by=c("fk_study_id"="id")) %>%
  #     left_join(s_list$Subjects, by=c("fk_subject_id"="id")) %>%
  #     select(species, subtype, weight, weight_units, height, height_units, #Needed for weight extrapolation
  #            test_substance_name, dose_level, dose_level_units, dose_volume, administration_route, dose_duration, dose_duration_units) %>%
  #     distinct() %>%
  #     mutate(file=f)
  # }) %>%
  #   bind_rows()
  # tmp = normalize_weight(raw=tmp, f=f)
  # tmp = normalize_height(raw=tmp, f=f)
  if(!nrow(raw)){#Empty dataframe
    message("...normalize_dose dataframe empty...returning...")
    return(raw)
  }
  # List of dataframe subsets
  out = list()
  out$raw = normalization_prep(x=raw, newcols=c("dose_level_normalized"))
  
  # Handle special cases
  out$raw = out$raw %>%
    dplyr::mutate(dose_level_units = dose_level_units %>%
                    # Remove the /day for mg/kg/day
                    gsub("mg/kg/day", "mg/kg", .))
  # Set to convert column to maintain original
  out$raw$dose_level_normalized = out$raw$dose_level
  species_list = db_query_cvt("SELECT distinct species FROM cvt.subjects") %>%
    dplyr::pull(species)
  # Remove species from units field
  out$raw$dose_level_units = gsub(paste0(c(paste0("/", species_list %>% unique()),
                                           species_list %>% unique()), collapse="|"), 
                                  "",
                                  out$raw$dose_level_units) %>%
    gsub(" per ", "/", .) %>% 
    stringr::str_squish()
  
  # Remove parenthetical information from dose_level_normalized
  if ("dose_level_normalized" %in% names(out$raw)) {
    out$raw$dose_level_normalized = gsub("\\([^()]*\\)", "", out$raw$dose_level_normalized) %>% stringr::str_squish()
  }
  # Missing dose value
  out = check_missing(x=out, miss_col = "dose_level", f=f, flag=TRUE, log_path=log_path)
  out$missing = out$missing %>%
    dplyr::mutate(conversion_factor_type = "missing_dose")
  
  # Missing units
  out = check_missing_units(x=out, f=f, units_col="dose_level_units", log_path=log_path)
  out$missing_units = out$missing_units %>%
    dplyr::mutate(conversion_factor_type = "missing_dose_units")
  
  # Percentage units flag
  out$percentage = out$raw %>% dplyr::filter(grepl("%|percent*", dose_level_units)) %>%
    dplyr::mutate(conversion_factor_type = "percent")
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$percentage$tempID)
  if(nrow(out$percentage)){
    log_CvT_doc_load(f=f, m="dose_conversion_needed_percentage", log_path=log_path, val=out$percentage$id)
  }
  # # Concentration units flag
  # out$concentration = out$raw %>% dplyr::filter(grepl("/l|/ml|/L|/mL|/0.1mL", dose_level_units))
  # out$raw = out$raw %>% dplyr::filter(!tempID %in% out$concentration$tempID)
  # if(nrow(out$concentration)){
  #   log_CvT_doc_load(f=f, m="dose_conversion_needed_concentration", log_path=log_path, val=out$concentration$id)
  # }
  # Radioactive units flag
  out$radioactive = out$raw %>% dplyr::filter(grepl("MBq|uCi", dose_level_units)) %>%
    dplyr::mutate(conversion_factor_type = "radioactive")
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$radioactive$tempID)
  if(nrow(out$radioactive)){
    log_CvT_doc_load(f=f, m="dose_conversion_needed_radioactive", log_path=log_path, val=out$radioactive$id)
  }
  # Rate units flag
  out$rate_units = out$raw %>% dplyr::filter(grepl("/hour|/day|/minute|/second|/hr|/min|/s", dose_level_units)) %>%
    dplyr::mutate(conversion_factor_type = "rate_units")
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$rate_units$tempID)
  if(nrow(out$rate_units)){
    log_CvT_doc_load(f=f, m="dose_conversion_needed_rate", log_path=log_path, val=out$rate_units$id)
  }
  # Gas/Liquid units flag
  out$gas_liquid = out$raw %>% dplyr::filter(grepl("gas|liquid", dose_level_units)) %>%
    dplyr::mutate(conversion_factor_type = "gas_liquid")
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$gas_liquid$tempID)
  if(nrow(out$gas_liquid)){
    log_CvT_doc_load(f=f, m="dose_conversion_gas_liquid", log_path=log_path, val=out$gas_liquid$id)
  }
  
  # # ppm/ppb conversion needed
  # out$parts_per = out$raw %>% dplyr::filter(grepl("ppm|ppb", dose_level_units))
  # out$raw = out$raw %>% dplyr::filter(!tempID %in% out$parts_per$tempID)
  # if(nrow(out$parts_per)){
  #   log_CvT_doc_load(f=f, m="dose_conversion_needed_ppm_ppb", log_path=log_path, val=out$parts_per$id)
  # }
  # List of doses
  out = check_subject_list(x=out, f=f, col="dose_level_normalized", log_path=log_path)
  
  out$split_subject = out$split_subject %>%
    dplyr::mutate(conversion_factor_type = "subject_list")
  # +/- Group
  out = check_unit_ci(x=out, f=f, col="dose_level_normalized", log_path=log_path)
  out$ci = out$ci %>%
    dplyr::mutate(conversion_factor_type = "ci")
  # Dose range
  out = check_unit_range(x=out, f=f, col="dose_level_normalized", log_path=log_path)
  out$unit_range = out$unit_range %>%  
    dplyr::mutate(conversion_factor_type = "unit_range")
  # Contains "body weight"
  out$bw = out$raw %>%
    dplyr::filter(grepl("bw|bodyweight|body weight", dose_level_units)) %>%
    dplyr::mutate(conversion_factor_type = "bw",
                  dose_level_units = dose_level_units %>%
                    # Clean-up body weight representation
                    gsub(" body weight", "-bw", .) %>%
                    gsub(" bw", "-bw", .),
                  dose_level_normalized = as.numeric(dose_level_normalized))
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$bw$tempID)
  # Ready for conversion
  out$conversion = out$raw %>% 
    dplyr::mutate(dose_level_normalized = suppressWarnings(as.numeric(dose_level_normalized))) %>%
    dplyr::filter(!is.na(dose_level_normalized), !is.nan(dose_level_normalized))
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$conversion$tempID)
  # Check unhandled cases
  if(nrow(out$raw)){
    message("...Unhandled cases for dose: ", paste0(out$raw$dose_level_normalized %>% unique(), collapse = "; "))
    log_CvT_doc_load(f=f, m="unhandled_dose_normalize_case", log_path=log_path, val=out$raw$id)
  }
  # Dose needs weight (doesn't have / units)
  out$need_per_weight = out$conversion %>% dplyr::filter(!grepl("/|per|ppm|ppb", dose_level_units) | 
                                                           grepl("^nmol$|^nmole$", dose_level_units)) %>%
    dplyr::mutate(conversion_factor_type = "need_per_weight")
  out$conversion = out$conversion %>% dplyr::filter(!tempID %in% out$need_per_weight$tempID)
  # Get conversion factor body weight
  out$need_per_weight = out$need_per_weight %>%
    # Split up collapsed species information
    tidyr::separate_longer_delim(subject_info, delim = ", ") %>%
    tidyr::separate_wider_delim(cols = subject_info,
                                names = c("species", "bw"),
                                delim = ": ") %>%
    dplyr::mutate(
      dplyr::across(c(bw), as.numeric),
      equ_const = bw
    ) %>%
    dplyr::group_by(id) %>%
    dplyr::reframe(
      # Retain non-group columns
      dplyr::across(),
      need_bw_min = suppressWarnings(min(equ_const, na.rm = TRUE)), 
      need_bw_max = suppressWarnings(max(equ_const, na.rm = TRUE))
    ) %>%
    # Replace min/max Inf with NA
    dplyr::mutate(
      dplyr::across(dplyr::contains("need_bw"), 
                    ~ replace(., is.infinite(.), NA)),
      conversion_factor_type = "need_bw"
    ) %>%
    dplyr::select(-c(species, bw, equ_const)) %>%
    dplyr::distinct()
  
  # Feed/Drinking water study
  out$feed_drink = out$conversion %>%
    dplyr::filter(
      dplyr::if_any(c(administration_route_original, administration_route_normalized,
                      administration_method_original, administration_method_normalized),
                    ~ grepl("drink|water|\\bfeed\\b", .),
                    # Ignore cases where already reported in /kg-bw
                    !grepl("\\/kg-bw", dose_level_units))
    ) %>%
    dplyr::mutate(conversion_factor_type = dplyr::case_when(
      grepl("\\bfeed\\b", administration_method_original) | grepl("\\bfeed\\b", administration_method_normalized) ~ "feed",
      TRUE ~ "drinking"
    ))
  out$conversion = out$conversion %>%
    dplyr::filter(!tempID %in% out$feed_drink$tempID)
  
  # Table 1-3 U.S. EPA. Recommendations For And Documentation Of Biological Values For Use In Risk Assessment. U.S. Environmental Protection Agency, Washington, DC, EPA/600/6-87/008 (NTIS PB88179874), 1988.
  # feed = kg/day; drinking = L/day
  feed_drink_rates = data.frame(
    species = c("rat", "mouse", "hamster", "rabbit"),
    type = c("feed", "feed", "feed", "feed",
             "drinking", "drinking", "drinking", "drinking"),
    f_d_equ = c("0.056*bw^0.6611", "0.056*bw^0.6611", "0.082*bw^0.9285", "0.041*bw^0.7898",
                "0.10*bw^0.7377", "0.10*bw^0.7377", NA, NA)
  )
  # Calculate feed/drinking water consumption per body weight, store min and max by study ID groups
  out$feed_drink = out$feed_drink %>%
    # Split up collapsed species information
    tidyr::separate_longer_delim(subject_info, delim = ", ") %>%
    tidyr::separate_wider_delim(cols = subject_info,
                                names = c("species", "bw"),
                                delim = ": ") %>%
    # Join to get rate equations
    dplyr::left_join(feed_drink_rates,
                     by = c("species", "conversion_factor_type"="type")) %>%
    tidyr::separate_wider_delim(f_d_equ,
                                names = c("equ_const", "equ_exp"),
                                delim = "*bw^") %>%
    dplyr::mutate(
      dplyr::across(c(equ_const, bw, equ_exp, administration_term), as.numeric),
      equ_term = dplyr::case_when(
        administration_term_units %in% c("d", "day", "days") ~ administration_term,
        administration_term_units %in% c("wk", "week", "weeks") ~ administration_term * 7,
        administration_term_units %in% c("hr", "hrs", "hour", "hours") ~ administration_term / 24,
        TRUE ~ NA
      ),
      consumption_bw = (equ_const * (bw ^ equ_exp) * equ_term) / bw
    ) %>%
    dplyr::group_by(id) %>%
    dplyr::reframe(
      # Retain non-group columns
      dplyr::across(),
      consumption_bw_min = suppressWarnings(min(consumption_bw, na.rm = TRUE)), 
      consumption_bw_max = suppressWarnings(max(consumption_bw, na.rm = TRUE))
    ) %>%
    # Replace min/max Inf with NA
    dplyr::mutate(
      dplyr::across(dplyr::contains("consumption"), 
                    ~ replace(., is.infinite(.), NA)),
      conversion_factor_type = "feed_drink"
    ) %>%
    dplyr::select(-c(species, bw, equ_const, equ_exp, equ_term, consumption_bw)) %>%
    dplyr::distinct()
  
  # Oral /mL or /L units - use dose volume and subject weight to get mg/kg-bw
  out$oral_vol = out$conversion %>%
    dplyr::filter(administration_route_normalized %in% c("oral", "iv"),
                  grepl("\\/ml|\\/l", dose_level_units, ignore.case = TRUE))
  out$conversion = out$conversion %>% dplyr::filter(!tempID %in% out$oral_vol$tempID)
  
  out$oral_vol = out$oral_vol %>%
    # Split up collapsed species information
    tidyr::separate_longer_delim(subject_info, delim = ", ") %>%
    tidyr::separate_wider_delim(cols = subject_info,
                                names = c("species", "bw"),
                                delim = ": ") %>%
    dplyr::mutate(
      dplyr::across(c(bw, dose_volume), as.numeric),
      equ_const = dplyr::case_when(
        # Already provided in ml/kg, so conversion to /kg-bw is multiply bw
        dose_volume_units == "ml/kg" ~ dose_volume * bw,
        TRUE ~ dose_volume / bw
      )
    ) %>%
    dplyr::group_by(id) %>%
    dplyr::reframe(
      # Retain non-group columns
      dplyr::across(),
      oral_vol_min = suppressWarnings(min(equ_const, na.rm = TRUE)), 
      oral_vol_max = suppressWarnings(max(equ_const, na.rm = TRUE))
    ) %>%
    # Replace min/max Inf with NA
    dplyr::mutate(
      dplyr::across(dplyr::contains("oral_vol"), 
                    ~ replace(., is.infinite(.), NA)),
      conversion_factor_type = "oral_vol",
      # Convert back to character to recombine with out list later...
      dose_volume = as.character(dose_volume)
    ) %>%
    dplyr::select(-c(species, bw, equ_const)) %>%
    dplyr::distinct()
  
  out$dermal = out$conversion %>%
    dplyr::filter(
      grepl("\\/m\\^3|\\/l|\\/ml", dose_level_units, ignore.case = TRUE), 
      administration_route_normalized == "dermal"
    )
  out$conversion = out$conversion %>% dplyr::filter(!tempID %in% out$out$dermal$tempID)
  
  out$dermal = out$dermal %>%
    # Split up collapsed species information
    tidyr::separate_longer_delim(subject_info, delim = ", ") %>%
    tidyr::separate_wider_delim(cols = subject_info,
                                names = c("species", "bw"),
                                delim = ": ") %>%
    dplyr::mutate(
      dplyr::across(c(bw, dose_volume), as.numeric),
      equ_const = dplyr::case_when(
        # Already provided in ml/kg, so conversion to /kg-bw is multiply bw
        dose_volume_units == "L" & grepl("\\/ml", dose_level_units, ignore.case = TRUE) ~ dose_volume * 1000 / bw,
        TRUE ~ dose_volume / bw
      )
    ) %>%
    dplyr::group_by(id) %>%
    dplyr::reframe(
      # Retain non-group columns
      dplyr::across(),
      dermal_vol_min = suppressWarnings(min(equ_const, na.rm = TRUE)), 
      dermal_vol_max = suppressWarnings(max(equ_const, na.rm = TRUE))
    ) %>%
    # Replace min/max Inf with NA
    dplyr::mutate(
      dplyr::across(dplyr::contains("dermal_vol"), 
                    ~ replace(., is.infinite(.), NA)),
      conversion_factor_type = "dermal_vol",
      # Convert back to character to recombine with out list later...
      dose_volume = as.character(dose_volume)
    ) %>%
    dplyr::select(-c(species, bw, equ_const)) %>%
    dplyr::distinct()
  
  if (isTRUE(debug)) {
    return(out)
  }
  
  # Fix need_per_weight - convert to mg then divide by kg weight (given or extrapolated)
  # Cannot use subject weight since it's not always possible to connect to 1 subject
  # if(nrow(out$need_per_weight)){
  #   for(i in seq_len(nrow(out$need_per_weight))){
  #     out$need_per_weight[i,] = convert_units(x=out$need_per_weight[i,], 
  #                                           num="dose_level_normalized", 
  #                                           units="dose_level_units", desired="mg",
  #                                           overwrite_units = FALSE)
  #   }
  #   #Divide by weight (given or extrapolated)
  #   out$need_per_weight$dose_level_normalized = out$need_per_weight$dose_level_normalized / out$need_per_weight$weight_kg
  # }
  
  # Convert dosages
  out$convert_ready = dplyr::bind_rows(out$conversion, out$ci, out$unit_range, out$need_per_weight, 
                                       out$feed_drink, out$bw, out$oral_vol, out$dermal, out$need_per_weight)
  out$conversion = NULL; out$ci = NULL; out$unit_range = NULL; out$need_per_weight = NULL; out$feed_drink = NULL; out$bw=NULL
  out$oral_vol = NULL; out$dermal = NULL; out$need_per_weight = NULL
  
  if(nrow(out$convert_ready)){
    # Map to chemical entries for DTXSID
    chem_map = db_query_cvt(paste0("SELECT id as fk_dosed_chemical_id, dsstox_substance_id ",
                                   "FROM cvt.chemicals WHERE id in (", 
                                   toString(unique(out$convert_ready$fk_dosed_chemical_id)), ") ",
                                   "AND dsstox_substance_id IS NOT NULL"))
    
    out$convert_ready = out$convert_ready %>%
      dplyr::left_join(chem_map, by="fk_dosed_chemical_id")
    
    # Pull MW dictionary
    if(any(grepl("mol/", out$convert_ready$dose_level_units))){
      # Check environment variable for api_key
      if(!exists("API_AUTH")){
        stop("Need API key for CCTE Chemicals API...")
      }
      MW_dict = get_mw_chemicals_api(dtxsid_list=unique(out$convert_ready$dsstox_substance_id),
                                     api_key=API_AUTH)
    }
    
    out$convert_ready = out$convert_ready %>%
      dplyr::mutate(
        dose_level_units = dose_level_units %>%
          # Remove all whitespace
          gsub("[[:space:]]", "", .) %>%
          # Replace unicode micro
          gsub("\u00b5", "u", .) %>%
          # Lowercase for conversion
          tolower()
      )
    ################################################################################
    # View unique combinations of units by route
    # out$convert_ready %>%
    #   dplyr::select(
    #     dose_level_units,
    #     # dose_level, dose_level_normalized,
    #     administration_route_original, administration_method_original, administration_form_original,
    #     administration_route_normalized, administration_method_normalized, administration_form_normalized
    #   ) %>%
    #   dplyr::distinct() %>%
    #   View(title = "dose_admin")
    
    # Prep conversion factor ahead of time
    out$convert_ready = out$convert_ready %>%
      dplyr::left_join(MW_dict,
                       by = c("dsstox_substance_id"="dtxsid")) %>%
      dplyr::mutate(
        # Append "tissue conc" to differentiate tissue density conversions
        dose_level_units_tagged = dplyr::case_when(
          conversion_factor_type == "need_bw" ~ paste0(dose_level_units, " need_bw"),
          # Dermal with dose volume conversion_factor for mg/kg-bw
          conversion_factor_type == "dermal_vol" ~ paste0(dose_level_units, " dermal_vol"),
          # Funnel oral_vol group to use correct conversion with conversion factor
          conversion_factor_type == "oral_vol" ~ paste0(dose_level_units, " oral_vol"),
          # Assumes ppm/ppb volume from chamber exposure
          dose_level_units %in% c("ppm", "ppb") & 
            administration_route_normalized %in% c("inhalation", "endotracheal") ~ paste0(dose_level_units, "v"),
          # Assumes ppm/ppb volume from chamber exposure
          dose_level_units %in% c("ppm", "ppb") & 
            administration_form_original %in% c("vapor") ~ paste0(dose_level_units, "v"),
          # Assumes ppm/ppb mass from feed
          dose_level_units %in% c("ppm", "ppb") & grepl("\\bfeed\\b", administration_method_normalized) ~ paste0(dose_level_units, "m"),
          # Feed study
          grepl("\\bfeed\\b", administration_route_original) & !conversion_factor_type %in% c("bw") ~ paste0(dose_level_units, " feed"),
          grepl("\\bfeed\\b", administration_method_original) & !conversion_factor_type %in% c("bw") ~ paste0(dose_level_units, " feed"),
          grepl("\\bfeed\\b", administration_method_normalized) & !conversion_factor_type %in% c("bw") ~ paste0(dose_level_units, " feed"),
          # Drinking water study
          grepl("drink|water", administration_route_original) & !conversion_factor_type %in% c("bw", "unit_range") ~ paste0(dose_level_units, " drinking"),
          grepl("drink|water", administration_method_original) & !conversion_factor_type %in% c("bw", "unit_range") ~ paste0(dose_level_units, " drinking"),
          grepl("drink|water", administration_method_normalized) & !conversion_factor_type %in% c("bw", "unit_range") ~ paste0(dose_level_units, " drinking"),
          TRUE ~ dose_level_units
        ) %>%
          # Append "dose" to all units to help differentiate during unit conversion
          paste0(., " dose"),
        conversion_factor_type = dplyr::case_when(
          # Case of need body weight denominator but numerator in moles
          conversion_factor_type == "need_bw" & grepl("mol", dose_level_units_tagged) ~ "mw_need_bw",
          # Case of denominator in bw but numerator in moles
          conversion_factor_type == "bw" & grepl("mol\\/", dose_level_units_tagged) ~ "mw_bw",
          # Set previously during screening/grouping
          !is.na(conversion_factor_type) ~ conversion_factor_type,
          dose_level_units_tagged %in% c("ppbv","ppmv") & administration_route_normalized %in% c("inhalation", "endotracheal") ~ "ppb_ppm_v_inh",
          dose_level_units_tagged %in% c("ppbv","ppmv") & administration_route_normalized %in% c("oral") ~ "ppb_ppm_v_oral",
          # /m^3, /l, /mL
          grepl("\\/m\\^3|\\/l|\\/ml", dose_level_units_tagged) & administration_route_normalized == "inhalation" ~ "inhalation_exp_conc",
          # molar /m^3
          grepl("mol\\/m\\^3", dose_level_units_tagged) & administration_route_normalized == "inhalation" ~ "inhalation_exp_conc_mol",
          # TODO determine how to handle this case
          grepl("\\/cm^2", dose_level_units_tagged) & administration_route_normalized == "dermal" ~ "dermal_sa",
          # Molar units, needs molecular weight
          grepl("mol/", dose_level_units_tagged) ~ "mw_only",
          TRUE ~ NA
        ),
        # Min is the default, or the minimum in a range
        conv_factor_min = dplyr::case_when(
          conversion_factor_type == "mw_bw" ~ mw,
          # Molar units, needs molecular weight
          conversion_factor_type == "mw_only" ~ mw,
          conversion_factor_type == "inhalation_exp_conc_mol" ~ mw,
          # TODO handle ppmv/ppbv inhalation conversion factor
          # TODO handle ppmv/ppbv oral conversion factor
          # feed consumption / bw min
          conversion_factor_type == "feed dose" ~ consumption_bw_min,
          # drinking consumption / bw min
          conversion_factor_type == "drinking dose" ~ consumption_bw_min,
          # oral studies reporting dosages per volume
          conversion_factor_type == "oral_vol" ~ oral_vol_min,
          # Dermal dose volume conversion
          conversion_factor_type == "dermal_vol" ~ dermal_vol_min,
          # Reported just as weight, conversion_factor is body_weight
          conversion_factor_type == "need_bw" ~ need_bw_min,
          # Reported just as molar weight, conversion_factor is mw/body_weight
          conversion_factor_type == "mw_need_bw" ~ mw/need_bw_min,
          TRUE ~ NA
        ),
        # Max is only used for cases where dose may be a range
        conv_factor_max = dplyr::case_when(
          # feed consumption / bw max
          conversion_factor_type == "feed dose" ~ consumption_bw_max,
          # drinking consumption / bw max
          conversion_factor_type == "drinking dose" ~ consumption_bw_max,
          # oral studies reporting dosages per volume
          conversion_factor_type == "oral_vol" ~ oral_vol_max,
          # Dermal dose volume conversion
          conversion_factor_type == "dermal_vol" ~ dermal_vol_max,
          # Reported just as weight, conversion_factor is body_weight
          conversion_factor_type == "need_bw" ~ need_bw_max,
          # Reported just as molar weight, conversion_factor is mw/body_weight
          conversion_factor_type == "mw_need_bw" ~ mw/need_bw_max,
          TRUE ~ NA
        ),
        desired_units = dplyr::case_when(
          conversion_factor_type %in% c("inhalation_exp_conc", 
                                        "inhalation_exp_conc_mol") ~ "mg/m3",
          conversion_factor_type %in% c("feed_drink") ~ "mg/kg BW-day",
          # TODO Add dermal normalization when ready
          # grepl("dermal", conversion_factor_type) ~ "mg/m2",
          TRUE ~ "mg/kg BW"
        )
      )
    
    # TODO For feed/drinking water studies, set dose_level_target as dose_level and replace dose_level with new calculation, if dose_level_target is NULL
    # TODO collapse into range where needed/possible for cases with multiple subjects
    
    # Get generic full conv list to see if conversion factors are missing
    conv_list_full = convert_get_conversion_factor(conv_factor = 1)
    
    message("...Converting dose values...")
    # Get conversion equation
    out$convert_ready = out$convert_ready %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        # Minimum calculation (default or range minimum)
        conv_min_equ_raw = ifelse(is.null(conv_list_full[[dose_level_units_tagged]][[desired_units]]), 
                                  paste0("No conversion for: `", dose_level_units_tagged, "` = list(`", desired_units, '`=""),'), 
                                  convert_get_conversion_factor(conv_factor_min)[[dose_level_units_tagged]][[desired_units]]
        ),
        conv_min_equ = dplyr::case_when(
          grepl("No conversion for", conv_min_equ_raw, fixed = TRUE) ~ "*NA",
          TRUE ~ conv_min_equ_raw
        ),
        # Maximum calculation (only used if a maximum value exists)
        conv_max_equ_raw = ifelse(is.null(conv_list_full[[dose_level_units_tagged]][[desired_units]]), 
                                  paste0("No conversion for: `", dose_level_units_tagged, "` = list(`", desired_units, '`=""),'), 
                                  convert_get_conversion_factor(conv_factor_max)[[dose_level_units_tagged]][[desired_units]]
        ),
        conv_max_equ = dplyr::case_when(
          grepl("No conversion for", conv_max_equ_raw, fixed = TRUE) ~ "*NA",
          TRUE ~ conv_max_equ_raw
        )
      ) %>%
      dplyr::ungroup()
    
    # Check missing conversions
    if(any(grepl("No conversion for", out$convert_ready$conv_min_equ_raw))){
      out$convert_ready %>% 
        dplyr::filter(grepl("No conversion for", conv_min_equ_raw)) %>%
        dplyr::pull(conv_min_equ_raw) %>%
        unique() %>%
        cat(sep = "\n")
      browser()
    }
    
    # Calculate conversion across conc columns using conv_equ
    out$convert_ready = out$convert_ready %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        dose_level_normalized_min = parse(text=paste0(dose_level_normalized, 
                                                      conv_min_equ)) %>% # parse the string
          eval() %>% # evaluate the string equation
          round(5),
        dose_level_normalized_max = parse(text=paste0(dose_level_normalized, 
                                                      conv_max_equ)) %>% # parse the string
          eval() %>% # evaluate the string equation
          round(5)
      ) %>%
      dplyr::ungroup()
    
    ################################################################################
    ## Old logic before tidyr approach
    # for(i in seq_len(nrow(out$convert_ready))){
    #   #Molecular Weight conversion (have to find MW first)
    #   MW=NA
    #   if(grepl("mol/", out$convert_ready[i,]$dose_level_units)){
    #     # Pull MW from dictionary by DTXSID
    #     MW <- MW_dict$mw[MW_dict$dtxsid == out$convert_ready[i,]$dsstox_substance_id]
    #   }
    #   #NEED subject weight to convert!
    #   out$convert_ready[i,] = convert_units(x=out$convert_ready[i,], 
    #                                         num="dose_level_normalized",
    #                                         units="dose_level_units", 
    #                                         desired="mg/kg",
    #                                         overwrite_units = FALSE,
    #                                         conv_factor=MW)
    # }
  }
  
  # Convert Failed
  out = check_convert_failed(x=out, f=f, col="dose_level_normalized", log_path=log_path, id_col = "fk_study_id")
  # Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  # Convert to NA for all lists that were not normalized
  out = lapply(names(out), function(n){
    if(n %in% c("unit_range", "need_per_weight", "convert_ready")){
      return(out[[n]])
    } else{
      convert_cols_to_NA(out[[n]], col_list=c("dose_level_normalized")) %>%
        return()
    }
  })
  
  out = out %>%
    dplyr::bind_rows() %>% 
    dplyr::arrange(tempID) %>%
    # TODO do not naively combine, min/max would be swapped due to conversion factor sizes
    # tidyr::unite(col = "dose_level_normalized_final",
    #              dose_level_normalized_min, dose_level_normalized_max,
    #              sep = "-",
    #              na.rm = TRUE,
    #              remove = FALSE) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dose_normalized_range = dplyr::case_when(
        is.na(dose_level_normalized_min) & is.na(dose_level_normalized_max) ~ NA,
        !is.na(dose_level_normalized_min) & is.na(dose_level_normalized_max) ~ as.character(dose_level_normalized_min),
        is.na(dose_level_normalized_min) & !is.na(dose_level_normalized_max) ~ "UNHANDLED CASE",
        dose_level_normalized_min == dose_level_normalized_max ~ as.character(dose_level_normalized_min),
        # Create range from min and max of the two columns
        TRUE ~ suppressWarnings(
          paste0(
            min(c(dose_level_normalized_min, dose_level_normalized_max), na.rm = TRUE),
            "-",
            max(c(dose_level_normalized_min, dose_level_normalized_max), na.rm = TRUE)
          )
        ) 
      ))
  
  # TODO Draft logic to combine dosages in ranges as needed
  # Logic:
  # For dermal, inhalation, and cases with ranges, if dose_target is NA, set it to be the same as dose_level
  # If dose_level is range, set dose_normalized as the average
  out = out %>%
    dplyr::mutate(
      dose_level_target_final = dplyr::case_when(
        all(
          is.na(dose_level_target),
          !is.na(dose_level_normalized_min),
          !is.na(dose_level_normalized_max),
          dose_level_normalized_min != dose_level_normalized_max
        ) ~ dose_level,
        TRUE ~ dose_level_target
      ),
      dose_level_target_units_final = dplyr::case_when(
        all(
          is.na(dose_level_target),
          !is.na(dose_level_normalized_min),
          !is.na(dose_level_normalized_max),
          dose_level_normalized_min != dose_level_normalized_max
        ) ~ dose_level_units,
        TRUE ~ dose_level_target_units
      ),
      # Replace originally reported dose as a range or as-is
      dose_level_final = dplyr::case_when(
        all(!is.na(dose_level_normalized_max), 
            dose_level_normalized_min != dose_level_normalized_max)  ~ paste0(min(c(dose_level_normalized_min, dose_level_normalized_max)), 
                                                                              "-", 
                                                                              max(c(dose_level_normalized_min, dose_level_normalized_max))),
        TRUE ~ dose_level
      ),
      # Replace originally reported dose as a range or as-is
      dose_level_units_final = dplyr::case_when(
        all(
          # No NA normalized Value
          !is.na(dose_level_normalized_max), 
          # min and max not equal  
          dose_level_normalized_min != dose_level_normalized_max)  ~ desired_units,
        TRUE ~ dose_level_units
      ),
      dose_level_normalized_final = dplyr::case_when(
        # If both NA, return NA
        is.na(dose_level_normalized_min) & is.na(dose_level_normalized_max) ~ NA,
        # If range present, report mean
        !is.na(dose_level_normalized_min) & !is.na(dose_level_normalized_max) ~ mean(c(dose_level_normalized_min, dose_level_normalized_max)),
        # Report min (default for cases without ranges)
        TRUE ~ dose_level_normalized_min
      ),
      dose_level_units_normalized_final = dplyr::case_when(
        # NA normalized value, NA units
        is.na(dose_level_normalized_final) ~ NA,
        # No conversion (probably a case covered by NA), NA units
        grepl("No conversion", conv_max_equ_raw) ~ NA,
        TRUE ~ desired_units
      ) 
    ) # %>%
  # Debug logic to compare old to new to see what was reassigned
  # dplyr::select(
  #   id, conversion_factor_type, 
  #   dose_level_normalized_min,
  #   dose_level_normalized_max,
  #   dose_level_target, dose_level_target_units,
  #   dose_level, dose_level_units,
  #   dose_level_normalized, dose_level_units_normalized = desired_units,
  #   dose_level_target_final, dose_level_target_units_final,
  #   dose_level_final, dose_level_units_final,
  #   dose_level_normalized_final
  # ) %>%
  # dplyr::distinct() %>%
  # tidyr::pivot_longer(
  #   cols = -dplyr::all_of(c("id", "conversion_factor_type", "dose_level_normalized_min", "dose_level_normalized_max")),
  #   names_to = "col_name",
  #   values_transform = as.character
  # ) %>%
  # dplyr::mutate(comp_group = dplyr::case_when(
  #   grepl("_final$", col_name) ~ "new",
  #   TRUE ~ "old"
  # )) %>%
  # dplyr::mutate(col_name = col_name %>%
  #                 gsub("_final", "", .)) %>%
  # tidyr::pivot_wider(
  #   # id_cols = -dplyr::all_of(c("comp_group", "value")),
  #   id_cols = c("id", "conversion_factor_type", "dose_level_normalized_min", "dose_level_normalized_max", "col_name"),
  #   names_from = comp_group,
  #   values_from = value
  # ) %>%
  # dplyr::mutate(
  #   compare = dplyr::case_when(
  #     is.na(old) & is.na(new) ~ TRUE,
  #     is.na(old) & !is.na(new) ~ FALSE,
  #     !is.na(old) & is.na(new) ~ FALSE,
  #     TRUE ~ old == new
  #   )
  # )
  
  
  # # Convert dose_level_normalized to numeric for unconverted lists
  # out = lapply(out, function(x){ 
  #   x = x %>% dplyr::mutate(dose_level_normalized = suppressWarnings(as.numeric(dose_level_normalized)))
  # })
  return(out %>% dplyr::select(-tempID))
}
