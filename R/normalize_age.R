#' @title normalize_age
#' @description FUNCTION_DESCRIPTION
#' @param raw PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION
#' @param log_path File path where to save the log file.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [read_xlsx][readxl::read_xlsx]
#'  [mutate][dplyr::mutate], [filter][dplyr::filter], [select][dplyr::select], [bind_rows][dplyr::bind_rows], [rename][dplyr::rename], [arrange][dplyr::arrange]
#' @rdname normalize_age
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate filter select bind_rows rename arrange
normalize_age <- function(raw, f, log_path, debug = FALSE){
  message("...normalizing age...")
  age_dict = readxl::read_xlsx("input/dictionaries/age_category_dict.xlsx")
  # tmp = lapply(fileList, function(f){
  #   s_list = load_sheet_group(fileName = f, template_path = template_path)
  #   s_list$Subjects %>% select(species, age, age_category, age_units) %>% distinct() %>%
  #     mutate(doc = f)
  # }) %>%
  #   bind_rows()
  # tmp$species = normalize_species(tmp$species)
  if(!nrow(raw)){#Empty dataframe
    message("...normalize_age dataframe empty...returning...")
    return(raw)
  }
  
  # Prep list for dataframe subsets
  out = list()
  out$raw = normalization_prep(x=raw, newcols=c())
  out$raw$species = tolower(trimws(out$raw$species))
  out$raw$age_original = out$raw$age
  out$raw$age_units_original = out$raw$age_units
  out$raw = out$raw %>% dplyr::mutate(age_normalized = age)
  # Extract units
  # out$raw = extract_weight_units(x=out$raw)
  # Cases where units were extracted into age_category column
  out$raw$age_units[is.na(out$raw$age_units)] = out$raw$age_category[is.na(out$raw$age_units)]
  out$raw = extract_units(x=out$raw, units_col="age_units", conv_col="age_normalized", unit_type="age")
  out$raw$age_units_original = out$raw$age_units
  # Extrapolate age
  out = norm_extrapolate(x=out, f=f, extrap_type="age", log_path=log_path)
  # Missing age
  out = check_missing(x=out, miss_col = "age", f=f, log_path=log_path, flag=FALSE)
  #Missing units
  out = check_missing_units(x=out, f=f, units_col="age_units", log_path=log_path, flag=FALSE)
  if(nrow(out$missing_units)){
    out$missing_units$age_units = NA #reverting missing units back to NA  
  }
  
  out$unmatched_species = out$raw %>% dplyr::filter(!species %in% age_dict$species)
  if(nrow(out$unmatched_species)){
    out$unmatched_species$age_category = "unmatched_species"  
  }

  if(nrow(out$unmatched_species)){
    message("...Missing age category species match...Needs further curation - ",
            paste0(out$unmatched_species$species, collapse = "; "))
    log_CvT_doc_load(f=f, m="no_species_age_category_match", log_path = log_path, val=out$unmatched_species$id)
  }

  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$unmatched_species$tempID)
  #Normalize units
  out$raw$age_units = normalize_age_units(out$raw$age_units)
  #Remove extraneous characters
  out$raw = out$raw %>%
    dplyr::mutate(age_normalized = sub("mean=|old|between|GD|gestational|≥|avg", "", age_normalized) %>%
             trimws(),
           age_units = sub("gestational", "", age_units) %>% trimws(.))
  #List of ages
  out = check_subject_list(x=out, f=f, col="age_normalized", log_path=log_path)
  # +/- Group
  out = check_unit_ci(x=out, f=f, col="age_normalized", estimated=c(), log_path=log_path)
  #age range
  out = check_unit_range(x=out, f=f, col="age_normalized", estimated=c(), log_path=log_path)
  #Missed cases to handle or curate
  out$need_curation = out$raw %>%
    dplyr::mutate(age_num = suppressWarnings(as.numeric(age_normalized))) %>%
    dplyr::filter(is.na(age_num) | (is.na(age_num & is.na(age_category)))) %>%
    dplyr::select(-age_num)
  
  if(nrow(out$need_curation)){
    message("...Unhandled age normalization...Needs further curation")
    log_CvT_doc_load(f=f, m="unhandled_age_normalize_case", log_path=log_path, val=out$need_curation$id)
  }
  
  if (isTRUE(debug)) {
    return(out)
  }
  
  out$raw = out$raw %>% dplyr::filter(!tempID %in% out$need_curation$tempID) %>%
    dplyr::mutate(age_normalized = as.numeric(age_normalized))
  out$to_convert = out$raw
  out$raw = NULL
  #Ready for conversion
  #Combine and convert prepped datasets
  out$mapped_age = dplyr::bind_rows(out$ci, out$unit_range, out$to_convert)
  #Remove mapped dataframes
  out$ci = NULL; out$unit_range = NULL; out$to_convert = NULL
  if(nrow(out$mapped_age)){
    out$mapped_age = map_age_category(x=out$mapped_age, dict=age_dict)
  }
  out$matching_err = out$mapped_age %>% dplyr::filter(!tempID %in% out$mapped_age$tempID[out$mapped_age$age_category %in% names(age_dict)])
  out$mapped_age = out$mapped_age %>% dplyr::filter(!tempID %in% out$matching_err$tempID)
  if(nrow(out$matching_err)){
    m = out$matching_err$age_category %>% unique() %>% unlist()
    message("...Age category matching error: ", paste0(m, collapse=", "))
    log_CvT_doc_load(f=f, m="age_category_match_error", log_path=log_path, val=out$matching_err$id)
  }
  
  # Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  #Convert to NA for all lists that were not normalized
  out = lapply(names(out), function(n){
    if(n %in% c("extrapolate", "mapped_age")){
      return(out[[n]])
    } else{
      convert_cols_to_NA(out[[n]], col_list=c("age_normalized", "age_category")) %>%
        return()
    }
  }) %T>% { names(.) <- names(out) }
  #Convert age_normalized to numeric for unconverted lists
  out = lapply(out, function(x){ 
    x = x %>% dplyr::mutate(age_normalized = suppressWarnings(as.numeric(age_normalized)))
  })
  # Remove empty list elements
  out = out[sapply(out, nrow) > 0]
  out %>% 
    dplyr::bind_rows() %>% 
    dplyr::rename(age_units_normalized = age_units) %>%
    dplyr::arrange(tempID) %>% 
    dplyr::select(-tempID) %>% 
    return()
  #Match to age category
  #Values represent the lower threshold for age inclusion in this category
  #Younger than infant was categorized "neonate"
}
