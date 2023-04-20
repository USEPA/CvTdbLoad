#' @param df Input chemical data to match
#' @param f_name File path to curated chemicals (multiple sheets)
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  [filter][dplyr::filter], [mutate][dplyr::mutate], [left_join][dplyr::left_join], [distinct][dplyr::distinct], [bind_rows][dplyr::bind_rows], [select][dplyr::select], [arrange][dplyr::arrange]
#' @rdname chemical_curation_match_curated_chemicals
#' @export 
#' @importFrom dplyr filter mutate left_join distinct bind_rows select arrange
chemical_curation_match_curated_chemicals <- function(df, f_name){
  # tmp = lapply(fileList, function(f){
  #   s_list = load_sheet_group(fileName = f, template_path = template_path)
  #   s_list$Studies %>% select(id, name=test_substance_name, name_secondary=test_substance_name_secondary, casrn=test_substance_casrn) %>% 
  #     distinct() %>%
  #     mutate(doc = f)
  # }) %>%
  #   bind_rows()
  #f_name = "input/chemicals/curated_chemicals_comparison_2021-11-23.xlsx"
  #Get curated chemicals
  curation_files = get_curation_files(f_name)
  #Prep chemical names
  df = prep_chemical_name_match(df)
  df$tempID = 1:nrow(df)
  out = list()
  #Remove names that are already DTXSID values (HERO docs)
  has_DTXSID = df %>% 
    dplyr::filter(grepl("DTXSID", name)) %>%
    dplyr::mutate(DTXSID = name,
           chemistry_team_mapping = 1,
           PREFERRED_NAME = NA,
           CASRN = NA)
  #No longer trying to match to httk for preferred_name and cas for provided DTXSID
  # if(nrow(has_DTXSID)){
  #   #See if can match to httk package DTXSID chemical information
  #   has_match = has_DTXSID %>%
  #     filter(DTXSID %in% httk::get_cheminfo(info="DTXSID"))
  #   
  #   if(nrow(has_match)){
  #     has_match = has_match %>%
  #     #User httk to get chemical information from DTXSID
  #     left_join(httk::get_chem_id(dtxsid = .$DTXSID) %>% 
  #                 data.frame() %>%
  #                 distinct() %>%
  #                 dplyr::rename(DTXSID = dtxsid, PREFERRED_NAME = chem.name, CASRN = chem.cas),
  #               by="DTXSID")
  #   }
  #   no_match = has_DTXSID %>%
  #     filter(!tempID %in% has_match) %>%
  #     mutate(PREFERRED_NAME = NA,
  #            CASRN = NA)
  #   has_DTXSID = rbind(has_match, no_match)
  # }
    
  df = df %>% dplyr::filter(!tempID %in% has_DTXSID$tempID)
  #Try matching CASRN first
  #Match 1 - CASRN
  if(nrow(curation_files$batch_casrn)){
    out$matched_casrn = df %>%
      dplyr::left_join(curation_files$batch_casrn,# %>% select(INPUT, FOUND_BY, DTXSID),
                by=c("casrn"="INPUT")) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(DTXSID))
  }
  df = df %>% dplyr::filter(!tempID %in% out$matched_casrn$tempID)
  
  #Try matching full name first
  #Match 1 - Full name
  if(nrow(curation_files$batch_full)){
    out$matched_full = df %>%
      dplyr::left_join(curation_files$batch_full,# %>% select(INPUT, FOUND_BY, DTXSID),
                by=c("name"="INPUT")) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(DTXSID))
  }
  df = df %>% dplyr::filter(!tempID %in% out$matched_full$tempID)
  #Match 2 (unmatched from 1) - No parentheses name
  if(nrow(curation_files$batch_no_par)){
    out$matched_no_par = df %>% 
      dplyr::left_join(curation_files$batch_no_par,# %>% select(INPUT, FOUND_BY, DTXSID),
                by=c("name_no_parentheses"="INPUT")) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(DTXSID))
  }
  df = df %>% dplyr::filter(!tempID %in% out$matched_no_par$tempID)
  out$unmatched = df %>%
    dplyr::mutate(DTXSID = NA, 
           FOUND_BY = NA,
           PREFERRED_NAME = NA,
           CASRN = NA)
  
  #Remove empty list elements
  out = out[sapply(out, nrow) > 0] %>%
    dplyr::bind_rows()
  if(nrow(out)){
    out = out %>%
      #Add curation boolean (see CvTdb for column name)
      dplyr::mutate(chemistry_team_mapping = ifelse(!is.na(DTXSID), 0, NA)) %>%
      dplyr::select(-FOUND_BY)
  }
  #Recombine has_DTXSID which has a chemistry_team_mapping = 1
  out = rbind(out, has_DTXSID) %>%
    dplyr::arrange(tempID) %>% 
    dplyr::select(-tempID)
  
  return(out %>% dplyr::select(dsstox_substance_id=DTXSID, dsstox_casrn=CASRN, preferred_name = PREFERRED_NAME, chemistry_team_mapping))
}
