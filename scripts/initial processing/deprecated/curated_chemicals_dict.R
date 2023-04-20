#Script to create/push curated chemicals dictionary
#'@description Helper function to get the concentration medium dictionary form the CvT database
chemicals_get_dict <- function(){
  return(query_cvt("SELECT * FROM cvt.chemicals"))
}

chemical_update_dict <- function(dict_file=NULL, dat=NULL){
  if(!is.null(dict_file)){
    if(file.exists(dict_file)){
      push_tbl_to_db(dat=readxl::read_xlsx(dict_file),
                     tblName="cvt.chemicals",
                     append=TRUE)
    } else {
      message("...input dict_file does not exist...cannot push dictionary")
    } 
  }
  
  if(!is.null(dat)){
    push_tbl_to_db(dat=dat,
                   tblName="cvt.chemicals",
                   overwrite=FALSE, append=TRUE)
  }
}


# tmp = readxl::read_xlsx(dict_file) %>% 
#   filter(!is.na(`Top HIT DSSTox_Substance_Id`)) %>%#, Validated != FALSE) %>%
#   select(dsstox_substance_id = `Top HIT DSSTox_Substance_Id`, dsstox_casrn=`Top Hit Casrn`, 
#          preferred_name = `Top Hit Name`, curation_notes = `Lookup Result`) %>% 
#   mutate(chemistry_team_mapping = 1) %>%
#   filter(!dsstox_substance_id %in% get_chemicals_dict()$dsstox_substance_id)
