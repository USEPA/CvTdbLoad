#Script to compile chemicals in need of curation

#Desired output: PMID, other_study_identifier, chemical_name, other_chemical_name, chemical_casrn
#unique list across the study and series sheets (difference in dosed vs. analyte)
library(dplyr); library(readxl); library(purrr)
outputDir = "L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\Format QA"
#outputDir = "L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\Format QA\\0_to_qa_format\\Needs Admin Check"
#f_list = list.files(outputDir, pattern="_CvT_")

f_list = list.files(outputDir,
                    pattern=".xlsx",#"_CvT_", 
                    full.names = TRUE,
                    recursive = TRUE)
f_list = f_list[!grepl("qa_log", f_list)]

#Load columns of interest (ignore temp files that start with '~')
problem_docs = list()
chems <- lapply(f_list[!grepl("^~", f_list)], function(x){
  message("File: ", x)
  #fn = paste0(outputDir, "\\", x)
  fn=x
  #Document PMID, other_study_identifier
  doc = tryCatch({
    readxl::read_xlsx(fn, sheet="Documents") %>% select(pmid, other_study_identifier) %>% distinct()
    },
    error = function(e){ 
      problem_docs = append(problem_docs, x)
    return(NULL)
  })
  if(is.null(doc)) return(NULL)
  #Studies test_substance_name  
  st = tryCatch({
    readxl::read_xlsx(fn, sheet="Studies") %>% 
      select(id, test_substance_name, test_substance_name_secondary, test_substance_casrn) %>% 
      distinct()
    },
    error = function(e){
      problem_docs = append(problem_docs, x)
    return(NULL)
  })
  if(is.null(st)) return(NULL)
  #Series analyte_name\
  se = tryCatch({
    readxl::read_xlsx(fn, sheet="Series") %>% 
      select(fk_study_id, analyte_name, analyte_name_secondary, analyte_casrn) %>% 
      distinct()
    },
    error = function(e){
      problem_docs = append(problem_docs, x)
    return(NULL)
  })
  if(is.null(se)) return(NULL)
  
  #Combine into unique chemical list by document identifiers
  out = tryCatch({
    left_join(st, se, by=c("id"="fk_study_id")) %>%
      select(-id) %>%
    # data.frame(chemical_name = append(st$test_substance_name, se$analyte_name), 
    #            chemical_name_secondary = append(st$test_substance_name_secondary,
    #                                             se$analyte_name_secondary),
    #            chemical_casrn = append(st$test_substance_casrn, se$analyte_casrn),
    #            stringsAsFactors = FALSE) %>%
      mutate(pmid = doc$pmid,
             other_study_identifier=doc$other_study_identifier) %>%
      distinct()
  },
  error= function(e){
    problem_docs = append(problem_docs, x)
    return(NULL)
  })
  
  return(out)
}) %>% 
  purrr::compact() %>%
  bind_rows()
