#Script to compile chemicals in need of curation

#Desired output: PMID, other_study_identifier, chemical_name, other_chemical_name, chemical_casrn
#unique list across the study and series sheets (difference in dosed vs. analyte)
library(dplyr); library(readxl); library(purrr)
#outputDir = "L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\Format QA"
outputDir = "L:/Lab/NCCT_ExpoCast/ExpoCast2021/CvT-CompletedTemplates/Format QA/1_qa_format_complete"
fileList = list.files(outputDir, full.names = TRUE, pattern=".xlsx")
fileList = fileList[!grepl("~|normalized_", fileList)] #Remove tmp files
fileList = c("L:/Lab/NCCT_ExpoCast/ExpoCast2021/CvT-CompletedTemplates/Format QA/0_to_qa_format/toQA/20210501_SPU_rat_PK-CvT_No4_77chem_clc_AJ.xlsx",
             "L:/Lab/NCCT_ExpoCast/ExpoCast2021/CvT-CompletedTemplates/Format QA/0_to_qa_format/toQA/20210106_SPU_rat_PK-CvT_No1_159chem_CRT2020_0c00009(Kamiya et al)_clc_AJ.xlsx")
#outputDir = "L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\Format QA\\0_to_qa_format\\Needs Admin Check"
#f_list = list.files(outputDir, pattern="_CvT_")

# f_list = list.files(outputDir,
#                     pattern=".xlsx",#"_CvT_", 
#                     full.names = TRUE,
#                     recursive = TRUE)
# f_list = f_list[!grepl("qa_log", f_list)]

#Load columns of interest (ignore temp files that start with '~')
problem_docs = list()
chems <- lapply(fileList[!grepl("^~", fileList)], function(x){
  message("File: ", x)
  #fn = paste0(outputDir, "\\", x)
  fn=x
  #Document PMID, other_study_identifier
  doc = tryCatch({
    readxl::read_xlsx(fn, sheet="Documents") %>% select(doc_id=id, pmid, other_study_identifier) %>% distinct()
    },
    error = function(e){ 
      problem_docs = append(problem_docs, x)
    return(NULL)
  })
  if(is.null(doc)) return(NULL)
  #Studies test_substance_name  
  st = tryCatch({
    readxl::read_xlsx(fn, sheet="Studies") %>% 
      select(study_id=id, fk_reference_document_id, test_substance_name, test_substance_name_secondary, test_substance_casrn) %>% 
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
    left_join(doc, st, by=c("doc_id"="fk_reference_document_id")) %>%
    left_join(se, by=c("study_id"="fk_study_id")) %>%
      #select(-id) %>%
    # data.frame(chemical_name = append(st$test_substance_name, se$analyte_name), 
    #            chemical_name_secondary = append(st$test_substance_name_secondary,
    #                                             se$analyte_name_secondary),
    #            chemical_casrn = append(st$test_substance_casrn, se$analyte_casrn),
    #            stringsAsFactors = FALSE) %>%
      mutate(#pmid = doc$pmid,
             #other_study_identifier=doc$other_study_identifier,
             file = x) %>% #Add filename
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

#Process for batch searching
#Assign unique record ID
tmp = chems %>%
  mutate(uuid = 1:n())
#Split out test vs. analyte chemicals
analyte_chems = tmp %>%
  select(-test_substance_name, -test_substance_name_secondary, -test_substance_casrn) %>%
  dplyr::rename(name=analyte_name,
                name_secondary=analyte_name_secondary,
                casrn=analyte_casrn) %>%
  mutate(chem_type = "analyte")
  
test_chems = tmp %>%
  select(-analyte_name, -analyte_name_secondary, -analyte_casrn) %>%
  mutate(chem_type = "test") %>%
  dplyr::rename(name=test_substance_name,
                name_secondary=test_substance_name_secondary,
                casrn=test_substance_casrn)

out = rbind(test_chems, analyte_chems) %>%
  mutate(name_parentheses = gsub("\\(([^()]+)\\)", "\\1", stringr::str_extract_all(name, "\\(([^()]+)\\)")),
         name_no_parentheses = gsub("\\(([^()]+)\\)", "", name),
         name_secondary_parentheses = gsub("\\(([^()]+)\\)", "\\1", stringr::str_extract_all(name_secondary, "\\(([^()]+)\\)")),
         name_secondary_no_parentheses = gsub("\\(([^()]+)\\)", "", name_secondary))
out$name_parentheses[out$name_parentheses == "character0"] = NA
out$name_parentheses[out$name_secondary_parentheses == "character0"] = NA
#Recombine and export
writexl::write_xlsx(list(out), paste0("input/chemicals/cvt_to_curate_unique_chemicals_", Sys.Date(),".xlsx"))
