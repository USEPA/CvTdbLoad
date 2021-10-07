#Script to check if templates are found in Original Docs and Format QA folders
library(dplyr); library(tidyr)
qa_list = list.files("L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\Format QA", 
                     recursive=TRUE,
                     pattern=".xlsx",
                     full.names = TRUE)
orig_list = list.files("L:\\Lab\\NCCT_ExpoCast\\ExpoCast2021\\CvT-CompletedTemplates\\Original Extracted Templates",
                       pattern=".xlsx",
                       full.names=TRUE)
###########################################################################################
#Find original documents missing in the Format QA workflow
missing_qa = data.frame(orig_file = orig_list, stringsAsFactors = FALSE) %>%
  mutate(basename = basename(orig_file))

tmp1 = qa_list %>% basename()
tmp = missing_qa$basename[!missing_qa$basename %in% tmp1]

missing_qa = missing_qa %>%
  filter(grepl(paste0(tmp, collapse="|"), basename)) %>%
  filter(!grepl("incomplete", basename, ignore.case=TRUE)) %>%
  separate(basename, c("ID"), remove=FALSE) %>%
  mutate(ID = gsub("PMID", "", ID))

tmp = lapply(missing_qa$ID, function(x){
  if(any(grepl(x, qa_list))){
    return(NULL)
  } else {
    return(x)
  }
}) %>% purrr::compact() %>% unlist()

if(purrr::is_empty(tmp)){
  missing_qa = "None missing"
} else {
  missing_qa = filter(missing_qa, grepl(paste0(tmp, collapse="|"), ID))  
}
###########################################################################################
#Find Format QA docs missing from Original Docs
missing_orig = data.frame(qa_file = qa_list, stringsAsFactors = FALSE) %>%
  mutate(basename = basename(qa_file))

tmp1 = orig_list %>% basename()
tmp = missing_orig$basename[!missing_orig$basename %in% tmp1]

missing_orig = missing_orig %>%
  filter(grepl(paste0(tmp, collapse="|"), basename)) %>%
  filter(!grepl("incomplete", basename, ignore.case=TRUE),
         !basename %in% c("qa_log.xlsx")) %>%
  mutate(clean = gsub("PMID", "", basename) %>%
           gsub("HERO", "", .)) %>%
  separate(basename, c("ID"), remove=FALSE) %>%
  mutate(ID = gsub("PMID", "", ID))

tmp = lapply(missing_orig$ID, function(x){
  if(any(grepl(x, orig_list))){
    return(NULL)
  } else {
    return(x)
  }
}) %>% purrr::compact() %>% unlist()

if(purrr::is_empty(tmp)){
  missing_orig = "None missing"
} else {
  missing_orig = filter(missing_orig, grepl(paste0(tmp, collapse="|"), ID))
}

if(is.data.frame(missing_qa)){
  message("Original Docs not in Format QA: \n", paste0(missing_qa$orig_file, collapse=" \n "))  
} else {
  message("Original Docs not in Format QA: ", missing_qa)  
}

if(is.data.frame(missing_orig)){
  message("QA Docs not in Original: \n", paste0(missing_orig$qa_file, collapse=" \n "))  
} else {
  message("QA Docs not in Original: ", missing_orig)  
}

