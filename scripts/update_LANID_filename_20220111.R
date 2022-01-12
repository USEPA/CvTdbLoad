#Script to update CvT template filenames to use LAN ID instead of initials
require(dplyr); require(magrittr)

outputDir = "L:/Lab/NCCT_ExpoCast/ExpoCast2021/CvT-CompletedTemplates/Format QA"
initials_dict = #readxl::read_xlsx("input/dictionaries/initials_LANID.xlsx") %>%
  #  filter(!is.na(LANID))
  readxl::read_xlsx("L:/Lab/NCCT_ExpoCast/ExpoCast2021/CvT-CompletedTemplates/CvT_roles.xlsx") %>%
  filter(!is.na(`LAN ID`), grepl("yes", `LAN ID Verified`, ignore.case = TRUE))
#initialize_CvTdb()
#initialize_CvTdb_from_RDat("L:/Lab/HEM/T_Wall_Projects_FY20/CvT Database/CvT_dump_20210825.RData")
fileList = list.files(outputDir, full.names = TRUE, pattern=".xlsx", recursive=TRUE)
fileList = fileList[!grepl("~|_normalize|_log|template_metadata", fileList)] #Remove tmp files

out = data.frame(orig=fileList, stringsAsFactors = FALSE) %>%
  mutate(basename = basename(orig),
         initials = gsub("CvT_data_template_articles_|.xlsx|HERO|PMID|[[:digit:]]+_","", basename))

to_curate_initials = out %>%
  filter(!initials %in% initials_dict$initials) %>%
  select(initials) %>% 
  unique()

out$fixed = lapply(seq_len(nrow(out)), function(r){
  fix = out$orig[r]
  for(i in seq_len(nrow(initials_dict))){
    fix = gsub(paste0("_", initials_dict$Initials[i],".xlsx"), 
               paste0("_", initials_dict$`LAN ID`[i], ".xlsx"), 
               fix,
               ignore.case = TRUE)
    fix = gsub(paste0("_", initials_dict$Initials[i],"_"), 
               paste0("_", initials_dict$`LAN ID`[i], "_"), 
               fix,
               ignore.case = TRUE)
  }
  return(fix)
}) %>% unlist()

out$name_changed = ifelse(out$orig==out$fixed, "No", "Yes")
out$fix_base = out$fixed %>% basename()
out = out %>% filter(name_changed == "Yes")
writexl::write_xlsx(x=list(out), path=paste0("output/log_template_initials_to_LANID_",Sys.Date(),".xlsx"))

#Insert logic to rename templates with fixed name
renamed = 0
skipped = 0
no_exist = 0
for(i in seq_len(nrow(out))){
  old = out$orig[i]
  new = out$fixed[i]
  if(old==new){
    skipped = skipped + 1
    next #Skip if not change made
  }
  
  if (file.exists(old)) {
    renamed = renamed + 1
    #message("Renaming: ", new)
    #file.rename(old, new)
  } else {
    no_exist = no_exist + 1
    message("File does not exist: ", old)
    next
  }
}

