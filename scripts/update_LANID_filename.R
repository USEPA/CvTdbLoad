#Script to update CvT template filenames to use LAN ID instead of initials
require(dplyr); require(magrittr)

outputDir = "L:/Lab/NCCT_ExpoCast/ExpoCast2021/CvT-CompletedTemplates/Format QA"
# initials_dict = readxl::read_xlsx("input/dictionaries/initials_LANID.xlsx") %>%
#   filter(!is.na(LANID))
initials_dict = readxl::read_xlsx("L:/Lab/NCCT_ExpoCast/ExpoCast2021/CvT-CompletedTemplates/CvT_roles.xlsx") %>%
  filter(!is.na(`LAN ID`), `LAN ID Verified` = "Yes")
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
    fix = gsub(paste0("_", initials_dict$initials[i],".xlsx"), 
               paste0("_", initials_dict$LANID[i], ".xlsx"), 
               fix,
               ignore.case = TRUE)
    fix = gsub(paste0("_", initials_dict$initials[i],"_"), 
               paste0("_", initials_dict$LANID[i], "_"), 
               fix,
               ignore.case = TRUE)
  }
  return(fix)
}) %>% unlist()

out$fix_base = out$fix %>% basename()

writexl::write_xlsx(out=out, x="output/log_template_initials_to_LANID.xlsx")

#Insert logic to rename templates with fixed name
for(i in seq_len(nrow(out))){
  old = out$orig[i]
  new = out$fixed[i]
  if (file.exists(old)) {
    #message("Renaming: ", new)
    #file.rename(old, new)
  } else {
    message("File does not exist: ", old)
  }
}

