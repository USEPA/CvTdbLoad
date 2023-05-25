# id_list = c(32, 60, 14, 1, 36, 18, 38, 59, 46, 45, 51, 129, 130, 132, 133, 135, 131, 158, 159, 160, 161, 162, 164, 157, 170, 159, 137, 175, 141, 6, 24785, 24983)
# id_list = c(1, 129, 130, 132, 133, 131, 135, 6, 137, 134, 139, 141, 14, 18, 149, 150, 24983, 151, 153, 157, 158, 159, 32, 160, 161, 162, 36, 164, 38, 165, 166, 167, 170, 169, 171, 45, 46, 175, 172, 173, 174, 51, 59, 60, 192, 24785)

# id_list = c(11, 14, 17, 22, 30, 31, 46, 48, 50, 51, 52, 62, 63, 64, 128, 192, 244)

pull_QC_templates <- function(id_list){
  for(id in id_list){
    pmid = query_cvt(paste0("SELECT pmid FROM cvt.documents where id = ", id))
    if(file.exists(paste0("output/CVTDB_QC/", id, "_PMID", pmid$pmid,"_document_QC_template_20230124.xlsx"))){
      next
    }
    message("Converting document ID ", id)
    out = cvtdb_to_template(id=list(id=id), 
                            template_path="L:/Lab/NCCT_ExpoCast/ExpoCast2022/CvT-CompletedTemplates/CvT_data_template_articles.xlsx", 
                            template_map="input/template_map.xlsx")
    writexl::write_xlsx(out, paste0("output/CVTDB_QC/", id, "_PMID", out$Documents$pmid,"_document_QC_template_20230124.xlsx"))
  } 
}
