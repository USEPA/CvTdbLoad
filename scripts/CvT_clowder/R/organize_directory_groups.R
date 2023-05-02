#'@title Organize Clowder Extracted Templates by Directory
#'@description This is a helper function to organize extracted templates by their subdirectory groups.
#'@param df A dataframe with a filename column of extracted template filepaths.
#'@return A modified version of the input df orgnaized by file directory.
#'@import dplyr tidyr
organize_directory_groups <- function(df=NULL){
  return(test = df %>%
    separate(col=filename, into=c("QA Category", "QA Subcategory"), sep="/", 
             extra="drop", remove=FALSE) %>%
    mutate(base=basename(filename),
           `QA Subcategory` = ifelse(`QA Subcategory`==base, NA, `QA Subcategory`),
           base = gsub("_CvT_data_template_articles","",base)) %>%
    separate(col=base, into=c("PMID", "Curator Initials"), sep="_", fill="right", 
             extra="merge") %>%
    mutate(`Curator Initials`=gsub(".xlsx", "", `Curator Initials`),
           filename = basename(filename),
           uploadReady = ifelse(grepl("qa_format_complete", `QA Category`), 1, 0))
    )
}
