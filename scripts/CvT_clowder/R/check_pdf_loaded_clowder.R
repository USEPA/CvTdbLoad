#'@title Get Clowder Document List
#'@description This is a helper function to get a list of documents available in a Clowder dataset
#'@param df Dataframe to modify with a "PDF in Clowder" field. Must have PMID values to compare to Clowder documents.
#'@param apiKey The API key required for a user to access the Clowder dataset
#'@return Returns a modified version of the input df with "PDF in Clowder" indicator filled in.
#'@import dplyr
check_pdf_loaded_clowder <- function(df=NULL, apiKey=NULL){
  clowderDocs = clowder_get_docList(clowderDir=c("CvT Data Documents"), 
                                    apiKey = apiKey) %>%
    mutate(PMID = gsub(".pdf","",filename) %>%
             gsub("PMID", "", .) %>% 
             as.numeric()) %>%
    select(PMID)
  return(df %>%
    mutate(`PDF in Clowder` = ifelse(PMID %in% clowderDocs$PMID, 1, 0)))
}
