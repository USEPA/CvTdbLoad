#Function to help match template documents to Clowder files

#'@description 
#'@param df Input template document's sheet for mapping
#'@param dsID Clowder dataset ID to pull from.
#'@param apiKey API key to access Clowder repo
match_clowder_docs <- function(df=NULL, dsID=NULL, apiKey=NULL){
  if(is.null(apiKey)) stop("Error: missing required Clowder apiKey")
  if(is.null(dsID)) stop("Error: missing required Clowder dataset ID")
  
  if(!"pdf_filepath" %in% names(df)){ df$pdf_filepath = NA }
  # Attempt to match to PMID
  c_docs = get_clowder_docList_2(dsID=dsID, apiKey=apiKey)
  pmid_match = df %>%
    filter(!is.na(pmid)) %>%
    mutate(pdf_filepath = paste0("PMID", pmid, ".pdf")) %>%
    #Match to Clowder filename
    left_join(c_docs,
              by=c("pdf_filepath"="filename")) %>%
    # Filter to those that matched
    filter(!is.na(clowder_file_id))
  df = df %>%
    filter(!id %in% pmid_match$id)
  
  other_match = df %>%
    filter(!is.na(other_study_identifier)) %>%
    mutate(pdf_filepath = paste0(other_study_identifier, ".pdf")) %>%
    # Match to Clowder filename
    left_join(c_docs,
              by=c("pdf_filepath"="filename")) %>%
    # Filter to those that matched
    filter(!is.na(clowder_file_id))
  
  df = df %>%
    filter(!id %in% other_match$id)
  
  # Recombine and return
  rbind(df, pmid_match, other_match) %>%
    arrange(id) %>%
    return()
}

#'@title Get Clowder Document List (varient of the other function in utilities)
#'@description This is a helper function to get a list of documents available in a Clowder dataset
#'@param dsID Clowder dataset ID to pull from.
#'@param apiKey The API key required for a user to access the Clowder dataset
#'@return Returns a dataframe with file details of: filename and ClowderID.
#'@import dplyr
get_clowder_docList_2 <- function(dsID=NULL, apiKey=NULL){
  Sys.sleep(0.25) #Wait between requetss
  baseurl = "https://clowder.edap-cluster.com/api"
  #Get dataset IDs of interest
  httr::GET(paste0("https://clowder.edap-cluster.com/api/datasets/",
                                   dsID,"/files?key=",
                                   apiKey)) %>% httr::content() %>%
    tibble(clowder_file_id = purrr::map_chr(.,"id"), filename = purrr::map_chr(.,"filename")) %>%
    select(-1) %>%
    return()
}

#'@description This is a helper function to match already loaded CvT document entries to Clowder docs.
#'@param dsID Clowder dataset ID to pull from.
#'@param apiKey The API key required for a user to access the Clowder dataset
#'@return Returns a dataframe with file details of: filename and ClowderID.
#'@import dplyr
match_post_upload <- function(dsID = NULL, apiKey = NULL){
  #Get all documents in CvT without Clowder ID
  docs = query_cvt("SELECT id, pmid, other_study_identifier FROM cvt.documents where clowder_file_id is NULL") %>%
    filter(!is.na(pmid) | !is.na(other_study_identifier))
  #Match to Clowder ID
  output = match_clowder_docs(df=docs, dsID=dsID, apiKey=apiKey) %>%
    filter(!is.na(clowder_file_id)) %>%
    select(id, clowder_file_id)
  #Push to CvT
  #Push updates
  con = connect_to_CvT()
  dbWriteTable(con, value = output, name=c("cvt", "temp_tbl"), overwrite=TRUE, row.names=FALSE)  
  dbDisconnect(con)
  
  query = paste0("UPDATE cvt.documents h SET clowder_file_id = m.clowder_file_id",
                 " FROM cvt.temp_tbl m",
                 " WHERE h.id = m.id")  
  #Make update (only uncomment when ready to use)
  #query_cvt(query=query)
  query_cvt("DROP TABLE cvt.temp_tbl")
}