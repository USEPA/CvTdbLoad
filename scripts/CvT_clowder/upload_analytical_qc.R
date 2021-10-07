#Adapted from Jason Brown Script "upload_analytical_qc.R" 2020-11-23
library(httr); library(dplyr); library(purrr); library(magrittr); library(tidyr)
source("metadata_functions.R")
#datadocumentDir = "C:\\Users\\JWALL01\\OneDrive - Environmental Protection Agency (EPA)\\Profile\\Desktop\\CvT PDFs"
datadocumentDir = "../CvT PDFs"
#extractedDir = "L:\\Lab\\NCCT_ExpoCast\\ExpoCast2020\\CvT-CompletedTemplates"
extractedDir = "../QA-Jan2021"
gsURL = "https://docs.google.com/spreadsheets/d/1Uzxw7p_6zlNehtGJlBP-AkTWvydFvt9CeCTtwyeewvE/edit#gid=0"
#baseurl <- "https://clowder.edap-cluster.com/api"

#Get user credentials to use the API (userID and apiKey)
userID <- Sys.getenv("clowder_user_id")
apiKey <- Sys.getenv("apiKey")# "?key=12345-12345-asdf"

if(grepl("*INSERT USER ID HERE*", userID) | userID == ""){
  stop("Please update .Renviron file or Sys.setenv() with 'clowder_user_id' URL")
} 

if(grepl("*INSERT API KEY HERE*", apiKey) | apiKey == ""){
  stop("Please update .Renviron file or Sys.setenv() with Clowder 'apiKey' with '?key=' at the start")
}
  
if(GET(paste0("https://clowder.edap-cluster.com/api/datasets", apiKey)) %>% 
   status_code() != 200){
  stop("Incorrect apiKey...")
}

#userID = credentials$userID
#apiKey = credentials$apiKey
  
#Upload PDFs and metadata first, so extracted templates can have "PDF in Clowder" flag
upload_to_clowder(metadata=create_meta_data_file(outputName="datadocument_files_metadata.csv",
                                                 type="datadocument",
                                                 fileDir=datadocumentDir,
                                                 gsURL=gsURL,
                                                 metaCurated=TRUE),
                  apiKey = apiKey, 
                  datasetName = "CvT Data Documents",
                  fileDir = datadocumentDir,
                  type = "datadocument")
#NEED TO FIGURE OUT HOW TO LOAD INTO SUBFOLDERS ON CLOWDER
upload_to_clowder(metadata=create_meta_data_file(outputName="extracted_files_metadata.csv",
                                                 type="extracted",
                                                 fileDir=extractedDir,
                                                 gsURL=gsURL,
                                                 metaCurated=TRUE,
                                                 apiKey=apiKey),
                  apiKey = apiKey, 
                  datasetName = "CvT Raw Extracted",
                  fileDir = extractedDir,
                  type = "extracted")

reset_environment_file()
