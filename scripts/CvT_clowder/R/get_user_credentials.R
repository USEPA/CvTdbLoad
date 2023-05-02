get_user_credentials <- function(){
  userID <- Sys.getenv("clowder_user_id")
  apiKey <- Sys.getenv("apiKey")# "?key=12345-12345-asdf"
  
  if(interactive()){
    message("Handle interactive session needs")
    if(grepl("*INSERT USER ID HERE*", userID)){
      userID <- readline(prompt="Enter Clowder UserID URL: " )
    } 
    
    #INSERT LOGIC TO CHECK USERID IS CORRECT
    
    if(grepl("*INSERT API KEY HERE*", apiKey)){
      apiKey = paste0("?key=", readline(prompt="Enter Clowder API Key: " ))
    }
    
    if(GET(paste0("https://clowder.edap-cluster.com/api/datasets", apiKey)) %>% 
       status_code() != 200){
      stop("Incorrect apiKey...")
    }
  } else {
    cat("Enter Clowder UserID URL:\n")
    userID <- readLines(file("stdin"), n = 1L)
    cat("Enter Clowder apiKey:\n")
    apiKey <- readLines(file("stdin"), n = 1L)
  }
  
  userID <<- userID
  apiKey <<- apiKey
  #return(list(userID=userID, apiKey=apiKey))
}
