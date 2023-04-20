reset_environment_file <- function(){
  reset = readline("Reset .Renviron file? (y/n): ")
  if(reset == "y"){
    message("Resetting .Renviron file...")
    if(!file.exists(".Renviron")){
      file.create(".Renviron")
    }
    fileConn<-file(".Renviron")
    writeLines(c('apiKey = "?key=*INSERT API KEY HERE*"',
                 'clowder_user_id="*INSERT USER ID HERE*"'), fileConn)
    close(fileConn)    
  }
  message("Done...")
}
