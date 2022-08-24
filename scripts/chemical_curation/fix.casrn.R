#--------------------------------------------------------------------------------------
#' Fix a CASRN that has one of several problems
#'
#' @param casrn Input CASRN to be fixed
#' @param verbose if TRUE, print hte input values
#' @return the fixed CASRN
#
#--------------------------------------------------------------------------------------
fix.casrn <- function(casrn,verbose=FALSE) {
  if(verbose) cat("input: ",casrn,"\n")
  
  casrn = casrn %>%
    # Update for better removal of leading 0's
    stringr::str_remove(., "^0+") %>%
    # Add dashes where missing (assuming is valid CASRN format)
    lapply(., function(c_element){
      if(is.na(c_element)) return(c_element)
      if(grepl("NOCAS",c_element)) return(c_element)
      if(!grepl("-", c_element)) {
        nc <- nchar(c_element)
        ctemp <- c_element
        right <- substr(ctemp,nc,nc)
        mid <- substr(ctemp,nc-2,nc-1)
        left <- substr(ctemp,1,nc-3)
        return(paste(left,"-",mid,"-",right,sep=""))
      } else {
        return(c_element)
      }
    }) %>% unlist()
  
  if(verbose) cat("output: ",casrn,"\n")
  return(casrn)
}
