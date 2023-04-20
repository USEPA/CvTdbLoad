#' @description Helpfer function to normalize concentration units
#' @title FUNCTION_TITLE
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname normalize_conc_units
#' @export 
normalize_conc_units <- function(x){
  if(length(x)==0){
    return(x)
  }
  #Convert conc units
  x = tolower(x)
  conv = list(`ng/ml`=list("ng/ml", "ng/mL"),
              `mg/ml`=list("mg/ml"),
              `percentage`=list("%", "% of original dose", "percent", "% Dose", "% dose", "cumulative %"),
              `ug/g`=list("ug/g", "ug/g liver"),
              `ug/kg`=list("ug/kg"),
              `mg/kg`=list("mg/kg"),
              `ug/ml`=list("mcg/mL", "mcg/ml", "ug/ml"),
              `mg/l`=list("mg/l", "mg/L"),
              `ug/l`=list("ug/l", "ug/L"),
              `ug/ml`=list("ug/ml", "ug/mL"),
              `ng/l`=list("ng/l", "ng/L"),
              `pmol/ml`=list("pmole/ml"),
              ug=list("ug", "ug", "ug concentration equivalents", "?g"),
              mg=list("mg"))
  
  x = lapply(x, function(s){
    for(c in names(conv)){
      if(!s %in% conv[[c]]){
        if(grepl(paste0(conv[[c]], collapse="|"), s)){
          #message("Potential match for: ", s)
          return(c)
        }
        next
      } else {
        return(c)  
      }
    }
    return(s)
  }) %>% unlist()
  return(x)
}
