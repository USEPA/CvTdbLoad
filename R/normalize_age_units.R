#' @title normalize_age_units
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname normalize_age_units
#' @export 
normalize_age_units <- function(x){
  if(!length(x)){
    return(x)
  }
  #Convert time
  x = tolower(x)
  conv = list(s=list("s", "sec", "second", "seconds"),
              min=list("min", "minute", "minutes"),
              hr=list("hr","hour", "hours", "h"),
              day=list("day", "days"),
              week=list("week", "weeks", "wk", "wks"),
              month=list("month", "months"),
              year=list("year", "years", "yr", "yrs")
  )
  
  x = lapply(x, function(s){
    for(c in names(conv)){
      if(!s %in% conv[[c]]){
        next
      } else {
        return(c)  
      }
    }
    return(s)
  }) %>% unlist()
  return(x)
}
