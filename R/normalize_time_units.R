#' @title normalize_time_units
#' @description Function to normalize time unit names to a standard format.
#' @param x Input vector of time units.
#' @return Modified `x` vector with normalized time unit names.
#' @rdname normalize_time_units
#' @export 
normalize_time_units <- function(x){
  #Convert time
  x = tolower(x)
  conv = list(s=list("s", "sec", "second", "seconds"),
              min=list("min", "minute", "minutes", "minutess"),
              hr=list("hr","hour", "hours", "h"),
              day=list("day", "days", "gestation day"),
              week=list("week", "weeks", "wk", "wks"),
              month=list("month", "months")
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
