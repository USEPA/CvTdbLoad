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
  conv = list(`percentage`=list("%", "% of original dose", "percent", "percentage", "% Dose", "% dose", "cumulative %", "perc_dose", "% 14C-PFOA recovered"),
              `ng/ml`=list("ng/ml", "ng/mL"),
              `ng/l`=list("ng/l", "ng/L"),
              `ng/g`=list("ng/g", "ng/g-tissue"),
              ug=list("ug", "ug concentration equivalents", "?g"),
              `ug/ml`=list("mcg/mL", "mcg/ml", "ug/ml", "ugml", "ug/mL"),
              `ug/l`=list("ug/l", "ug/L", "ugL"),
              `ug/mg`=list("ug/mg"),
              `ug/g`=list("ug/g", "ug/g liver", "ugg", "µg/g"),
              `ug/kg`=list("ug/kg", "ugkg", "µgkg"),
              mg=list("mg"),
              `mg/ml`=list("mg/ml", "mgmL"),
              `mg/l`=list("mg/l", "mg/L"),
              `mg/kg`=list("mg/kg", "mgkg", "mg kg", "mgkgday"),
              `mg/m^3`=list("mgm3"),
              `nl/g`=list("nl gasg"),
              `ul/kg`=list("µl liquidkg"),
              `pmol/ml`=list("pmole/ml"),
              `nmol`=list("nmole"),
              `umol/l`=list("um", "uM"),
              `umol/kg`=list("umolkg"),
              `mmol/m3`=list("mmolm3"),
              `pcb/kg`=list("PCBkg"),
              `ppm`=list("ppm", "ppmv")
              )
  
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
