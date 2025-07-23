#' @title normalize_conc_units
#' @description Helper function to normalize concentration unit names.
#' @param x Input vector of conc units.
#' @return Modified input `x` vector with normalized concentration unit names.
#' @rdname normalize_conc_units
#' @export 
normalize_conc_units <- function(x){
  if(length(x)==0){
    return(x)
  }
  # Convert conc units
  # x = tolower(x) # Commented out due to edge cases like meter (m) and molarity (M)
  conv = list(`percentage`=list("%", "% of original dose", "percent", "percentage", 
                                "% Dose", "% dose", "cumulative %", "perc_dose", 
                                "% 14C-PFOA recovered", "fraction absorbed",
                                "Percent Dose", "Percent total dose"),
              `ng/ml`=list("ng/ml", "ng/mL"),
              `ng/l`=list("ng/l", "ng/L"),
              `ng/g`=list("ng/g", "ng/g-tissue", "ng/g lipid weight", "ng/g whole weight basis"),
              ug=list("ug", "ug concentration equivalents"),
              `ug/ml`=list("mcg/mL", "mcg/ml", "ug/ml", "ugml", "ug/mL"),
              `ug/l`=list("ug/l", "ug/L", "ugL"),
              `ug/mg`=list("ug/mg"),
              `ug/g`=list("ug/g", "ug/g liver", "ugg", "µg/g", "ug/g wet wt", "ug/g-tissue", "ug/g \\(wet wt tissue\\)"),
              `ug/kg`=list("ug/kg", "ugkg", "µgkg"),
              mg=list("mg"),
              `mg/ml`=list("mg/ml", "mgmL"),
              `mg/l`=list("mg/l", "mg/L"),
              `mg/kg`=list("mg/kg", "mgkg", "mg kg", "mgkgday"),
              `mg/m^3`=list("mgm3"),
              `nl/g`=list("nl gasg"),
              `ul/kg`=list("µl liquidkg", "ul liquidkg"),
              `pmol/ml`=list("pmole/ml"),
              `nmol`=list("nmole"),
              `nmol/g`=list("nmole/g"),
              `nmol/ml`=list("nmol/ml", "nmoles/ml", "nmol/mL", "nmole/ml"),
              `nmol/l`=list("nmol/l", "nmol/L", "nmol/liter"),
              `umol/l`=list("umol/l", "um", "uM"),
              `umol/kg`=list("umolkg"),
              `mmol/m3`=list("mmolm3"),
              `pcb/kg`=list("PCBkg"),
              `ppm`=list("ppm", "ppmv"),
              `ppb`=list("ppb", "ppbv")
              )
  
  x = lapply(x, function(s){
    for(c in names(conv)){
      if(!s %in% conv[[c]]){
        # Add \b to only match whole words
        if(grepl(paste0("\b", conv[[c]], "\b", collapse="|"), s)){
          #message("Potential match for: ", s)
          browser()
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
