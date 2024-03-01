#' @description A helper function to convert input values to desired units.
#' @param x Input dataframe to convert
#' @param num Name of column with values to convert
#' @param units Name of column with units to convert from
#' @param desired Desired units to convert the input value into
#' @param MW Conversion factor to use (such as Molecular weight, tissue Density, etc.)
#' @param overwrite_units Boolean to overwrite the 'units' with desired units.
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname convert_units
#' @export 
convert_units <- function(x, num, units, desired, MW=NA, overwrite_units=FALSE){
  #Map of input units to desired output units equation
  conv = list(day = list(hr="*24", day="/1", week="/7", month="/30", year="/365"),
              week = list(hr="*24*7", day="*7", week="/1", month="/4", year="/52"),
              month = list(hr="*30*24", day="*30", week="*4", month="/1", year="/52"),
              year = list(day="*365", week="*52", month="*12", year="/1"),
              kg = list(kg="/1"), #Only care to convert to kg for all weights
              g = list(mg="*1000", kg="/1000"),
              ug = list(mg="/1000"),
              `µg` = list(mg="/1000"),
              mg = list(mg="/1", kg="/1000000"),
              lb = list(kg="/2.2"),
              mm = list(cm="/10"), #Only care to convert to cm for all heights
              cm = list(cm="/1"),
              m = list(cm="*100"),
              `in`=list(cm="*2.54"),
              ft=list(cm="*12*2.54"),
              s=list(hr="/60/60"), #Only care to convert to hr f time
              min=list(hr="/60"),
              hr=list(hr="/1"),
              `mg/kg`=list(`mg/kg`="/1", `ug/ml`=paste0("*", MW)), #1 mg/kg*MW kg/L*1L/1000mL*1000ug/mg=ug/mL --> using httk density value for MW variable (refactor name)
              `ug/kg`=list(`mg/kg`="/1000", `ug/ml`=paste0("*", MW, "/1000")),
              `µg/kg` = list(`ug/ml`=paste0("*", MW, "/1000")), 
              `g/kg`=list(`mg/kg`="*1000"),
              `ug/250 g`=list(`mg/kg`="*4/1000"),
              `ug/ml`=list(`ug/ml`="/1"),
              `ug/l`=list(`ug/ml`="/1000"),
              `ng/ml`=list(`ug/ml`="/1000"),
              `ng/l`=list(`ug/ml`="/1000000"),
              `mg/ml`=list(`ug/ml`="*1000"),
              `mg/l`=list(`ug/ml`="/1"),
              ppm=list(`ug/ml`="/1"), #1 ppm = 1 ug/mL
              ppbv = list(`ug/ml`="/1000"), #1 ppb = 0.001 ug/mL,
              ppb = list(`ug/ml`="/1000"), #1 ppb = 0.001 ug/mL,
              `nmol/l` = list(`ug/ml`=paste0("*",MW,"/1000000")), #1 nmol/L*(1mol/1000000000nmol)*(MW g/1mol)*(1000000ug/1g)*(1L/1000mL)=1*MW/1000000
              `nmol/ml` = list(`ug/ml`=paste0("*",MW,"/1000")),
              `nmoles/ml` = list(`ug/ml`=paste0("*",MW,"/1000")),
              `umol/l` = list(`ug/ml`=paste0("*",MW,"/1000")), #1000 less than nmol/l conversion 
              `pmol/ml` = list(`ug/ml`=paste0("*",MW,"/1000000")), #1 pmol/ml*(1mol/1000000000000pmol)*(MW g/1mol)*(1000000ug/1g)=1*MW/1000000
              `ug/g` = list(`ug/ml`=paste0("*", MW)), #1 ug/g*1000g/kg*MW kg/L*1L/1000mL=ug/mL --> using httk density value for MW variable (refactor name)
              `umol/kg` = list(`mg/kg`= paste0("*", MW, "/1000")), # MW is g/mol, which is the same as mg/mmol or ug/umol
              # Tissue density conversions (Density = g/mL from httk)
              `ug tissue conc` = list(`ug/ml`=paste0("*MW"))
              )
  #Convert units based on input string equation
  if(is.null(conv[[x[[units]]]][[desired]])){
    #No matching desired output
    x[[num]] = NA
  } else {
    #Get the conversion equation (e.g. 20 days to weeks is '20/7')
    equ = paste0(x[[num]], conv[[x[[units]]]][[desired]])
    x[[num]] = parse(text=equ) %>% #parse the string
      eval() %>% #evaluate the string equation
      round(., 5) #round to 5 decimal places
    if(overwrite_units){
      x[[units]] = desired #Set to converted units  
    }
  }
  return(x) 
}
