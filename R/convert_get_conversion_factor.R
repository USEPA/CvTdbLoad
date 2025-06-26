#' @description A helper function to convert input values to desired units.
#' @param conv_factor Conversion factor to use (such as Molecular weight, tissue Density, etc.)
#' @title convert_get_conversion_factor
#' @return List of conversion factors
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname convert_get_conversion_factor
#' @export
convert_get_conversion_factor <- function(conv_factor=1){
  # Map of input units to desired output units equation
  list(day = list(hr="*24", day="/1", week="/7", month="/30", year="/365"),
       week = list(hr="*24*7", day="*7", week="/1", month="/4", year="/52"),
       month = list(hr="*30*24", day="*30", week="*4", month="/1", year="/52"),
       year = list(day="*365", week="*52", month="*12", year="/1"),
       kg = list(kg="/1"), #Only care to convert to kg for all weights
       g = list(mg="*1000", kg="/1000"),
       ug = list(ug="/1", mg="/1000", 
                 # Skipping conversion for a concentration at this time
                 `ug/ml` = "*NA"),
       `µg` = list(mg="/1000"),
       mg = list(mg="/1", kg="/1000000"),
       lb = list(kg="/2.2"),
       mm = list(cm="/10"), #Only care to convert to cm for all heights
       cm = list(cm="/1"),
       m = list(cm="*100"),
       `in` = list(cm="*2.54"),
       ft = list(cm="*12*2.54"),
       s = list(hr="/60/60"), #Only care to convert to hr f time
       min = list(hr="/60"),
       hr = list(hr="/1"),
       `mg/kg` = list(`mg/kg`="/1", `ug/ml`= paste0("*", conv_factor)), #1 mg/kg*conv_factor kg/L*1L/1000mL*1000ug/mg=ug/mL --> using httk density value for conv_factor variable (refactor name)
       # TODO Handle special dosed feed and drinking water cases
       `mg/kg feed dose` = list(`mg/kg BW-day`="/1"),
       `mg/kg drinking dose` = list(`mg/kg BW-day`="/1"),
       `mg/kgbw dose` = list(`mg/kg BW`="/1"),
       
       `ng/g` = list(`ug/kg`="/1", `ug/ml`= paste0("*", conv_factor, "/1000")), # ug/mL from httk g/mL tissue density
       # TODO potentially remove
       `ug/kg` = list(`ug/ml`=paste0("*", conv_factor, "/1000")), # ug/mL from httk g/mL tissue density
       `ug/ml` = list(`ug/ml`="/1"),
       `ug/l` = list(`ug/ml`="/1000"),
       `ng/ml` = list(`ug/ml`="/1000"),
       `pg/ml` = list(`ug/ml`="/1000000"),
       `ng/l` = list(`ug/ml`="/1000000"),
       `mg/ml` = list(`ug/ml`="*1000"),
       `mg/l` = list(`ug/ml`="/1"),
       `mg/L` = list(`ug/ml`="/1"),
       `ng/kg` = list(`mg/kg`="/1000000"),
       `g/dl` = list(`ug/ml`="*10000"),
       `ug/dl` = list(`ug/ml`="/100"),
       `nmol/l` = list(`ug/ml`=paste0("*",conv_factor,"/1000000")), #1 nmol/L*(1mol/1000000000nmol)*(conv_factor g/1mol)*(1000000ug/1g)*(1L/1000mL)=1*conv_factor/1000000
       `mmol/l` = list(`ug/ml`=paste0("*",conv_factor)),
       `nmol/ml` = list(`ug/ml`=paste0("*",conv_factor,"/1000")),
       `nmoles/ml` = list(`ug/ml`=paste0("*",conv_factor,"/1000")),
       `umol/l` = list(`umol/l`="/1", `ug/ml`=paste0("*",conv_factor,"/1000")), #1000 less than nmol/l conversion 
       `pmol/ml` = list(`ug/ml`=paste0("*",conv_factor,"/1000000")), #1 pmol/ml*(1mol/1000000000000pmol)*(conv_factor g/1mol)*(1000000ug/1g)=1*conv_factor/1000000
       `ug/g tissue conc` = list(`ug/g`="/1", `ug/ml`=paste0("*", conv_factor)), #1 ug/g*1000g/kg*conv_factor kg/L*1L/1000mL=ug/mL --> using httk density value for conv_factor variable (refactor name)
       `ug/g wet wt tissue conc` = list(`ug/ml`=paste0("*", conv_factor)),
       `umol/kg dose` = list(`mg/kg BW`= paste0("*", conv_factor, "/1000")), # conv_factor is g/mol, which is the same as mg/mmol or ug/umol
       # Molarity
       `x 10^-3 mol/l` = list(`ug/ml`=paste0("*", conv_factor)), # M * MW
       # Tissue density conversions (Density = g/mL from httk)
       `ug tissue conc` = list(`ug/ml`=paste0("*", conv_factor)),
       `ng/g tissue conc` = list(`ug/ml`=paste0("*", conv_factor, "/1000")),
       `mg/kg tissue conc` = list(`ug/ml`=paste0("*", conv_factor, "/1000000")),
       `ug/kg tissue conc` = list(`ug/ml`=paste0("*", conv_factor, "/1000")),
       `mg/g tissue conc` = list(`ug/ml`=paste0("*", conv_factor, "*1000")),
       `pg/g tissue conc` = list(`ug/ml`=paste0("*", conv_factor, "/1000000")),
       `ug/mg tissue conc` = list(`ug/ml`=paste0("*", conv_factor, "*1000")),
       
       # Special MW * tissue density conv_factor
       `nmol/g tissue conc` = list(`ug/ml`=paste0("*", conv_factor, "/1000")),
       `pmol/g tissue conc` = list(`ug/ml`=paste0("*", conv_factor, "/1000000")),
       `umol/kg tissue conc` = list(`ug/ml`=paste0("*", conv_factor, "/1000")),
       
       # Air conversions
       `ug/ml air conc` = list(`ug/m3`="*1000000"),
       `mg/l air conc` = list(`ug/m3`="*1000000"),
       `ug/l air conc` = list(`ug/m3`="*1000"),
       `umol/m^3 air conc` = list(`ug/m3` = paste0("*", conv_factor)),
       `umol/l air conc` = list(`ug/m3`=paste0("*", conv_factor, "*1000")),
       `nmol/l air conc` = list(`ug/m3`=paste0("*", conv_factor), `ugEq/m3`=paste0("*", conv_factor)),
       `ng/l air conc` = list(`ug/m3`="/1"),
       `ug air conc` = list(`ug/m3`="*NA", `ugEq/m3`="*NA"),
       
       # Dose conversions
       `g/kg dose` = list(`mg/kg BW`="*1000"),
       `mg/kg dose` = list(`mg/kg BW`="/1"),
       `ug/g dose` = list(`mg/kg BW`="/1"),
       `mg/kg-bw dose` = list(`mg/kg BW`="/1"),
       `ng/kg-bw dose` = list(`mg/kg BW`="/1000000"),
       `ug/250g-bw dose` = list(`mg/kg BW`="*4/1000"),
       `ug/kg dose` = list(`mg/kg BW`="/1000"),
       
       # Doses reported as masses without denominators (conv_factor is bw or mw/bw)
       `g need_bw dose` = list(`mg/kg BW` = paste0("/", conv_factor, "*1000")),
       `mg need_bw dose` = list(`mg/kg BW` = paste0("/", conv_factor)),
       `ug need_bw dose` = list(`mg/kg BW` = paste0("/", conv_factor, "/1000")),
       `nmol need_bw dose` = list(`mg/kg BW` = paste0("*", conv_factor, "/1000000")),
       `nmole need_bw dose` = list(`mg/kg BW` = paste0("*", conv_factor, "/1000000")),
       
       # Oral dose with volume conversions
       `ug/ml oral_vol dose` = list(`mg/kg BW`=paste0("*", conv_factor, "/1000")),
       `mg/ml oral_vol dose` = list(`mg/kg BW`=paste0("*", conv_factor)),
       `umol/kg-bw dose` = list(`mg/kg BW`=paste0("*", conv_factor, "/1000")),
       
       # Inhalation dose conversions
       `mg/m^3 dose` = list(`mg/m3`="/1"),
       `mmol/m^3 dose` = list(`mg/m3`=paste0("*", conv_factor)),
       `mg/l dose` = list(`mg/m3`="*1000"),
       `ug/l dose` = list(`mg/m3`="/1"),
       `mg/ml dose` = list(`mg/m3`="*1000000"),
       `ug/ml dose` = list(`mg/m3`="*1000"),
       `ug/m^3 dose` = list(`mg/m3`="/1000"),
       
       # Dermal dose conversions
       # dermal_vol conv_factor is dose volume ml/kg BW, so just convert numerator to mg
       `ug/ml dermal_vol dose` = list(`mg/m2`= paste0("*", conv_factor, "/1000")),
       `mg/ml dermal_vol dose` = list(`mg/m2`= paste0("*", conv_factor)),
       
       # Handle equivalent cases (radiolabeled)
       `ugeq/g tissue conc` = list(`ugEq/ml`=paste0("*", conv_factor)),
       `ngeq/g tissue conc` = list(`ugEq/ml`=paste0("*", conv_factor, "/1000")),

       # TODO Handle ppm and ppb cases
       `ppmm` = list(`ug/ml` = "*NA"),
       `ppbm` = list(`ug/ml` = "*NA"),
       `ppmv air conc` = list(`ug/m3` = "*NA"),
       `ppbv air conc` = list(`ug/m3` = "*NA"),
       
       ## TODO Special ignore cases
       `ng/l / ng/kg tissue conc` = list(`ug/ml` = "*NA"),
       `ug 4-cb/total carcass` = list(`ug/ml` = "*NA")
  ) %>%
    return()
}
