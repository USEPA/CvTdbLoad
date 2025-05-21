#' @description A helper function to convert input values to desired units.
#' @param x Input dataframe to convert
#' @param num Name of column with values to convert
#' @param units Name of column with units to convert from
#' @param desired Desired units to convert the input value into
#' @param conv_factor Conversion factor to use (such as Molecular weight, tissue Density, etc.)
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
convert_units <- function(x, num, units, desired, conv_factor=NA, overwrite_units=FALSE){
  
  # Get conversion factor
  conv = convert_get_conversion_factor(conv_factor)
  
  #Convert units based on input string equation
  if(is.null(conv[[x[[units]]]][[desired]])){
    #No matching desired output
    browser()
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
