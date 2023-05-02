#' @description A helper function to convert input dataframe column list to NA
#' @title FUNCTION_TITLE
#' @param df PARAM_DESCRIPTION
#' @param col_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname convert_cols_to_NA
#' @export 
convert_cols_to_NA <- function(df, col_list){
  df[col_list] = NA
  return(df)
}
