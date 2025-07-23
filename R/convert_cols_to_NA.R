#' @title convert_cols_to_NA
#' @description A function to convert input dataframe column list to NA.
#' @param df Input dataframe.
#' @param col_list String of name of column to convert to NA.
#' @return Modified input `df` dataframe with `col_list` as NA.
#' @rdname convert_cols_to_NA
#' @export 
convert_cols_to_NA <- function(df, col_list){
  df[col_list] = NA
  return(df)
}
