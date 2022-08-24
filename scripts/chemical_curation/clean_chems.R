#--------------------------------------------------------------------------------------
#' Clean chemical information (adapted from ToxVal source_chemical.process.R)
#' @param res The input dataframe to which chemical information will be added
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#' @param casrn.col The name of the column containing the CASRN
#' @param name.col The name of the column containing the chemical name
#' @param id.cols A vector of ID column names to generate unique ID
#' @param verbose If TRUE, write out diagnostic messages
#'
#'@import dplyr magrittr digest
#'
#' @return Returns the original dataframe with a chemical_id appended
#' @export
#--------------------------------------------------------------------------------------
clean_chems <- function(res,
                        chem.check.halt=FALSE,
                        casrn.col="casrn",
                        name.col="name",
                        id.cols=NULL,
                        verbose=FALSE) {
  #####################################################################
  cat("Do the chemical checking\n")
  #####################################################################
  res = res %>%
    mutate(chemical_index = paste(!!as.symbol(casrn.col),!!as.symbol(name.col), sep="_"))
  # Clean and flag chemical information
  result = chem.check(res0=res,
                      name.col=name.col,
                      casrn.col=casrn.col,
                      verbose=verbose)
  if(chem.check.halt) if(!result$name.OK || !result$casrn.OK || !result$checksum.OK) browser()

  #####################################################################
  cat("Build the chemical table\n")
  #####################################################################
  
  chems = cbind(res %>% select(all_of(c(id.cols, casrn.col,name.col))),
                result$res0 %>% select(all_of(c(casrn.col,name.col)))) %T>% {
                  names(.) <- c(id.cols, "raw_casrn","raw_name","cleaned_casrn","cleaned_name") 
                } %>%
    distinct() %>%
    tidyr::unite("chemical_id", everything(), sep="_", remove=FALSE) %>%
    dplyr::rowwise() %>%
    mutate(chemical_id = digest::digest(chemical_id,
                                algo="xxhash64", serialize = FALSE))
  
  # check for duplicates
  x = chems$chemical_id
  y=sum(duplicated(x))
  if(y>0) {
    cat(paste0("******************************************************************\n",
        "...some chemical hash keys are duplicated...\n",
        "******************************************************************\n"))
    browser()
  }
  
  return(chems)
}
