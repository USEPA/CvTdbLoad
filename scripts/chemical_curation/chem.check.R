#--------------------------------------------------------------------------------------
#' Check the chemicals from a file - Adapted from ToxVal chem.check.R
#' Names with special characters are cleaned and trimmed
#' CASRN are fixed (dashes put in, trimmed) and check sums are calculated
#' The output is sent to a file called chemcheck.xlsx in the source data file
#' One option for using this is to edit the source file until no errors are found
#'
#' @param res0  The data frame in which chemicals names and CASRN will be replaced
#' @param name.col The column name that contains the chemical names
#' @param casrn.col The column name that contains the CARN values
#' @param verbose If TRUE, print diagnostic messages
#' @return Return a list with fixed CASRN and name and flags indicating if fixes were made:
#' res0=res0,name.OK=name.OK,casrn.OK=casrn.OK,checksum.OK=checksum.OK
#'
#--------------------------------------------------------------------------------------
chem.check <- function(res0,
                       name.col="name",
                       casrn.col="casrn",
                       verbose=FALSE) {
  # Set default flags and empty log
  ccheck = NULL; name.OK = TRUE; casrn.OK = TRUE; checksum.OK = TRUE
################################################################################
  cat(">>>Deal with name\n")
  # Make original vs. encoded vs. clean placeholders
  res0 = res0 %>%
    mutate(n0 = !!sym(name.col),
           # Handle encoding
           n1 = iconv(n0,from="UTF-8",to="ASCII//TRANSLIT"),
           n2 = stringi::stri_escape_unicode(n1) %>%
             # Handle backslash
             stringr::str_replace_all(.,"\\\\'","\'") %>%
             # Handle line breaks
             stringr::str_replace_all(.,"[\r\n]"," ") %>%
             # Trim and remove extra whitespace
             stringr::str_squish(.)
             )
  # Remove anything after the list of symbols/scenarios
  remove_list = c(";", " \\(")
  match = paste0(remove_list, collapse="|")
  subt = paste0(remove_list, ".*", collapse="|")
  # Only change those that contain the remove_list scenarios
  res0$n2[grepl(match, res0$n2)] = res0$n2[grepl(match, res0$n2)] %>%
    sub(subt, '', .)
  # Remove symbols/scenarios from end of string
  remove_list = c(";","/","\\.") %>%
    paste0(., "$", collapse="|")
  res0$n2 = res0$n2 %>%
    sub(remove_list, '', .) %>%
    stringr::str_squish()
  # Log any name changes for checking
  res0 = res0 %>%
    # Case where orig and cleaned don't match OR orig NOT NA but cleaned IS NA OR
    # orig IS NA but cleaned is NOT NA
    mutate(name_diff = ((n2 != n0) | 
                          (!is.na(n2) & is.na(n0)) |
                          (is.na(n2) & !is.na(n0)))
           )
  # Append to log if differences found
  ccheck = ccheck %>%
    rbind(res0 %>% 
            # There is a difference between original and cleaned
            filter(name_diff == TRUE) %>%
            select(original=n0, escaped=n1, cleaned=n2) %>%
            mutate(checksum = NA))
  # Flag difference found
  if(any(res0$n2 != res0$n0)) name.OK = FALSE
  # Replace original with cleaned string
  res0[[name.col]] = res0$n2
################################################################################
  cat("\n>>> Deal with CASRN\n")
  # TO DO: Vectorize with tiydr
  # res0 = res0 %>%
  #   mutate(n0 = !!sym(casrn.col),
  #          n1 = iconv(n0,from="UTF-8",to="ASCII//TRANSLIT"),
  #          n2 = stringi::stri_escape_unicode(n1) %>%
  #            fix.casrn(),
  #          cs = cas_checkSum(n2)
  #   )
  
  for(i in 1:nrow(res0)) {
    n0 = res0[[i,casrn.col]]
    if(is.na(n0)) next # Skip NA CAS
    
    n1 = iconv(n0,from="UTF-8",to="ASCII//TRANSLIT")
    n2 = stringi::stri_escape_unicode(n1)
    n2 = fix.casrn(n2)
    cs = cas_checkSum(n2)
    if(is.na(cs)) cs = 0
    if(verbose) cat("2>>> Orig: ",n0, " | encoded: ", n1, " | cleaned: ", n2, " | checksum: ", cs,"\n")
    if(n2!=n0) {
      res0[i,casrn.col] = n2
      row[1,1] = n0
      row[1,2] = n1
      row[1,3] = n2
      row[1,4] = cs
      ccheck = rbind(ccheck,row)
      casrn.OK = FALSE
    }
    if(!cs) {
      row[1,1] = n0
      row[1,2] = n1
      row[1,3] = NA
      row[1,4] = cs
      ccheck = rbind(ccheck,row)
      checksum.OK = FALSE
      cat("bad checksum:",n0,n1,"\n")
    }
    if(i%%status_bar==0) cat(" chemcheck casrn: finished ",i," out of ",nrow(res0),"\n")
  }
  ccheck = unique(ccheck)
  if(!dir.exists("input/chemcheck")) dir.create("input/chemcheck")
  
  if(!is.null(ccheck)){
    if(nrow(ccheck)){
      # Need to come up with file naming convention to better log which 
      # dataset it came from
      writexl::write_xlsx(ccheck, paste0("input/chemcheck/chemchecklog_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
    } 
  }

  if(!name.OK) cat("Some names fixed\n")
  else cat("All names OK\n")
  if(!casrn.OK) cat("Some casrn fixed\n")
  else cat("All casrn OK\n")
  if(!checksum.OK) cat("Some casrn have bad checksums\n")
  else cat("All checksums OK\n")
  # Return cleaned chemical information with flags
  return(list(res0=res0,name.OK=name.OK,casrn.OK=casrn.OK,checksum.OK=checksum.OK))
}
