#' @title normalize_species
#' @description Normalize species field for input dataframe.
#' @param x Dataframe with species information to normalize.
#' @param log_path File path where to save the log file.
#' @return Modified input `x` dataframe with normalized species information.
#' @rdname normalize_species
#' @export 
normalize_species <- function(x, log_path=NULL){
  message("...normalizing species...")
  # Convert species
  x$species = tolower(x$species)
  conv = list(dog=list("dog", "dogs"),
              human=list("human", "humans", "female", "male", "females", "males", "woman", "women", "man", "men"),
              mouse=list("mouse", "mice", "mouses"),
              `monkey`=list("nonhuman primate", "monkey", "primate", "monkies", "monkeys",
                                      "rhesus monkeys (macaca mulatta)", "rhesus monkeys",
                                      "cynomolgus monkeys (macaca fascicularis)"),
              rat=list("rat", "rats"),
              rabbit = list("rabbit", "rabbits"),
              `guinea pig` = list("guinea pig", "guinea pigs"),
              frog = list("frog", "frogs"),
              hamster = list("hamster", "hamsters")
  )
  
  if(!is.null(log_path)){
    if(any(!x$species %in% unlist(conv))){
      log_CvT_doc_load(f=f, 
                       m="species_not_normalized", 
                       log_path=log_path,
                       val = x$id[!x$species %in% unlist(conv)])
    }  
  }
  
  x$species = lapply(x$species, function(s){
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
