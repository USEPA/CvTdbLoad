#' @title normalize_species
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname normalize_species
#' @export 
normalize_species <- function(x){
  message("...normalizing species...")
  #Convert species
  x = tolower(x)
  conv = list(dog=list("dog", "dogs"),
              human=list("human", "humans", "female", "male", "females", "males", "woman", "women", "man", "men"),
              mouse=list("mouse", "mice", "mouses"),
              `monkey`=list("nonhuman primate", "monkey", "primate", "monkies", "monkeys",
                                      "rhesus monkeys (macaca mulatta)", "rhesus monkeys",
                                      "cynomolgus monkeys (macaca fascicularis)"),
              rat=list("rat", "rats"),
              rabbit = list("rabbit", "rabbits"),
              `guinea pig` = list("guinea pig", "guinea pigs"),
              frog = list("frog", "frogs")
  )
  
  x = lapply(x, function(s){
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
