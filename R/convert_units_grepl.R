#' @title convert_units_grepl
#' @description Function to get various grepl statements for unit extraction/conversion
#' @param unit_type PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname convert_units_grepl
#' @export 
convert_units_grepl <- function(unit_type){
  switch(unit_type, 
         "weight" = list(kg = "\\bkg\\b|kilogram|kilo",
                         mg = "\\bmg\\b|milligram",
                         g = "\\bg\\b|gram|grams",
                         lb = "\\blb\\b|pound",
                         rm_list = c("kilogram", "kilograms", "kg", 
                                     "milligram", "milligrams", "mg", 
                                     "gram", "grams", "g",
                                     "pound", "pounds", "lb", "lbs")
                         ),
         "height" = list(cm = "\\bcm\\b|centimeter",
                         mm = "millimeter|mm",
                         m = "\\bm\\b|meter",
                         `in`="\\bin\\b|inch",
                         ft="\\bft\\b|foot|feet",
                         rm_list = c("cm", "centimeter", "centimeters", 
                                     "mm", "millimeter", "millimeters", 
                                     "m", "meter", "meters",
                                     "in", "inch", "inches",
                                     "ft", "foot", "feet")
                         ),
         "age" = list(week = "week|weeks|\\bwk\\b|\\bwks\\b",
                      year = "years old|year|years|\\byr\\b|\\byrs\\b",
                      day = "day|days|GD|gestation",
                      month = "month|months",
                      rm_list = c("week", "weeks","wk","wks",
                                  "month",
                                  "years","year","-year","yr","yrs",
                                  "day","days","GD","gestational day","gestational days")),
         "dose_duration" = list(day = "day|days|\\bGD\\b",
                                week = "week|weeks|\\bwk\\b|\\bwks\\b",
                                month = "month|months",
                                min = "\\bmin\\b|\\bmins\\b",
                                hr = "hour|hours|\\bhr\\b|\\bhrs\\b|\\bh\\b",
                                s = "\\bsec\\b|\\bs\\b|second|seconds",
                                rm_list= c("within", "day",
                                           "week", "wk", "wks",
                                           "month",
                                           "min", "mins",
                                           "hour", "hr", "hrs", "h",
                                           "sec", "s"))#,
         # # Default case
         # list(
         #   kg = "kg|kilogram|kilo",
         #   mg = "mg|milligram",
         #   g = "g|gram",
         #   lb = "lb|pound",
         #   cm = "cm|centimeter",
         #   mm = "millimeter|mm",
         #   m = "m|meter",
         #   `in`="in|inch",
         #   ft="ft|foot|feet",
         #   week = "week|weeks|wk|wks",
         #   year = "years old|year|years|yr|yrs",
         #   day = "day|days|GD|gestation",
         #   month = "month|months",
         #   s = "sec|s|second|seconds",
         #   rm_list = c("kilogram", "kilograms", "kg", "milligram", "milligrams", "mg", "gram", "grams",           
         #               "g", "pound", "pounds", "lb", "lbs", "cm", "centimeter", "centimeters",
         #               "mm", "millimeter", "millimeters", "m", "meter", "meters", "in", "inch", 
         #               "inches", "ft", "foot", "feet", "week", "weeks", "wk", "wks", 
         #               "month", "years", "year", "-year", "yr", "yrs", "day", "days", 
         #               "GD", "gestational day", "gestational days", "within", "min", "mins", "hour", "hr", 
         #               "hrs", "h", "sec", "s")
         # )
         ) %>%
    return()
}
