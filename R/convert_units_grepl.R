#' @title convert_units_grepl
#' @description Function to get various grepl statements for unit name standardization.
#' @param unit_type Input unit type (e.g., weight, height, age, dose_duration, conc)
#' @return List of unit name standardizations by input unit type.
#' @rdname convert_units_grepl
#' @export 
convert_units_grepl <- function(unit_type){
  switch(unit_type, 
         "weight" = list(kg = c("kg",  "kilo", "kilogram", "kilograms"),
                         mg = c("mg", "milligram", "miligrams"),
                         g = c("g", "gm", "gram", "grams"),
                         lb = c("lb", "lbs", "pound", "pounds")
         ),
         "height" = list(cm = c("cm", "centimeter", "centimenters"),
                         mm = c("mm", "millimeter", "millimeters"),
                         m = c("m", "meter", "meters"),
                         `in`= c("in", "inch", "inches"),
                         ft= c("ft", "foot", "feet")
         ),
         "age" = list(week = c("wk", "wks", "week", "weeks"),
                      year = c("yr", "yrs", "year", "years", "year old", "years old"),
                      day = c("GD", "day", "days", "gestation"),
                      month = c("month", "months")
         ),
         "dose_duration" = list(day = c("GD", "day", "days"),
                                week = c("wk", "wks", "week", "weeks"),
                                month = c("month", "months"),
                                min = c("min", "mins", "minute", "minutes"),
                                hr = c("h", "hr", "hrs", "hour", "hours"),
                                s = c("s", "sec", "secs", "second", "seconds")
         ),
         "conc" = list(`mmol/l` = c("mmol/L"))
  ) %>%
    return()
}
