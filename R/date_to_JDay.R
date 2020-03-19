#' Transform dates into JDay
#' 
#' In agricultural assessments, the begining and end for a period of interest (e.g. chill accumulation) 
#' are often expressed as Julian days (JDay) or Day Of the Year (DOY). This function transform dates into 
#' JDays without the need of looking at external resources for the information
#' 
#' @param date Character string or date class elements. This is the date for which the function must return the JDay. The function also
#' accepts a vector of \code{length > 1}
#' 
#' @param format Character string. This is the format in which the date(s) are introduced when calling the
#' function. See \code{\link[base:as.Date]{base::as.Date}}
#' 
#' @examples 
#' 
#' date_to_JDay(date = c("15/10/2020", "15/10/2019", "01/08/2003"), 
#'              format = "%d/%m/%Y")
#' 
#' @export date_to_JDay

date_to_JDay <- function(date, format){
  
  # Check for adequate inputs
  
  assertthat::assert_that(all(is.character(date) | lubridate::is.Date(date)),
                          msg = "'date' parameter is not character of date class, please provide a valid input")
  assertthat::assert_that(is.character(format), msg = "'format' parameter is not character, plase provide a valid input")
  
  
  # Make a primer DF to get the date
  
  primer <- chillR::make_all_day_table(data.frame(Year = c(2019, 2020),
                                                  Month = c(1, 12),
                                                  Day = c(1, 31),
                                                  Tmin = as.numeric(NA),
                                                  Tmax = as.numeric(NA)), add.DATE = T, timestep = "day")
  
  # Compute the JDay in the primer DF
  
  primer <- chillR::make_JDay(primer)
  
  # Formatting the date
  
  formated_date <- as.Date(date, format = format)
  
  # Extract the day, month, and year from the date privided
  
  months <- as.numeric(substr(formated_date, 6, 7))
  
  days <- as.numeric(substr(formated_date, 9, 10))
  
  years <- as.numeric(substr(formated_date, 1, 4))
  
  
  # Extract the JDay from the DF according to the date and leap_year
  
  JDays <- NULL
  
  for (i in 1 : length(date)){
    
    # Define the year in case of leap years
    
    if (chillR::leap_year(years[i])) year <- 2020 else year <- 2019
    
    JDay <- primer[primer$Month %in% months[i] & primer$Day %in% days[i] & primer$Year == year, "JDay"]
    
    JDays <- c(JDays, JDay)}
  
  return(JDays)
  
}

