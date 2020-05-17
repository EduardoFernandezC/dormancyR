#' Transform JDay to date
#' 
#' In agricultural assessments, the begining and end for a period of interest (e.g. chill accumulation) 
#' are often expressed as Julian days (JDay) or Day Of the Year (DOY). However, this makes difficult the
#' interpretation of the results since most people are used to see dates. This function transform JDays into 
#' dates
#' 
#' @param JDay Numeric vector. It can be a vector of \code{length = 1} containing integers between 1 and 366
#' 
#' @param year Numeric input. This is to define the year for which the JDay is transformed to date
#' 
#' @param date_format Character vector representing the format in which the date should be retrieved. For
#' more options see \code{\link[base:as.Date]{as.Date}}
#' 
#' @examples
#' \dontrun{
#' JDay_to_date(JDay = 67 : 69, year = 2020, date_format = "%d.%m.%Y")
#' }
#' @export JDay_to_date

JDay_to_date <- function(JDay, year, date_format = "%Y-%m-%d"){
  
  # Define the JDay parameter as.integer
  
  JDay <- as.integer(JDay)
  
  # Check for adequate inputs
  
  assertthat::assert_that(all(is.integer(JDay)), msg = "'JDay' parameter is not integer, please provide a valid input")
  assertthat::assert_that(all(JDay %in% 1 : 366), msg = "'JDay' parameter out of range, plase provide a value between 1 and 366")
  
  # Make a primer DF to get the date
  
  primer <- chillR::make_all_day_table(data.frame(Year = c(year, as.numeric(substr(Sys.Date(), 1, 4))),
                                                  Month = c(1, 12),
                                                  Day = c(1, 31),
                                                  Tmin = as.numeric(NA),
                                                  Tmax = as.numeric(NA)), add.DATE = T, timestep = "day")
  
  # Compute the JDay in the primer DF
  
  primer <- chillR::make_JDay(primer)
  
  
  # Extract the date from the DF according to the JDay and leap_year
  
  dates <- NULL
  
  for (day in JDay){
    
    date <- as.character(primer[primer$JDay == day & primer$Year == year, "DATE"])
    
    dates <- c(dates, date)}
  
  dates <- as.Date(substr(dates, 1, 10), format = "%Y-%m-%d")
  
  return(format(dates, date_format))
  
}