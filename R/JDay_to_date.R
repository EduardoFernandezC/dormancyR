#' Transform JDay to date
#' 
#' In agricultural assessments, the begining and end for a period of interest (e.g. chill accumulation) 
#' are often expressed as Julian days (JDay) or Day Of the Year (DOY). However, this makes difficult the
#' interpretation of the results since most people are used to see dates. This function transform JDays into 
#' dates
#' 
#' @param JDay is a numeric vector of \code{length = 1} or longer containing integers between 1 and 366
#' 
#' @param year is a numeric input to define the year for which the JDay is transformed to date
#' 
#' @param date_format is a character vector representing the format in which the date should be retrieved. For
#' more options see \code{\link[base:as.Date]{as.Date}}
#' 
#' @param na.rm is a boolean parameter used to skip the missing values. Default is set to \code{NULL} to
#' produce an error in case the \code{JDay} vector contains some
#' 
#' @examples
#' 
#' JDay_to_date(JDay = 67 : 69, year = 2020, date_format = "%d.%m.%Y")
#' 
#' JDay_to_date(JDay = c(67, NA, 69), year = 2025, date_format = "%d.%m.%Y",
#'              na.rm = TRUE)
#' 
#' @export JDay_to_date

JDay_to_date <- function(JDay, year, date_format = "%Y-%m-%d", na.rm = FALSE){
  
  # Define the JDay parameter as.integer
  
  JDay <- as.integer(JDay)
  
  # Check for adequate inputs
  
  assertthat::assert_that(all(is.integer(JDay)),
                          msg = "'JDay' parameter is not integer, please provide a valid input")
  
  
  # Check the JDay input in te case the user uses na.rm = TRUE
  
  if (!na.rm)
    assertthat::assert_that(all(JDay %in% 1 : 366),
                            msg = "'JDay' parameter out of range. Please provide a valid output between 1 and 366, or set the 'na.rm' argument to TRUE to skip NAs in case your vector contains some.")
  
  
  # Add the check to the year
  
  if (year > as.numeric(substr(Sys.Date(), 1, 4)))
    
    year_vector <- c(as.numeric(substr(Sys.Date(), 1, 4)), year) else
      
      year_vector <- c(year, as.numeric(substr(Sys.Date(), 1, 4)))
      
  
  
  # Make a primer DF to get the date
  
  primer <- chillR::make_all_day_table(data.frame(Year = year_vector,
                                                  Month = c(1, 12),
                                                  Day = c(1, 31),
                                                  Tmin = as.numeric(NA),
                                                  Tmax = as.numeric(NA)), add.DATE = T, timestep = "day")
  
  # Compute the JDay in the primer DF
  
  primer <- chillR::make_JDay(primer)
  
  
  # Extract the date from the DF according to the JDay and leap_year
  
  dates <- NULL
  
  for (day in JDay){
    
    if (is.na(day)){
      
      date <- NA} else {
        
        date <- as.character(primer[primer$JDay == day & primer$Year == year, "DATE"])
        
        date <- as.Date(substr(date, 1, 10), format = "%Y-%m-%d")
        
        date <- format(date, date_format)}
    
    dates <- c(dates, date)}
  
  
  return(as.Date(dates, format = date_format))
  
}
