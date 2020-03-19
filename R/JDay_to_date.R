#' Transform JDay to date
#' 
#' In agricultural assessments, the begining and end for a period of interest (e.g. chill accumulation) 
#' are often expressed as Julian days (JDay) or Day Of the Year (DOY). However, this makes difficult the
#' interpretation of the results since most people are used to see dates. This function transform JDays into 
#' dates
#' 
#' @param JDay Numeric vector. It can be a vector of \code{length = 1} containing integers between 1 and 366
#' 
#' @param leap_year Boolean parameter. This is to define if the date must be provided for a leap year
#' 
#' @examples
#' JDay_to_date(JDay = 67:69, leap_year = FALSE)
#' 
#' @export JDay_to_date

JDay_to_date <- function(JDay, leap_year = FALSE){
  
  # Check for adequate inputs
  
  assertthat::assert_that(all(is.integer(JDay)), msg = "'JDay' parameter is not integer, please provide a valid input")
  assertthat::assert_that(all(JDay %in% 1 : 366), msg = "'JDay' parameter out of range, plase provide a value between 1 and 366")
  
  if (any(JDay == 366) & !leap_year)
    
    stop(paste("'JDay(s)' not valid for 'leap_year' =", leap_year))
  
  
  # Define the year in case of leap years
  
  if (leap_year) year <- 2020 else year <- 2019
  
  
  # Make a primer DF to get the date
  
  primer <- chillR::make_all_day_table(data.frame(Year = c(2019, 2020),
                                                  Month = c(1, 12),
                                                  Day = c(1, 31),
                                                  Tmin = as.numeric(NA),
                                                  Tmax = as.numeric(NA)), add.DATE = T, timestep = "day")
  
  # Compute the JDay in the primer DF
  
  primer <- chillR::make_JDay(primer)
  
  
  # Extract the date from the DF according to the JDay and leap_year
  
  date <- as.character(primer[primer$JDay %in% JDay & primer$Year == year, "DATE"])
  
  return(paste(month.name[as.numeric(substr(date, 6, 7))], substr(date, 9, 10)))
  
}