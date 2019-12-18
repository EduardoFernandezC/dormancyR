#' Get humidity and concentration of CO2 from the heated greenhouse at CKA
#' 
#' This function returns information about relative humidity and concentration of CO2 inside the heated
#' greenhouse located at Campus Klein Altendorf of the University of Bonn. It has two options for time step
#' (hourly and daily) basis.
#' 
#' @param path_data Character string. This is the directory in which the file has been stored. It should include
#' the complete name of the file in ".xls" format.
#' 
#' @param time_step Character string. Time step in which the data must be returned. It has two options for the
#' frequency of records. "hourly" (the default) and "daily"
#' 
#' @examples
#' 
#' # As each user has different path for the folder this example is not running until "#"
#' # is removed
#' 
#' # path <- "C:/Users/...../...."
#' 
#' # handle_gh_hum_co2(path_data = path, time_step = "daily")
#' 
#' @export handle_gh_hum_co2
#' @importFrom dplyr "%>%"

handle_gh_hum_co2 <- function(path_data, time_step = "hourly") {
  
  
  # Read the data in xls format
  
  weather <- readxl::read_xls(path_data, sheet = 1)
  
  
  # Change the name of the columns by identifying a pattern for each variable
  
  if (length(grep("oC", colnames(weather))) > 0)
    
    colnames(weather)[grep("oC", colnames(weather))[[1]]] <- "Temp"
  
  
  if (length(grep("[%rf]", colnames(weather))) > 0)
    
    colnames(weather)[grep("[%rf]", colnames(weather))[[1]]] <- "Humidity"
  
  
  if (length(grep("ppm", colnames(weather))) > 0)
    
    colnames(weather)[grep("ppm", colnames(weather))[[1]]] <- "CO2"
  
  
  # Separate the hour from the date
  
  weather <- tidyr::separate(weather, Zeit, c("Date", "Hour"), sep = " ", convert = TRUE)
  
  
  # Separate the hour and minute from the hour column
  
  weather <- tidyr::separate(weather, Hour, c("Hour", "Min"), convert = TRUE)
  
  
  # Separate year, month and day
  
  weather <- tidyr::separate(weather, Date, c("Day", "Month", "Year"), convert = TRUE)
  
  weather["Year"] <- weather$Year + 2000
  
  
  # Summarise by hour 
  
  weather <- data.frame(weather %>% dplyr::group_by(Year, Month, Day, Hour) %>%
                          dplyr::summarise(Temp = mean(Temp, na.rm = T),
                                           Humidity = mean(Humidity, na.rm = T),
                                           CO2 = mean(CO2, na.rm = T)))
  
  
  # Check if the dataframe has the first and last row
  
  if (is.na(weather[length(weather$Year), "Year"]))
    weather <- weather[-length(weather$Year), ]
  
  if (is.na(weather[1, "Year"]))
    weather <- weather[-1, ]
  
  
  # Make all day table to return a complete dataframe for the whole period
  
  weather <- chillR::make_all_day_table(weather, timestep = "hour")
  
  # Add YEARMODAHO
  
  weather["YEARMODAHO"] <- weather$Year * 1000000 + weather$Month * 10000 + weather$Day * 100 +  weather$Hour
  
  # Remove NAN and Inf values and change to NA
  
  weather[which(is.na(weather$Humidity) | !is.finite(weather$Humidity)), "Humidity"] <- NA
  
  weather[which(is.na(weather$CO2)| !is.finite(weather$Humidity)), "CO2"] <- NA
  
  
  # Return the dataframe in hourly time_step
  
  if (time_step == "hourly")
    
    return(weather[c("YEARMODAHO", "Year", "Month", "Day", "Hour", "Humidity", "CO2")]) else
      
      if (time_step == "daily")
        
        weather <- suppressWarnings(data.frame(weather %>% dplyr::group_by(Year, Month, Day) %>%
                                                 dplyr::summarise(Humidity = mean(Humidity, na.rm = T),
                                                                  CO2 = mean(CO2, na.rm = T))))
  
  # Remove NAN and Inf values and change to NA
  
  if (time_step == "daily") {
    
    weather[which(!is.finite(weather$Humidity) | is.na(weather$Humidity)), "Humidity"] <- NA
    
    weather[which(!is.finite(weather$CO2) | is.na(weather$CO2)), "CO2"] <- NA
    
    # Add YEARMODA
    
    weather["YEARMODA"] <- weather$Year * 10000 + weather$Month * 100 + weather$Day}
  
  return(weather[c("YEARMODA", "Year", "Month", "Day", "Humidity", "CO2")])}
