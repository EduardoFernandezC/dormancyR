#' Get weather data from the greenhouses at CKA in chillR format
#' 
#' This function is to handle weather data obtained from the greenhouses located in Campus Klein Altendorf.
#' For now, this only works for temperatures. Input files must be stored as ".xls" format.
#' 
#' @param path_data Character string. This is the directory in which the data is stored. It is important to note
#' that the file must be a ".xls"
#' 
#' @param time_step Character string. This is the time step in which the data must be obtained. It has two options;
#' "hourly" (the default) and "daily"
#' 
#' @note 
#' In the "daily" mode, this function returns the variables Tmin, Tmean and Tmax. In contrast, under the "hourly"
#' mode, it returns the mean temperature for each hour as "Temp".
#' 
#' @examples 
#' # As each user has different path for the folder this example is not running until "#"
#' # is removed
#' 
#' # path <- "C:/Users/...../...."
#' 
#' # handle_GH_data(path_data = path, time_step = "daily")
#' 
#' @export handle_GH_data
#' @importFrom dplyr "%>%"


handle_GH_data <- function(path_data, time_step = "hourly") {
  
  
  # Read the data in xls format
  
  weather <- readxl::read_xls(path_data, sheet = 1)
  
  
  # Change the name of the columns by identifying a pattern for each variable
  
  if (length(grep("oC", colnames(weather))) > 0)
    
    colnames(weather)[grep("oC", colnames(weather))[[1]]] <- "Temp"
  
  
  # Separate the hour from the date
  
  weather <- tidyr::separate(weather, Zeit, c("Date", "Hour"), sep = " ", convert = TRUE)
  
  
  # Separate the hour and minute from the hour column
  
  weather <- tidyr::separate(weather, Hour, c("Hour", "Min"), convert = TRUE)
  
  
  # Separate year, month and day
  
  weather <- tidyr::separate(weather, Date, c("Day", "Month", "Year"), convert = TRUE)
  
  weather["Year"] <- weather$Year + 2000
  
  
  # Summarise by hour 
  
  weather <- data.frame(weather %>% dplyr::group_by(Year, Month, Day, Hour) %>%
                          dplyr::summarise(Temp = mean(Temp, na.rm = T)))
  
  
  # Check if the dataframe has the first and last row
  
  if (is.na(weather[length(weather$Year), "Year"]))
    weather <- weather[-length(weather$Year), ]
  
  if (is.na(weather[1, "Year"]))
    weather <- weather[-1, ]
  
  
  # Make all day table to return a complete dataframe for the whole period
  
  weather <- chillR::make_all_day_table(weather, timestep = "hour")
  
  # Add YEARMODAHO
  
  weather["YEARMODAHO"] <- weather$Year * 1000000 + weather$Month * 10000 + weather$Day * 100 +  weather$Hour
  
  weather[which(is.na(weather$Temp)), "Temp"] <- NA
  
  if (time_step == "hourly")
    
    return(weather[c("YEARMODAHO", "Year", "Month", "Day", "Hour", "Temp")]) else
      
      if (time_step == "daily")
        
        weather <- suppressWarnings(data.frame(weather %>% dplyr::group_by(Year, Month, Day) %>%
                                                 dplyr::summarise(Tmin = min(Temp, na.rm = T),
                                                                  Tmean = mean(Temp, na.rm = T),
                                                                  Tmax = mean(Temp, na.rm = T))))
  
  
  if (time_step == "daily") {
    
    weather[which(!is.finite(weather$Tmin) | is.na(weather$Tmin)), "Tmin"] <- NA
    
    weather[which(!is.finite(weather$Tmean) | is.na(weather$Tmean)), "Tmean"] <- NA
    
    weather[which(!is.finite(weather$Tmax) | is.na(weather$Tmax)), "Tmax"] <- NA
    
    weather["YEARMODA"] <- weather$Year * 10000 + weather$Month * 100 + weather$Day}
  
  return(weather[c("YEARMODA", "Year", "Month", "Day", "Tmin", "Tmean", "Tmax")])}