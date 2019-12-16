#' Get weather data from Tinytag sensor in chillR format
#' 
#' This function returns weather data (temperature and humidity) from tinytag sensors (model TGP-4500). It also
#' contains options to check for missing observations and fill them through linear interpolation.
#' 
#' @param path_data Character string. This is the directory in which the data is stored. It is important to note
#' that the file must be in ".csv" (using sep = ",") or ".xlsx" format
#' 
#' @param vars Character string. Climate related variables that will be returned by the function. It has two
#' options for temperature according to the time_step parameter. Tmean, Tmin and Tmax only work for daily
#' observations. Temp works for hourly observations. The Humidity is also possible to obtain. Default is set to
#' "Temp" and "Humidity"
#' 
#' @param time_step Character string. This is the time step in which the data must be obtained. Default is set to
#' "hourly"
#' 
#' @param check_data Boolean parameter to decide whether the data should be quality checked or not. If so, it uses
#' functions contained in chillR to fill the gaps. Default is TRUE
#' 
#' @param latitude Numeric vector. If the check_data parameter is used, this is to fill the gaps by linear
#' interpolation according to the latitude of the place. Default is NULL
#' 
#' @param sep Sometimes the file is saved in ".csv" format but the character for separating the rows is not ","
#' (comma). With sep, this can be set to a different separator character only if the user knows it
#'  
#' @details 
#' This function has an option to fill the gaps through linear interpolation. However, this step is ONLY
#' recommended if the sensor was placed in normal environmental conditions. Otherwise, the values used to fill
#' the gaps are not representatives for the conditions.
#' 
#' @examples 
#' 
#' # As each user has different path for the folder this example is not running until "#"
#' # is removed
#' 
#' # path <- "C:/Users/...../...."
#' 
#' # handle_tinytag(path_data = path, vars = c("Tmin", "Tmean", "Tmax"),
#' # time_step = "daily", check_data = T, latitude = 50.62, sep = NULL)
#' 
#' @export handle_tinytag
#' @importFrom dplyr "%>%"

handle_tinytag <- function(path_data, vars = c("Temp", "Humidity"), time_step = "hourly", check_data = T,
                           latitude = NULL, sep = NULL){
  
  requireNamespace("dplyr")
  
  # Check some previos conditions for avoid errors
  
  if (vars %in% c("Temp", "Humidity") & time_step == "daily" || time_step == "hourly" & vars %in% c("Tmean",
                                                                                                    "Tmin",
                                                                                                    "Tmax"))
    stop("Variable(s) not included for the time step specified. Please change either the vars or time_step
  parameter into a valid option")
  
  
  if (any(!(vars %in% c("Temp", "Tmean", "Tmax", "Tmin", "Humidity"))))
    stop("Variables not supported by the function. Please specified any valid option")
  
  
  if (!(time_step %in% c("hourly", "daily")))
    stop("Time step not supported by the function. Please specified any valid option")
  
  if (check_data & is.null(latitude))
    stop("Please provide a valid latitude for filling the gaps. Otherwise set check_data to FALSE")
  
  
  
  # Load the data from the "Tynitag" datalogger
  
  
  # Check the extension of the file
  
  if (tools::file_ext(path_data) %in% c("xls", "xlsx")){
    
    weather <- suppressWarnings(suppressMessages(readxl::read_xlsx(path_data, sheet = 1, skip = 4,
                                                                   col_types = c("numeric", "date",
                                                                                 "numeric", "numeric"))))
    
    colnames(weather) <- c("Eingenschaft", "Date", "Temp", "Humidity")
    
    weather <- weather[c("Date", "Temp", "Humidity")]}
  
  
  
  if (tools::file_ext(path_data) %in% c("csv")){
    
    if (!is.null(sep)){
      
      weather <- utils::read.csv(path_data, skip = 4, sep = sep,
                                 col.names = c("Eingenschaft", "Date", "Temperature",
                                               "Humidity"))[, c("Date", "Temperature", "Humidity")]}
    
    else {
      
      weather <- utils::read.csv(path_data, skip = 4,
                                 col.names = c("Eingenschaft", "Date", "Temperature",
                                               "Humidity"))[, c("Date", "Temperature", "Humidity")]}
    
    
    # Remove the C and RH characters from each row
    
    weather <- tidyr::separate(weather, Temperature, c("Temp", "Unit"), sep = " ",
                               convert = TRUE)
    
    weather <- tidyr::separate(weather, Humidity, c("Humidity", "Unit"), sep = " ",
                               convert = TRUE)[c("Date", "Temp", "Humidity")]}
  
  
  # Separate the hour from the date
  
  weather <- tidyr::separate(weather, Date, c("Date", "Hour"), sep = " ", convert = TRUE)
  
  # Separate the hour and minute from the hour column
  
  weather <- suppressWarnings(tidyr::separate(weather, "Hour", c("Hour", "Min"), convert = TRUE))
  
  
  # Add the Year, Month, Day, Hour, Min and condition columns
  
  weather <- tidyr::separate(weather, "Date", c("Year", "Month", "Day"), convert = TRUE)
  
  if (!(length(unique(weather$Year)) < length(unique(weather$Month)) & 
        length(unique(weather$Month)) < length(unique(weather$Day)))){
    
    colnames(weather) <- c("Day", "Month", "Year", "Hour", "Min", "Temp", "Humidity")
    
    weather <- weather[c("Year", "Month", "Day", colnames(weather)[4 : length(colnames(weather))])]}
  
  
  if (nchar(as.character(weather[1, "Year"])) == 2){
    
    weather$Year <- weather$Year + 2000}
  
  # Summarise the data by hour
  
  weather <- data.frame(weather %>% dplyr::group_by(Year, Month, Day, Hour) %>% 
                          dplyr::summarise(Temp = mean(Temp, na.rm = T), Humidity = mean(Humidity, na.rm = T)))
  
  
  
  # Check for missing observations
  
  if (check_data)
    missings <- chillR::make_all_day_table(dplyr::bind_rows(weather[1, ], weather[length(weather$Year), ]),
                                           timestep = "hour", add.DATE = F) else
                                             
                                             missings <- weather
  
  
  # Interpolate gaps
  
  if (check_data == T & length(missings$Year) != length(weather$Year))
    weather <- chillR::interpolate_gaps_hourly(chillR::make_all_day_table(weather, timestep = "hour",
                                                                          add.DATE = F),
                                               latitude = latitude)[[1]] else
                                                 
                                                 weather <- weather
  
  # Summarise by day
  
  if (time_step == "daily")
    weather <- data.frame(weather %>% dplyr::group_by(Year, Month, Day) %>%
                            dplyr::summarise(Tmin = min(Temp, na.rm = T),
                                             Tmean = mean(Temp, na.rm = T),
                                             Tmax = max(Temp, na.rm = T),
                                             Humidity = mean(Humidity, na.rm = T)))
  
  
  # Add the column YEARMODAHO
  
  if (time_step == "hourly")
    weather["YEARMODAHO"] <- weather$Year * 1000000 + weather$Month * 10000 + weather$Day * 100 + weather$Hour else
      weather["YEARMODA"] <- weather$Year * 10000 + weather$Month * 100 + weather$Day
  
  
  # Return the data
  
  if (time_step == "hourly") return(weather[c("YEARMODAHO", "Year", "Month", "Day", "Hour", vars)]) else
    return(weather[c("YEARMODA", "Year", "Month", "Day", vars)])}