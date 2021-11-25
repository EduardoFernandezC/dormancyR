#' Get weather data from the greenhouses at CKA in \code{\link[chillR:chillR-package]{chillR}} format
#' 
#' This function is to handle weather data obtained from the greenhouses located in Campus Klein Altendorf.
#' For now, this works for input files stored as \strong{'.xls'} or \strong{'.xlsx'} format
#' 
#' @param path_data Character string. This is the directory in which the data is stored. It is important to note
#' that the file must have either \strong{'.xls'} or \strong{'.xlsx'} extension
#' 
#' @param time_step Character string. This is the time step in which the data must be retrieved. It has two options;
#' \emph{'hourly'} (the default) and \emph{'daily'}
#' 
#' @param vars Character string. Variables that must be retrieved by the function. In general, the options are
#' \emph{'Temperatures'}, \emph{'Humidity'}, and \emph{'CO2 concentration'}. Regarding \emph{'Temperatures'}, 
#' the options differed depending on the \code{time_step} parameter. For \code{time_step = 'daily'}, the options
#' are \emph{'Tmean'}, \emph{'Tmin'}, and \emph{'Tmax'}
#' 
#' @param check_data Boolean parameter to check the observations for inconsistencies. Unfortunately,
#' the dataloggers in the greenhouses often define missing records as 0. This makes exremely complicated 
#' to know if one record was actually 0 or missing. This check for that inconsistency by comparing the
#' temperature at a given hour 'x' with the mean for the previous hours (defined by the \code{n_hours}).
#' If this difference is greater than a given threshold (i.e. \code{threshold}) the record is corrected to
#' \code{NA}
#' 
#' @param n_hours Numeric input. This is the number of previous hours used to compute the mean and to check for
#' inconsistencies. Default is set to 24 hours
#' 
#' @param threshold Numeric input. This is the limit to define whether the record was a true observation or not.
#' Default is set to 10 celsius degree since greenhouse temperatures are controlled
#' 
#' @param fix_data Boolean parameter. If \code{fix_data = TRUE}, fill in the missing hours
#' by the function \code{\link[chillR:interpolate_gaps_hourly]{interpolate_gaps_hourly}} from 
#' \code{\link[chillR:chillR-package]{chillR}}
#' 
#' @param na_strings Character vector of strings to interpret as missing values. By default,
#' \code{handle_greenhouse} treats the word \emph{'FAULT'} and blank cells as missing data.
#' 
#' @note 
#' In the \emph{'daily'} mode, this function can returns the variables \emph{'Tmin'}, \emph{'Tmean'} and \emph{'Tmax'}. In contrast, under the \emph{'hourly'}
#' mode, it can returns the mean temperature for each hour as \emph{'Temp'}
#' 
#' @examples 
#' # As each user has different path for the folder this example is not running until "#"
#' # is removed
#' 
#' # path <- "C:/Users/...../...."
#' 
#' # handle_greenhouse(path_data = path, time_step = "daily", vars = c("Tmin", "Tmax"),
#' #                   check_data = T, n_hours = 12, threshold = 5, fix_data = T)
#' 
#' @export handle_greenhouse
#' @importFrom dplyr "%>%"

handle_greenhouse <- function (path_data, time_step = "hourly", vars = c("Temp", "Humidity", "CO2"),
                               check_data = T, n_hours = 24, threshold = 10, fix_data = T,
                               na_strings = c("FAULT", "")){
  
  # Check if the file exist
  
  if (!file.exists(path_data))
    
    stop("File does not exist. Please provide a valid path.")
  
  # Check if the path_data input is character string
  
  if (!is.character(path_data))
    
    stop("'path_data' argument is nor a character string.")
  
  # Check if time_step is in hourly or daily options
  
  if (!(time_step %in% c("hourly", "daily")))
    
    stop("'time_step' is not valid. Please provide a valid option.")
  
  if (any(vars == "Temp") & time_step == "daily" | time_step == "hourly" &
      any(vars %in% c("Tmean", "Tmin", "Tmax")))
    
    stop("Variable(s) not included for the 'time_step' specified. Please change either the 'vars' or 'time_step'\n  parameter into a valid option.")
  
  
  # Check the extension of the file. For now, this works for sure with 'xls' and 'xlsx' extensions...
  # 'csv' files may not work.
  
  if (tools::file_ext(path_data) %in% c("xlsx", "xls"))
    
    weather <- suppressWarnings(suppressMessages(readxl::read_excel(path_data, sheet = 1, na = na_strings)))
  
  if (tools::file_ext(path_data) == "csv")
    
    weather <- utils::read.csv(path_data, na.strings = na_strings)
  
  
  # Identify the columns for temperature vars by matching the symbol for celsius degree. Change the name of the
  # columns
  
  colnames(weather)[grep("oC", colnames(weather))][[1]] <- "Temp"
  
  # The same as above for humidity and CO2 concentration
  
  colnames(weather)[grep("%rF", colnames(weather))][[1]] <- "Humidity"
  
  colnames(weather)[grep("ppm", colnames(weather))][[1]] <- "CO2"
  
  
  # Select only the relevan columns
  
  weather <- weather[c("Zeit", "Temp", "Humidity", "CO2")]
  
  
  # Separate the columns for date and time
  
  weather <- tidyr::separate(weather, Zeit, c("Date", "Hour"), sep = " ", convert = TRUE)
  
  weather <- tidyr::separate(weather, Hour, c("Hour", "Min"), convert = TRUE)
  
  weather <- tidyr::separate(weather, Date, c("Day", "Month", "Year"), convert = TRUE)
  
  
  # Add the year column
  
  weather["Year"] <- weather$Year + 2000
  
  
  # Force vars columns as.numeric
  
  for (colname in colnames(weather))
    
    weather[colname] <- as.numeric(weather[[colname]])
  
  
  # Group the data by hour
  
  weather <- data.frame(weather %>% 
                          dplyr::group_by(Year, Month, Day, Hour) %>% 
                          dplyr::summarise(Temp = mean(Temp, na.rm = T),
                                           Humidity = mean(Humidity, na.rm = T),
                                           CO2 = mean(CO2, na.rm = T)))
  
  # Change NaN by NA
  
  weather[which(is.nan(weather$Temp)), "Temp"] <- NA
  weather[which(is.nan(weather$Humidity)), "Humidity"] <- NA
  weather[which(is.nan(weather$CO2)), "CO2"] <- NA
  
  # Check the data for inconsistencies. Unfortunately, the dataloggers in the greenhouses often add 0 as missing
  # records. This makes exremely complicated to know if one record was actually 0 or missing. This step check for
  # that inconsistency by comparing the temperature at hour 'x' with the mean for the previous 24 hours. If this
  # difference is above a given threshold the record is corrected to NA.
  
  if (check_data){
    
    for (i in 1 : length(weather$Temp)){
      
      if (!is.na(weather[i, "Temp"])){
        
        if (i < n_hours + 1) j <- 1 else j <- i - n_hours
        
        difference <- weather[i, "Temp"] - mean(weather[j : i, "Temp"], na.rm = T)
        
        if (abs(difference) > threshold){
          
          weather[i, "Temp"] <- NA}}}
    
    warning(paste(length(which(is.na(weather$Temp))), "observations were corrected to NA's due to",
                  "inconsistency with the mean temp for the previous", n_hours, "hours"))}
  
  # If the user wants to fix the missing records, this step helps to do it through linear interpolation
  
  if (fix_data){
    
    vector_temps_interpolated <- chillR::interpolate_gaps_hourly(weather)[["weather"]][["Temp"]]
    
    weather$Temp <- vector_temps_interpolated}
  
  
  # Check if the latest row has information. If not, this will remove that row
  
  if (is.na(weather[length(weather$Year), "Year"])) 
    
    weather <- weather[-length(weather$Year), ]
  
  # Do the same as above for the first row
  
  if (is.na(weather[1, "Year"])) 
    
    weather <- weather[-1, ]
  
  
  # Complete the data frame to get all the hours for the period
  
  weather <- chillR::make_all_day_table(weather, timestep = "hour")
  
  
  # Add the YEARMODAHO column
  
  weather["YEARMODAHO"] <- weather$Year * 1e+06 + weather$Month * 10000 + weather$Day * 100 + weather$Hour
  
  
  # Final part for returning a DF according to the time step and vars choosen
  
  if (time_step == "hourly")
    
    return(weather[c("YEARMODAHO", "Year", "Month", "Day", "Hour", vars)]) else {
      
      weather <- suppressWarnings(data.frame(weather %>% 
                                               dplyr::group_by(Year, Month, Day) %>%
                                               dplyr::summarise(Tmin = min(Temp, na.rm = T),
                                                                Tmean = mean(Temp, na.rm = T),
                                                                Tmax = max(Temp, na.rm = T),
                                                                Humidity = mean(Humidity, na.rm = T),
                                                                CO2 = mean(CO2, na.rm = T)) %>% 
                                               dplyr::mutate(YEARMODA = Year * 10000 +
                                                               Month * 100 + Day)))
      
      return(weather[c("YEARMODA", "Year", "Month", "Day", vars)])}
}
