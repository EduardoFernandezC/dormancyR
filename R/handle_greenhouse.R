#' Get weather data from the greenhouses at CKA in \code{\link{chillR}} format
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
#' # handle_greenhouse(path_data = path, time_step = "daily", vars = c("Tmin", "Tmax"))
#' 
#' @export handle_greenhouse
#' @importFrom dplyr "%>%"

handle_greenhouse <- function (path_data, time_step = "hourly", vars = c("Temp", "Humidity", "CO2")){
  
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
    
    weather <- suppressWarnings(suppressMessages(readxl::read_excel(path_data, sheet = 1, na = "FAULT")))
  
  if (tools::file_ext(path_data) == "csv")
    
    weather <- utils::read.csv(path_data, na.strings = "FAULT")
  
  
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
  
  
  # Change NaN by NA
  
  weather[which(is.na(weather$Temp)), "Temp"] <- NA
  weather[which(is.na(weather$Humidity)), "Humidity"] <- NA
  weather[which(is.na(weather$CO2)), "CO2"] <- NA
  
  
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
