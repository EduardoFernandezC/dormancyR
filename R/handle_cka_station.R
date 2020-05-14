#' Get weather data from Campus Klein Altendorf in \code{\link{chillR}} format
#' 
#' This function allows to handle weather data recorded by the weather station located in Campus Klein Altendorf
#' in Rhineland, Germany. Normally, the access to the data is restricted to CKA staff but, any person could ask
#' for it at: \url{https://www.cka.uni-bonn.de/forschung/wetterdaten/aktuelle-wetterdaten}. It is important to note that
#' CKA has the data organized into daily \strong{'.csv'} files for the recent period as well as in \strong{'.zip'} files for each year.
#' This function only works for data downloaded and extracted as \strong{'.csv'} files into one common folder. This means, all the files 
#' need to be in the same folder, independent of the year. The function returns the most important variables for
#' agricultural production. Among these, temperature, rainfall, wind speed, PAR, among others.
#' 
#' @param folder_path Character string. This is the complete directory name where the files are stored
#' 
#' @param vars Character string. Variables of interest returned by the function. These are:
#' \emph{"Wind_speed"} (m/s), \emph{"Wind_direction"} (degrees), \emph{"Temp"} (Celsius), \emph{"Tmean"} (Celsius), \emph{"Tmax"} (Celsius),
#' \emph{"Tmin"} (Celsius), \emph{"Humidity"} (\%), \emph{"Above_Ground_Temp"} (Celsius), \emph{"Soil_Temp"} (Celsius), \emph{"Precipitation"} (mm),
#' \emph{"Radiation"} (W/m2) and \emph{"PAR"} (umol/m2/s). Default is set to temperature (\emph{"Temp"}) in an hourly basis
#' 
#' @param time_step Character string. This is related to the frequency in which the data was recorded. Normally,
#' CKA records weather parameters each 10 min. However, this function summarizes values into \emph{hourly} and 
#' \emph{daily} basis. Default is set to \emph{hourly}
#' 
#' @param check_data Boolean parameter to define whether the data should be quality checked or not. This means to
#' look for missing observations and fill them through \code{\link[chillR:interpolate_gaps_hourly]{chillR::interpolate_gaps_hourly}}. This only works for temperature
#' records
#' 
#' @details 
#' It is important to note that the data has to be downloaded by hand from the website or requested to the staff
#' at CKA. The output is a data frame useful for further analysis with \code{\link{chillR}} or this package. \emph{Tmin}, \emph{Tmean} and \emph{Tmax}
#' only work for \emph{daily} time step whereas \emph{Temp} only does for \emph{hourly} time step.
#' 
#' @examples
#' 
#' # As each user has different path for the folder this example is not running until "#"
#' # is removed
#' 
#' # path <- "C:/Users/...../...."
#' 
#' # handle_cka_station(folder_path = path, vars = c("Tmin", "Tmean", "Tmax"),
#' # time_step = "daily", check_data = TRUE)
#' 
#' @export handle_cka_station
#' @importFrom dplyr "%>%"

handle_cka_station <- function(folder_path, vars = c("Temp"), time_step = "hourly", check_data = TRUE){
  
  requireNamespace("dplyr")
  
  # Check for valid inputs in vars and time_step parameters
  
  if (vars == "Temp" & time_step == "daily" || time_step == "hourly" & vars %in% c("Tmean", "Tmin", "Tmax"))
    
    stop("Variable(s) not included for the time step specified. Please change either the 'vars' or 'time_step'
  parameter into a valid option")
  
  
  if (any(!(vars %in% c("Wind_speed", "Wind_direction", "Temp", "Tmean", "Tmax", "Tmin", "Humidity",
                        "Above_Ground_Temp", "Soil_Temp", "Precipitation", "Radiation", "PAR"))))
    
    stop("Variables not supported by the function. Please specified any valid option")
  
  
  if (!(time_step %in% c("hourly", "daily")))
    
    stop("Time step not supported by the function. Please specified any valid option")
  
  
  # Check if the last character in folder_path is '/' if so, remove it for future computations
  
  if (substr(folder_path, nchar(folder_path) - 1, nchar(folder_path)) == '/')
    
    folder_path <- substr(folder_path, 1, nchar(folder_path) - 1)
  
  
  # As CKA has records for each day separately, this is to get the name of the files downloaded by hand
  # and saved in a given folder (specified in folder_path)
  
  files <- list.files(folder_path)
  
  # Check files availability
  
  if (length(files) == 0)
    
    stop("No files found. Has been the folder path specified correctly?")
  
  # Check for non-csv like files and remove them from the 'files' vector
  
  if (!all(tools::file_ext(files) == "csv")){
    
    warning(paste(length(which(tools::file_ext(files) != "csv")), "file(s) ignored since",
                  "the extension is not '.csv'"))
    
    files <- files[-which(tools::file_ext(files) != "csv")]}
  
  # Check for empty files and remove them from the reading part
  
  if (any(file.size(paste(folder_path, "/", files, sep = "")) == 0)){
    
    warning(paste(length(which(file.size(paste(folder_path, "/", files, sep = "")) == 0)), "file(s) ignored",
                  "since its (their) size is 0 bytes"))
    
    files <- files[-which(file.size(paste(folder_path, "/", files, sep = "")) == 0)]}
  
  
  # Select only the important variables (by position) to reduce the time spent in loading the data
  
  variables_german <- c(1, 2, 4, 7, 8, 11, 14, 15, 17, 19, 28)
  
  
  # This step is to read all the files into R
  
  weather_data <- NULL
  
  for (file in files) {
    
    # read the file as is to know which lines should be skiped
    temp_data <- utils::read.csv(paste(folder_path, "/", file, sep = ""), sep = ";")
    
    # extract the name of the column
    col_name <- names(temp_data)
    
    # get the numbers of lines that should be skiped
    skip <- which(substr(temp_data[, col_name], 1, 4) == "date")
    
    # read the data keeping only the rows and columns of interest
    data <- utils::read.csv(paste(folder_path, "/", file, sep = ""),
                            skip = skip, stringsAsFactors = F)[variables_german]
    
    # save the data in one table
    if (is.null(weather_data)) weather_data <- data else
      
      weather_data <- dplyr::bind_rows(weather_data, data)
  }
  
  # Change the name of the columns to avoid problems with reoxygen2
  
  colnames(weather_data) <- c("date", "time", "Wind_speed", "Wind_direction", "Temp", "Humidity",
                              "Above_Ground_Temp", "Soil_Temp", "Precipitation", "Radiation", "PAR")
  
  
  
  # Separate the time column to get unique hours
  
  weather_data <- tidyr::separate(weather_data, "time", c("Hour", "Min", "Sec"), convert = TRUE)
  
  
  # Summarize the data by hour
  
  weather_data <- data.frame(weather_data %>% dplyr::group_by(date, Hour) %>%
                               dplyr::summarise(Wind_speed = mean(Wind_speed, na.rm = T),
                                                Wind_direction = mean(Wind_direction, na.rm = T),
                                                Temp = mean(Temp, na.rm = T),
                                                Humidity = mean(Humidity, na.rm = T),
                                                Above_Ground_Temp = mean(Above_Ground_Temp, na.rm = T),
                                                Soil_Temp = mean(Soil_Temp, na.rm = T),
                                                Precipitation = sum(Precipitation, na.rm = T),
                                                Radiation = mean(Radiation, na.rm = T),
                                                PAR = mean(PAR, na.rm = T)))
  
  
  # Add the columns Year, Month and Day to such dataframe
  
  weather_data <- tidyr::separate(weather_data, "date", c("Year", "Month", "Day"),
                                  sep = "-", convert = TRUE)
  
  
  # Get the total number of observation in the original dataframe
  
  observations <- length(weather_data$Year)
  
  # Check the data
  
  if (check_data){
    
    weather_data <- chillR::make_all_day_table(weather_data, timestep = "hour", add.DATE = F)
    
    warning(paste(length(weather_data$Year) - observations, "missing observations were",
                  "filled by 'interpolate_gaps_hourly' function"))
    
    
    # Interpolate hourly temps according to latitude. This will add a bit of error since it does from
    # sunlight curves instead of empirical observations
    
    weather_data <- chillR::interpolate_gaps_hourly(weather_data, latitude = 50.62)
    
    weather_data <- weather_data$weather}
  
  
  # Add the YEARMODAHO column in the case the user has specified 'hourly' records
  
  if (time_step == "hourly")
    
    weather_data["YEARMODAHO"] <- weather_data$Year * 1000000 + weather_data$Month * 10000 +
    weather_data$Day * 100 +  weather_data$Hour
  
  
  # In case the user has specified 'daily' records this add the YEARMODA column
  
  if (time_step == "daily")
    
    weather_data["YEARMODA"] <- weather_data$Year * 10000 + weather_data$Month * 100 + weather_data$Day  
  
  
  # For daily data, this resumes the parameter into one value per day. Only in the case of temperatures
  # this includes minimum and maximun values extracted from hourly records
  
  if (time_step == "daily")
    
    weather_data <- data.frame(weather_data %>% dplyr::group_by(YEARMODA, Year, Month, Day) %>%
                                 dplyr::summarise(Wind_speed = mean(Wind_speed, na.rm = T),
                                                  Wind_direction = mean(Wind_direction, na.rm = T),
                                                  Tmean = mean(Temp, na.rm = T),
                                                  Tmax = max(Temp, na.rm = T),
                                                  Tmin = min(Temp, na.rm = T),
                                                  Humidity = mean(Humidity, na.rm = T),
                                                  Above_Ground_Temp = mean(Above_Ground_Temp, na.rm = T),
                                                  Soil_Temp = mean(Soil_Temp, na.rm = T),
                                                  Precipitation = sum(Precipitation, na.rm = T),
                                                  Radiation = mean(Radiation, na.rm = T),
                                                  PAR = mean(PAR, na.rm = T)))
  
  # Return the dataframe
  
  if (time_step == "hourly")
    
    return(weather_data[c("YEARMODAHO", "Year", "Month", "Day", "Hour", vars)]) else
      return(weather_data[c("YEARMODA", "Year", "Month", "Day", vars)])
}
