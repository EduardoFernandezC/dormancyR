#' Get weather data from Campus Klein Altendorf in chillR format
#' 
#' This function allows to handle weather data recorded by weather stations located in Campus Klein Altendorf
#' in Rhineland, Germany. Normally, the access to the data is restricted to CKA stuff but, any person could ask
#' for it (https://www.cka.uni-bonn.de/forschung/wetterdaten/aktuelle-wetterdaten). It is important to note that
#' CKA has the data organized into daily .csv files for the recent period as well as in .zip files for each year.
#' This function only works for data downloaded and extracted into one common folder. This means, all the files 
#' need to be in the same folder, independent of the year. The function returns the most important variables for
#' agricultural production. Among these, temperature, rainfall, wind speed, PAR, among others.
#' 
#' @param folder_path Character string. This is the complete directory name where the files are stored
#' 
#' @param vars Character string. Variables of interest returned by the function. These are:
#' "Wind_speed (m/s)", "Wind_direction (degrees)", "Temp (Celsius)", "Tmean (Celsius)", "Tmax (Celsius)",
#' "Tmin (Celsius)", "Humidity (%)", "Above_Ground_Temp (Celsius)", "Soil_Temp (Celsius)", "Precipitation (mm)",
#' "Radiation (W/m2)" and "PAR (umol/m2/s)". Default is set to temperature ("Temp")
#' 
#' @param time_step Character string. This is related to the frequency in which the data was recorded. Normally,
#' CKA records weather parameters each 10 min. However, this function only returns values for hourly and 
#' daily time steps. Default is set to "hourly"
#' 
#' @param check_data Boolean parameter to define whether the data should be quality checked or not. This means to
#' look for missing hours or days and fill them through linear interpolation. This only works for temperature
#' 
#' @details 
#' It is important to note that the data has to be downloaded by hand from the website or requested to the stuff
#' working at CKA. The output is a data frame useful for further analysis with chillR or this same package.
#' The interpolation is made by using some functions contained in the chillR package. Tmin, Tmean and Tmax
#' only work for "daily" time step and Temp only does for "hourly" time step.
#' 
#' @examples
#' 
#' # As each user has different path for the folder this example is not running until "#"
#' # is removed
#' 
#' # path <- "C:/Users/...../...."
#' 
#' # handle_CKA(folder_path = path, vars = c("Tmin", "Tmean", "Tmax"),
#' # time_step = "daily", check_data = T)
#' 
#' @export handle_CKA
#' @importFrom dplyr "%>%"

handle_CKA <- function(folder_path, vars = c("Temp"), time_step = "hourly", check_data = T){
  
  requireNamespace("dplyr")
  
  if (vars == "Temp" & time_step == "daily" || time_step == "hourly" & vars %in% c("Tmean", "Tmin", "Tmax"))
    stop("Variable(s) not included for the time step specified. Please change either the vars or time_step
  parameter into a valid option")
  
  
  if (any(!(vars %in% c("Wind_speed", "Wind_direction", "Temp", "Tmean", "Tmax", "Tmin", "Humidity",
                        "Above_Ground_Temp", "Soil_Temp", "Precipitation", "Radiation", "PAR"))))
    stop("Variables not supported by the function. Please specified any valid option")
  
  
  if (!(time_step %in% c("hourly", "daily")))
    stop("Time step not supported by the function. Please specified any valid option")
  
  
  # As CKA has records for each day separately, this is to get the name of the files downloaded by hand, extrated
  # and saved in one common folder.
  
  files <- list.files(folder_path)
  
  if (length(files) == 0)
    stop("No files found. Has been the folder path specified correctly?")
  
  # Keep only csv files
  
  if (length(which(substr(files, 23, 27) != ".csv")) == 0) files <- files else
    files <- files[-which(substr(files, 23, 27) != ".csv")]
  
  
  # Check if the file "003090_2017-06-20_stat.csv" exist and if so, remove it since it is blank
  
  if ("003090_2017-06-20_stat.csv" %in% files)
    files <- files[-which(files == "003090_2017-06-20_stat.csv")] else
      files <- files
  
  
  # Select only the important variables to reduce the time spent in loading the data
  
  variables_german <- c(1, 2, 4, 7, 8, 11, 14, 15, 17, 19, 28)
  
  
  # Make a primer null dataframe
  
  weather <- NULL
  
  for (i in files){
    
    # Some files start in row 68, while other start on row 63. 3067 is the treshold between both format files
    
    if (as.numeric(substr(i, 1, 6)) < 3067) row <- 68 else row <- 63
    
    # Read each file into one dataframe
    
    data <- utils::read.csv(paste("C:/Users/Admin/Desktop/temps/", i, sep = ""),
                     skip = row, stringsAsFactors = F)[variables_german]
    
    # Merge these data into the main dataframe
    
    if (is.null(weather)) weather <- data else
      weather <- dplyr::bind_rows(weather, data)}
  
  
  # Change the name of the columns to avoid problems with reoxygen
  
  colnames(weather) <- c("date", "time", "Wind_speed", "Wind_direction", "Temp", "Humidity", "Above_Ground_Temp",
                         "Soil_Temp", "Precipitation", "Radiation", "PAR")
  
  
  # Separate the time column to get unique hours
  
  weather <- tidyr::separate(weather, "time", c("Hour", "Min", "Sec"), convert = TRUE)
  
  
  # Summarize the data by hour
  
  weather <- data.frame(weather %>% dplyr::group_by(date, Hour) %>%
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
  
  weather <- tidyr::separate(weather, "date", c("Year", "Month", "Day"), sep = "-", convert = TRUE)
  
  
  # Check the data
  
  if (check_data) weather_all_days <- chillR::make_all_day_table(weather, timestep = "hour", add.DATE = F)
  
  
  # Check for missing days
  
  if(length(weather$Year) != length(weather_all_days$Year))
    weather_all_days <- chillR::interpolate_gaps_hourly(weather_all_days, latitude = 50.6248)
  
  
  # Decide on one of the two options (the real one versus the interpolated one)
  
  if (check_data) weather <- weather_all_days$weather
  
  
  # Add the YEARMODAHO column in the case the user has specified "hourly" records
  
  if (time_step == "hourly") weather["YEARMODAHO"] <- weather$Year * 1000000 + weather$Month * 10000 +
    weather$Day * 100 +  weather$Hour
  
  
  # In case the user has specified daily records this add the YEARMODA column
  
  if (time_step == "daily") weather["YEARMODA"] <- weather$Year * 10000 + weather$Month * 100 +
    weather$Day
  
  
  # For daily data, this resumes the parameter into one value per day. Only in the case of temperatures
  # this includes minimum and maximun values extracted from hourly records
  
  if (time_step == "daily") weather <- data.frame(weather %>% dplyr::group_by(YEARMODA, Year, Month, Day) %>%
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
  
  
  if (time_step == "hourly")
    return(weather[c("YEARMODAHO", "Year", "Month", "Day", "Hour", vars)]) else
      return(weather[c("YEARMODA", "Year", "Month", "Day", vars)])}