#' Handle weather data downloaded from the Agromet Chilean database
#' 
#' This function convert a previously downloaded weather file into a dataframe in the \code{\link{chillR}} format. For now,
#' the downloading part must be done directly by the user through accessing the website
#' \url{http://agromet.inia.cl/estaciones.php} Once there, the user needs to select the weather station,
#' the variables requested, the time-step and the period of interest. Finally, the data must be downloaded in
#' excel format and stored. For now, this function only works for files downloaded in the hourly time-step option.
#' 
#' @param pathfile Character string. This is the complete path to the excel file containing the weather data. It must
#' include the extension (".xls") of the file.
#' 
#' @param vars Character string. Vector of one or more variables that are requested from the original weather file.
#' Default is set to \emph{"Temp"} since for now, the function only works for hourly records. Available options depend on
#' the variables specified by the user while downloading from the website. Possible inputs are \emph{"Precip_Accum",
#' "Humidity",
#' "Wind_direction", "Solar_radiation", "Pressure_ATM", "Temp", "Tmax", "Tmin", "Soil_Tmean", "Soil_Tmax",
#' "Wind_speed_max", "Surface_Tmean", "Surface_Tmax", "Soil_Tmin", "Surface_Tmin", "Wind_speed_mean"}.
#' 
#' @examples
#' # As each user has different path for the folder this example is not running until "#"
#' # is removed
#' 
#' # path <- "C:/Users/...../...."
#' 
#' # handle_agromet_chile(pathfile = path, vars = c("Tmin", "Tmean", "Tmax"))
#' 
#' @export handle_agromet_chile

handle_agromet_chile <- function(pathfile, vars = c("Temp")){
  
  # Check if the filepath provided exist or not
  
  if (!file.exists(pathfile))
    stop("File does not exist. Please provide a valid pathfile or file")
  
  
  # Define possible variables to retrieve data
  
  vars_english <- c("Date/Hour", "Precip_Accum", "Humidity", "Wind_direction", "Solar_radiation",
                    "Pressure_ATM", "Temp", "Tmax", "Tmin", "Soil_Tmean", "Soil_Tmax",
                    "Wind_speed_max", "Surface_Tmean", "Surface_Tmax", "Soil_Tmin", "Surface_Tmin",
                    "Wind_speed_mean")
  
  # Check if the variables needed are possible to be retrieved
  
  if (!(any(vars %in% vars_english)))
    stop("Any of the variables required cannot be retrieved by this function. Please provide valid inputs as
    specified in the function's documentation")
  
  
  # Read the data as a HTML table since xls format is not the real format of the data downloaded
  
  data <- XML::readHTMLTable(pathfile)
  
  # Change the names of the output list for further subsetting
  
  names(data) <- c("Info", "Data")
  
  # Select only the dataframe containing the weather data
  
  data <- data$Data
  
  
  # Due to issues with the translation from Spanish to English, I had to remove the letters having non-ASCII
  # characters. I am not sure if this will work in a Spanish windows option
  
  colnames(data) <- iconv(colnames(data), from = "latin1", to = "ASCII", sub = "")
  
  # Since these variables or column names do not change across files. I set up all the possibilities that
  # a user has while downloading the data
  
  vars_spanish <- c("FECHA/HORA (UTC-4)", "Precipitacin Acumulada (mm)", "Humedad Relativa (%)",
                    "Direccin del Viento (moda)", "Radiacin Solar (W/m2)", "Presin (mbar)",
                    "Temperatura del Aire (C)", "Temperatura del Aire Mxima ABS (C)",
                    "Temperatura del Aire Mnima ABS (C)", "Temperatura Suelo 10cm (C)",
                    "Temperatura Suelo 10cm Mxima ABS (C)", "Rfaga (Km/h)", "Temperatura de Superficie (C)",
                    "Temperatura de Superficie Mxima ABS (C)", "Temperatura Suelo 10cm Mnima ABS (C)",
                    "Temperatura de Superficie Mnima ABS (C)", "Velocidad del Viento (Km/h)")
  
  
  # At this point, filter only the relevant columns
  
  data <- data[, which(colnames(data) %in% vars_spanish)]
  
  
  # Translate the name of the columns. This works 1:1. It means that the second element of vector vars_spanish has to
  # be exactly the translation for it that is located at vars_english[2]
  
  for (i in 1 : length(vars_spanish))
    
    colnames(data)[which(colnames(data) == vars_spanish[i])] <- vars_english[i]
  
  
  # Check if the file has all the variables required while calling the function
  
  if(!(all(vars %in% colnames(data))))
    warning("Some variables are not available in the original dataframe. Are you sure downloaded the
     correct variables?")
  
  
  # In the original dataframe the decimal separator is "," instead of ".". This for loop solves it
  
  for (i in colnames(data)[-which(colnames(data) %in%  c("Date/Hour", "Wind_direction"))])
    
    data[, i] <- as.numeric(stringr::str_replace_all(data[, i], ",", "."))
  
  
  # Separate the data into useful columns (Year, Month, Day, Hour)
  
  data <- tidyr::separate(data, "Date/Hour", into = c("Date", "Hour"), sep = " ", convert = T)
  
  data <- tidyr::separate(data, "Hour", into = c("Hour", "Min"), sep = ":", convert = T)
  
  data <- tidyr::separate(data, "Date", into = c("Day", "Month", "Year"), sep = "-", convert = T)
  
  
  # Check if "Temp" is a column in the dataframe
  
  if(!("Temp" %in% colnames(data))) data["Temp"] <- as.numeric(NA)
  
  
  # If missing days or hours, this make the complete dataframe for the period of interest.
  
  data <- chillR::make_all_day_table(data, timestep = "hour")
  
  # Return the simplyfied dataframe containing the columns Year, Month, Day and the variables required
  
  return(data[, c("Year", "Month", "Day", "Hour", vars[which(vars %in% colnames(data))])])}