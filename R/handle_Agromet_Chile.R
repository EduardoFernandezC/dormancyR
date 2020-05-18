#' Handle weather data downloaded from the Agromet Chilean database
#' 
#' This function convert a previously downloaded weather file into a dataframe in the \code{\link{chillR}}
#' format. For now,
#' the function \strong{only} works with hourly records. The downloading part must be done directly by the
#' user through accessing the
#' \href{https://agrometeorologia.cl/}{agromet website} Once there, the user needs to select the weather station,
#' the variables requested, the time-step and the period of interest. Finally, the data must be downloaded in
#' Excel or CSV format.
#' 
#' @param inputfile Character string or dataframe. This is the complete path to the weather file containing the data. It must
#' include the extension (".xls") of the file. The function also supports a dataframe
#' 
#' @param vars Character string. Vector of one or more variables that are requested from the original weather file.
#' Default is set to \emph{"Temp"} and \emph{"Humidity"} since for now, the function only works for hourly records.
#' Available options depend on
#' the variables specified by the user while downloading from the website. Possible inputs are
#' \emph{"Temp"}, \emph{"Humidity"}, \emph{"Prec_accum"}, \emph{"Solar_radiation"},
#' \emph{"Wind_speed"}, \emph{"Pressure_ATM"}, \emph{"Surface_Temp"}, \emph{"Soil_Temp"},
#' \emph{"Wind_direction"}
#' 
#' @examples
#' 
#' handle_agromet_chile(inputfile = agromet_weather_data, vars = c("Temp", "Humidity"))
#' 
#' @export handle_agromet_chile

handle_agromet_chile <- function(inputfile, vars = c("Temp", "Humidity")){
  
  # Check if the filepath provided exist or not
  
  if (is.character(inputfile))
    if(!file.exists(inputfile))
    stop("File does not exist. Please provide a valid inputfile or file")
  
  
  # Define possible variables to retrieve data
  
  vars_english <- c("Date/Hour", "Temp", "Humidity", "Prec_accum", "Solar_radiation",
                    "Wind_speed", "Pressure_ATM", "Surface_Temp", "Soil_Temp", "Wind_direction")
  
  # Check if the variables needed are possible to be retrieved
  
  if (!(any(vars %in% vars_english)))
    stop("Any of the variables required cannot be retrieved by this function. Please provide valid inputs as
    specified in the function's documentation")
  
  
  # Check the extension of the file to import
  
  if (is.character(inputfile))
    
    if (tools::file_ext(inputfile) == 'csv')
      
      data <- suppressMessages(readr::read_csv(inputfile, skip = 5)) else data <- readxl::read_excel(inputfile, skip = 5)
  
  if (is.data.frame(inputfile))
    
    data <- inputfile
  
  
  # Remove the last 4 rows having text data
  
  data <- data[1 : (nrow(data) - 4), ]
  
  # Remove unknown characters
  
  colnames(data) <- iconv(colnames(data), from = "latin1", to = "ASCII", sub = "")
  
  
  # Define the vars in spanish
  
  vars_spanish <- c("Tiempo UTC-4", "Temperatura del Aire C", "Humedad Relativa %",
                    "Precipitacin Acumulada mm", "Radiacin Solar w/m", "Velocidad de Viento km/h",
                    "Presin Atmosfrica mbar", "Temp. de Superficie C", "Temp. de Suelo Bajo 10cm C",
                    "Direccin de Viento ")
  
  
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
  
  
  # Separate the data into useful columns (Year, Month, Day, Hour)
  
  data <- tidyr::separate(data, "Date/Hour", into = c("Date", "Hour"), sep = " ", convert = T)
  
  data <- tidyr::separate(data, "Hour", into = c("Hour", "Min"), sep = ":", convert = T)
  
  data <- tidyr::separate(data, "Date", into = c("Day", "Month", "Year"), sep = "-", convert = T)
  
  
  # Return the simplyfied dataframe containing the columns Year, Month, Day and the variables required
  
  return(data[, c("Year", "Month", "Day", "Hour", vars[which(vars %in% colnames(data))])])}

