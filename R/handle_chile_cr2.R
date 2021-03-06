#' Get historical records of extreme temperatures for Chile
#'
#' This function allows to obtain information about weather stations located in Chile as well as
#' a data frame containing historical records of minimum and maximum temperatures from those weather
#' stations. This function works with data downloaded from the website of the
#' \href{http://www.cr2.cl/}{Center for Climate and Resilience Research (CR)2} sponsored by the University of Chile.
#' The function allows to download the data (only recommended once) as well as to handle zip files previously
#' downloaded. Data can be downloaded from the following links: \href{http://www.cr2.cl/download/cr2_tasmindaily_2018_ghcn-zip/?wpdmdl=15125}{minimum temperatures}
#' and \href{http://www.cr2.cl/download/cr2_tasmaxdaily_2018_ghcn-zip/?wpdmdl=15126}{maximum temperatures}
#' 
#' @param action Character string input that defines the kind of data required. There are three options
#' for this parameter:
#'  \describe{
#'   \item{\emph{"info_stations"}}{provides a dataframe with information about a given number
#'   of weather stations (set in \code{number_of_stations}) located close to the location
#'   established with \code{latitude} and \code{longitude} parameters}
#'   \item{\emph{"list_data"}}{provides a list of dataframes containing minimum and maximum temperature
#'    records from each weather station obtained with the \emph{"info_stations"} mode within the period
#'    established in the call of the function}
#'   \item{\emph{"my_data"}}{provides the first dataframe of the previous list and represent the
#'   data of the closest weather station to the location established}}
#' 
#' @param begin Numeric parameter in YEARMODA format. This parameter represents the
#' initial date of the period of interest. If it is not provided, the default is established as
#' 19500101 which correspond to the oldest date possible to use
#' 
#' @param end Numeric parameter in YEARMODA format. This parameter represents the
#' final date of the period of interest. If it is not provided, the default is established as
#' 20180310 which corresponds to the earliest possible date that can be used for the assessment
#' 
#' @param latitude Numerical input. Latitude of the site of interest in decimal format
#' 
#' @param longitude Numerical input. Longitude of the site of interest in decimal format
#' 
#' @param number_of_stations Numerical input. Number of stations wanted to use as closest stations to
#' the site of interest. Default option is the value 25
#' 
#' @param path_zip_tmin Character string input. Location of the zip file containing minimum
#' temperatures. This input must include the name and extension of the file. Default is set to \code{NULL} so the
#' function will download the data from the \href{http://www.cr2.cl/}{CR2} site and store the zip file in a
#' temporary folder. If you have the data, please provide the path. This will save a bit of time.
#' 
#' @param path_zip_tmax Character string input. Location of the zip file containing maximum
#' temperatures. This input must include the name and extension of the file. Default is set to NULL so the
#' function will download the data from the \href{http://www.cr2.cl/}{CR2} site and store the zip file in a
#' temporary folder. If you have the data, please provide the path. This will save a bit of time.
#' 
#' @param stations_df A dataframe containing the list of stations for which this function will retrieve
#' weather data. It is important that this dataframe be produced by this function under the action 
#' \emph{"info_stations"}. The default is set to \code{NULL}
#' 
#' @param keep_data Boolean parameter indicating if the data downloaded should be deleted or not
#' 
#' @examples
#' 
#' #handle_chile_cr2(action = "my_data", begin = 20000101, end = 20101030,
#' #                latitude = -32.8958, longitude = -71.2092, number_of_stations = 25,
#' #                path_zip_tmin = NULL, path_zip_tmax = NULL,
#' #                stations_df = NULL, keep_data = TRUE)
#' 
#' 
#' @export handle_chile_cr2

handle_chile_cr2 <- function(action, begin = 19500101, end = 20180309,
                             latitude, longitude, number_of_stations = 25,
                             path_zip_tmin = NULL, path_zip_tmax = NULL,
                             stations_df = NULL, keep_data = TRUE){
  
  # Check input parameters
  
  if (!(action %in% c("my_data", "info_stations", "list_data")))
    stop("Please provide a valid 'action' for the function")
  
  if (!is.numeric(begin) | !is.numeric(end))
    stop("Please provide a numeric input in 'begin' or 'end' parameters in YEARMODA format")
  
  if (begin < 19500101 | end > 20180309)
    stop("'begin' or 'end' parameters out of range. Please provide a valid input")
  
  if (begin >= end)
    stop("'begin' parameter is greater than 'end' parameter. Please provide a valid input")
  
  if (!is.numeric(latitude) | !is.numeric(longitude) | !is.numeric(number_of_stations))
    stop("Please provide a numeric input in 'latitude', 'longitude', and 'number_of_stations' parameters")
  
  if (!is.null(stations_df) & !is.data.frame(stations_df))
    stop("Please provide a data frame in 'stations_df' parameter")
  
  
  # Check if the zip files have been already downloaded. If not, this will download the zip files from
  # the cr2 site
  
  # Tmin
  
  if (is.null(path_zip_tmin)){
    
    message("'path_zip_tmin' not provided. Data are going to be download from 'http://www.cr2.cl/' \ndatabase and stored in a temporary folder")
    
    if (!dir.exists("temp_data"))
      
      dir.create("temp_data")
    
    utils::download.file("http://www.cr2.cl/download/cr2_tasmindaily_2018_ghcn-zip/?wpdmdl=15125",
                  "temp_data/cr2_tasmindaily_2018_ghcn.zip", mode = "wb")
    
    path_zip_tmin <- "temp_data/cr2_tasmindaily_2018_ghcn.zip"} else {
      
      if (!file.exists(path_zip_tmin)){
        stop("'path_zip_tmin' file doesn't exists. Please provide a valid path to the zip file \nfor minimum records")}}
  
  
  # Tmax
  
  if (is.null(path_zip_tmax)){
    
    message("'path_zip_tmax' not provided. Data are going to be download from 'http://www.cr2.cl/' \ndatabase and stored in a temporary folder")
    
    if (!dir.exists("temp_data"))
      
      dir.create("temp_data")
    
    utils::download.file("http://www.cr2.cl/download/cr2_tasmaxdaily_2018_ghcn-zip/?wpdmdl=15126",
                  "temp_data/cr2_tasmaxdaily_2018_ghcn.zip", mode = "wb")
    
    path_zip_tmax <- "temp_data/cr2_tasmaxdaily_2018_ghcn.zip"} else {
      
      if (!file.exists(path_zip_tmax)){
        stop("'path_zip_tmax' file doesn't exists. Please provide a valid path to the zip file \nfor maximum records")}}
  
  
  
  #Getting the names of the files inside the zip document using a dataframe
  
  path_tmin <- utils::unzip(path_zip_tmin, list = T)
  path_tmax <- utils::unzip(path_zip_tmax, list = T)
  
  
  #Setting the to unzip the files
  
  pattern_positions <- unlist(stringr::str_locate_all(path_zip_tmin, pattern = '/'))
  
  pattern_end <- max(pattern_positions)
  
  folder_path <- substr(path_zip_tmin, 1, pattern_end - 1)
  
  
  # Check if the zip files were already unzipped
  
  if (!dir.exists(substr(path_zip_tmin, 1, nchar(path_zip_tmin) - 4)))
    
    #"unziping" Tmin files
    
    utils::unzip(path_zip_tmin, exdir = folder_path)
  
  if (!dir.exists(substr(path_zip_tmax, 1, nchar(path_zip_tmax) - 4)))
    
    #"unziping" Tmin files
    
    utils::unzip(path_zip_tmax, exdir = folder_path)
  
  
  #Loading the dataframe that contains the information about the weather stations available for 
  #Tmin records
  
  Stations_Tmin <- utils::read.table(as.character(paste0(folder_path, "/", path_tmin[3, 1])),
                                     sep = ",", header = T, quote = "\"")
  
  #Selecting the columns of interest 
  
  Stations_Tmin <- subset(Stations_Tmin, select = c("codigo_estacion", "institucion", "fuente", "nombre",
                                                    "altura", "latitud", "longitud"))
  
  #Extracting the cod labels for each weather station to use as column names for loading the Tmin
  #dataframe
  
  Cod_stations_Tmin <- as.character(Stations_Tmin$codigo_estacion)
  Cod_stations_Tmin <- paste("Cod_", Cod_stations_Tmin, sep = "")
  
  #Replacing the original Cod label in the dataframe
  
  Stations_Tmin$codigo_estacion <- Cod_stations_Tmin
  
  #Making the column names for loading the Tmin dataframe
  
  Cod_stations_Tmin <- c("Fecha", Cod_stations_Tmin)
  
  #Loading the Tmin dataframe. This step uses as column names the cod labels mentioned above.
  
  Tmin <- utils::read.table(as.character(paste0(folder_path, "/", path_tmin[2, 1])), sep = ",",
                            quote = "", skip = 15, blank.lines.skip = F,
                            col.names = Cod_stations_Tmin, na.strings = c("-9999", "-9999.000"))
  
  Tmin$Fecha <-as.Date(Tmin$Fecha)
  
  # Add the column YEARMODA for subsetting
  
  Tmin$YEARMODA <- chillR::Date2YEARMODA(Tmin$Fecha)
  
  #Selecting only the period of interest
  
  Tmin <- Tmin[which(Tmin$YEARMODA >= begin & Tmin$YEARMODA <= end), ]
  
  #Loading the dataframe that contains the information about the weather stations available for 
  #Tmax records 
  
  Stations_Tmax <- utils::read.table(as.character(paste0(folder_path, "/", path_tmax[3, 1])),
                                     sep = ",", header = T, quote = "\"")
  
  #Selecting the columns of interest
  
  Stations_Tmax <- subset(Stations_Tmax, select = c("codigo_estacion", "institucion", "fuente", "nombre",
                                                    "altura", "latitud", "longitud"))
  
  #Extracting the cod labels for each weather station to use as column names for loading the Tmax
  #dataframe
  
  Cod_stations_Tmax <- as.character(Stations_Tmax$codigo_estacion)
  Cod_stations_Tmax <- paste("Cod_", Cod_stations_Tmax, sep = "")
  
  #Replacing the original Cod label in the dataframe
  
  Stations_Tmax$codigo_estacion <- Cod_stations_Tmax
  
  #Making the column names for loading the Tmin dataframe
  
  Cod_stations_Tmax <- c("Fecha", Cod_stations_Tmax)
  
  #Loading the Tmin dataframe. This step uses as column names the cod labels mentioned above.
  
  Tmax <- utils::read.table(as.character(paste0(folder_path, "/", path_tmax[2, 1])),
                            sep = ",", quote = "", skip = 15, blank.lines.skip = F,
                            col.names = Cod_stations_Tmax, na.strings = c("-9999", "-9999.000"))
  
  Tmax$Fecha <- as.Date(Tmax$Fecha)
  
  # Add the column YEARMODA for subsetting
  
  Tmax$YEARMODA <- chillR::Date2YEARMODA(Tmax$Fecha)
  
  #Selecting the period of interest
  
  Tmax <- Tmax[which(Tmax$YEARMODA >= begin & Tmax$YEARMODA <= end), ]
  
  
  # Remove the downloaded data in case you want to
  
  if (!keep_data)
    
    unlink(folder_path, recursive = T)
  
  
  
  #Setting the location to compute the distance of the weather stations
  
  mypoint <- c(longitude, latitude)
  
  #Adding the distance of each station to the location specified above
  
  Stations_Tmin[, "distance"] <- round(sp::spDistsN1(as.matrix(Stations_Tmin[, c("longitud", "latitud")]),
                                                     mypoint, longlat = T), 2)
  
  #Ordering the dataframe according to the distance
  
  Sorted_Stations <- Stations_Tmin[order(Stations_Tmin$distance), ]
  
  #Selecting the relevant weather stations according to the number of stations
  
  Sumarized_stations <- Sorted_Stations[c(1 : number_of_stations), ]
  
  colnames(Sumarized_stations) <- c("Cod_Station", "Institution", "Source", "Name", "Elevation", "Latitude",
                                    "Longitude", "Distance")
  
  #Creating a primer data for the period of interest
  
  primer_data <- data.frame(Weather_Station = NA,
                            Year = c(as.numeric(substr(begin, 1, 4)),
                                     as.numeric(substr(end, 1, 4))),
                            Month = c(as.numeric(substr(begin, 5, 6)),
                                      as.numeric(substr(end, 5, 6))),
                            Day = c(as.numeric(substr(begin, 7, 8)),
                                    as.numeric(substr(end, 7, 8))),
                            JDay = as.numeric(NA),
                            Tmin = as.numeric(NA),
                            Tmax = as.numeric(NA))
  
  
  daily_data <- chillR::make_all_day_table(primer_data, add.DATE = FALSE)
  
  daily_data <- chillR::make_JDay(daily_data)
  
  #Merging Tmin y Tmax according to the code of the station for the total number of stations setted
  
  # Check if the df with the station list has been provided while calling the function
  
  if (is.data.frame(stations_df) & "Cod_Station" %in% colnames(stations_df) & "Name" %in% colnames(stations_df))
    Sumarized_stations <- stations_df
  
  dfs <- NULL
  
  for (i in 1 : length(Sumarized_stations$Cod_Station)) {
    
    daily_data[, "Weather_Station"] <- as.character(Sumarized_stations[i, "Name"])
    
    if (Sumarized_stations[i, "Cod_Station"] %in% colnames(Tmin)){
      
      daily_data[, "Tmin"] <- Tmin[, Sumarized_stations[i, "Cod_Station"]]} else {
        
        daily_data[, "Tmin"] <- as.numeric(NA)}
    
    if (Sumarized_stations[i, "Cod_Station"] %in% colnames(Tmax)){
      
      daily_data[,"Tmax"] <- Tmax[, Sumarized_stations[i, "Cod_Station"]]} else {
        
        daily_data[, "Tmax"] <- as.numeric(NA)}
    
    
    dfs <- c(dfs, list(daily_data))
  }
  
  if(action == "list_data" & is.null(stations_df))
    return(dfs[2 : number_of_stations])
  
  if(action == "list_data" & is.data.frame(stations_df))
    return(dfs[2 : length(stations_df[, 1])])
  
  if(action == "my_data")
    return(dfs[[1]])
  
  #Computing the number of observations for the whole period. This value includes Tmin + Tmax observations
  
  N_obs <- NULL
  
  for (i in 1 : length(dfs)) {
    
    NobsTemp <- length(which(!is.na(dfs[[i]]["Tmin"]))) + length(which(!is.na(dfs[[i]]["Tmax"])))
    
    N_obs <- c(N_obs, NobsTemp)}
  
  #Adding the number of observations vector to the dataframe of stations  
  
  Sumarized_stations[, "N_Obs"] <- N_obs
  
  #Calculating the percentage of days with complete data  
  
  Sumarized_stations[, "Perc_days_complete"] <- round((N_obs / length(dfs[[1]][, 1])) * 100 / 2, 2)
  
  if(action == "info_stations")
    return(Sumarized_stations)
  
}
