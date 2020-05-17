#' Get historical records of daily precipitation for Chile
#'
#' This function allows to obtain information about weather stations located in Chile as well as
#' a data frame containing historical records of daily precipitation from those weather
#' stations. This function works with data previously downloaded from the website of the
#' \href{http://www.cr2.cl/}{Center for Climate and Resilience Research (CR)2} sponsored by the University of Chile.
#' The function works for
#' downloaded data in ".zip" format. Data can be obtained from the following
#' \href{http://www.cr2.cl/download/cr2_tasmindaily_2018_ghcn-zip/?wpdmdl=15125}{link}.
#'
#' @param action Character string input that defines the kind of data required. There are three options
#' for this parameter. \emph{"info_stations"} provides a dataframe with information about a given number
#' of weather stations (set in \code{number_of_stations}) located close to the location
#' established with \code{latitude} and \code{longitude} parameters. \emph{"list_data"} provides a list of
#' dataframes containing daily precipitation records from each weather station
#' obtained with the \emph{"info_stations"} mode within the period established in the call of the function.
#' Finally, \emph{"my_data"} provides the first dataframe of the previous list and represent the
#' data of the closest weather station to the location established
#' 
#' @param begin Numeric parameter in YEARMODA format. This parameter represents the
#' initial date of the period of interest. If it is not provided, the default is set to
#' 19500101 which correspond to the oldest date possible to use
#' 
#' @param end Numeric parameter in YEARMODA format. This parameter represents the
#' final date of the period of interest. If it is not provided, the default is set to
#' 20180309 which corresponds to the earliest possible date that can be used for the assessment
#' 
#' @param latitude Numerical input. Latitude of the site of interest in decimal format
#' 
#' @param longitude Numerical input. Longitude of the site of interest in decimal format
#' 
#' @param number_of_stations Numerical input. Number of stations used as closest weather stations to
#' the site of interest. Default option is 25
#' 
#' @param path_zip_prec Character string input. Location and name of the zip file downloaded and
#' containing the records for precipitation. This input must include the name and extension of the file
#' 
#' @param stations_df A dataframe containing the list of stations for which this function will retrieve
#' precipitation data. It is important that this dataframe be produced by this function under the action 
#' \emph{"info_stations"}. The default is set to \code{NULL}
#' 
#' @examples
#' ##Getting the location of zip files
#' 
#' #path_zip_prec <- "[Your folder]/cr2_prDaily_2018_ghcn.zip"
#'
#' ##Call of the function
#' 
#' #handle_chile_cr2_prec(action = "my_data", begin = 20000101, end = 20101030,
#' #                latitude = -32.8958, longitude = -71.2092,
#' #                number_of_stations = 25,
#' #                path_zip_prec = path_zip_prec, stations_df = NULL)
#' 
#' 
#' @export handle_chile_cr2_prec


handle_chile_cr2_prec <- function(action, begin = 19500101, end = 20180309,
                             latitude, longitude, number_of_stations = 25,
                             path_zip_prec, stations_df = NULL){

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
  
  if (!file.exists(path_zip_prec))
    stop("'path_zip_prec' does not exist. Please provide a valid character string vector with the complete path to the zip files")
  
  if (!is.null(stations_df) & !is.data.frame(stations_df))
    stop("Please provide a data frame in 'stations_df' parameter")



  #Saving the actual work directory
  
  actual_WD <- getwd()
  
  #Setting the working directory to the location of the zip file
  
  pattern_positions <- unlist(stringr::str_locate_all(path_zip_prec, pattern = '/'))
  
  pattern_end <- max(pattern_positions)
  
  folder_path <- substr(path_zip_prec, 1, pattern_end - 1)
  
  setwd(folder_path)
  
  #Getting the names of the files inside the zip document using a dataframe
  
  path <- utils::unzip(path_zip_prec, list = T)
  
  
  #"unziping" precipitation files
  
  utils::unzip(path_zip_prec, exdir = folder_path)
  
  
  #Loading the dataframe that contains the information about the weather stations available for 
  #Tmin records
  
  Stations <- utils::read.table(as.character(path[3, 1]), sep = ",", header = T, quote = "\"")
  
  #Selecting the columns of interest 
  
  Stations <- subset(Stations, select = c("codigo_estacion", "institucion", "fuente", "nombre",
                                                    "altura", "latitud", "longitud"))
  
  #Extracting the cod labels for each weather station to use as column names for loading the
  #precipitation dataframe
  
  Cod_stations <- as.character(Stations$codigo_estacion)
  
  Cod_stations <- paste("Cod_", Cod_stations, sep = "")
  
  #Replacing the original Cod label in the dataframe
  
  Stations$codigo_estacion <- Cod_stations
  
  #Making the column names for loading the precipitation dataframe
  
  Cod_stations <- c("Fecha", Cod_stations)
  
  #Loading the Prec dataframe. This step uses as column names the cod labels generated above
  
  Prec <- utils::read.table(as.character(path[2, 1]), sep = ",", quote = "", skip = 15,
                            blank.lines.skip = F, col.names = Cod_stations,
                            na.strings = c("-9999", "-9999.000"))
  
  Prec$Fecha <-as.Date(Prec$Fecha, format = '%Y-%m-%d')
  
  # Add the column YEARMODA for subsetting
  
  Prec$YEARMODA <- chillR::Date2YEARMODA(Prec$Fecha)
  
  #Selecting only the period of interest
  
  Prec <- Prec[which(Prec$YEARMODA >= begin & Prec$YEARMODA <= end), ]
  
  #Changing to the actual working directory
  
  setwd(actual_WD)
  
  #Setting the location to compute the distance of the weather stations
  
  mypoint <- c(longitude, latitude)
  
  #Adding the distance of each station to the location specified above
  
  Stations["distance"] <- round(sp::spDistsN1(as.matrix(Stations[, c("longitud", "latitud")]),
                                                     mypoint, longlat = T), 2)
  
  #Ordering the dataframe according to the distance
  
  Sorted_Stations <- Stations[order(Stations$distance), ]
  
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
  
  # Remove the temperature columns
  
  daily_data <- dplyr::select(daily_data, -Tmin, -Tmax)
  
  
  # Check if the df with the station list has been provided while calling the function
  
  if (is.data.frame(stations_df) & "Cod_Station" %in% colnames(stations_df) & "Name" %in% colnames(stations_df))
    Sumarized_stations <- stations_df
  
  dfs <- NULL
  
  for (i in seq_along(Sumarized_stations$Cod_Station)) {
    
    daily_data[, "Weather_Station"] <- as.character(Sumarized_stations[i, "Name"])
    
    if (Sumarized_stations[i, "Cod_Station"] %in% colnames(Prec)){
      
      daily_data[, "Precipitation"] <- Prec[, Sumarized_stations[i, "Cod_Station"]]} else {
        
        daily_data[, "Precipitation"] <- as.numeric(NA)}
    
    
    dfs <- c(dfs, list(daily_data))
  }
  
  if(action == "list_data" & is.null(stations_df))
    return(dfs[2 : number_of_stations])
  
  if(action == "list_data" & is.data.frame(stations_df))
    return(dfs[2 : length(stations_df[, 1])])
  
  if(action == "my_data")
    return(dfs[[1]])
  
  #Computing the number of observations for the whole period
  
  N_obs <- NULL
  
  for (i in seq_along(dfs)) {
    
    NobsPrec <- length(which(!is.na(dfs[[i]]["Precipitation"])))
    
    N_obs <- c(N_obs, NobsPrec)}
  
  #Adding the number of observations vector to the dataframe of stations  
  
  Sumarized_stations[, "N_Obs"] <- N_obs
  
  #Calculating the percentage of days with complete data  
  
  Sumarized_stations[, "Perc_days_complete"] <- round((N_obs / nrow(daily_data)) * 100, 2)
  
  if(action == "info_stations")
    return(Sumarized_stations)}



