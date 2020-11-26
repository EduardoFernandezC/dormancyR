#' Handle historic weather records from stations across Germany
#' 
#' This function access the \href{https://www.dwd.de/EN/climate_environment/cdc/cdc_node.html}{Climate Data Center - CDC} 
#' and allows to:\itemize{\item{1) obtain information of the weather stations from Germany.}
#' \item{2) get weather data for a specific place of interest given the coordinates.}
#' \item{3) get weather data from several weather stations located close to a specific location
#'  given its coordinates.}}
#'    
#' @param action is a character string to decide on 3 modes of action for the function.
#' \emph{"list_stations"} returns a dataframe with the information on close weather stations
#' to the location defined by \code{number_of_stations} and \code{longitude} and \code{latitude} parameters.
#' \emph{"download_weather"} retrieve the records for the closest weather station to the defined location.
#' \emph{"download_weather_list"} downloads the records for several weather stations (according to
#' \code{number_of_stations}) which are close to the location of interest.
#' 
#' @param variables is a character vector representing the variables required. For now, the function can return the
#' wind speed (mean - \emph{"Wind_speed"} and maximum - \emph{"Wind_speed_max"}); the atmospheric pressure
#' (\emph{"ATM_pressure"}); the rainfall (\emph{"Rainfall"}); the precipitation as snow (\emph{"Snow"}), the minimum 
#' temperature 5 cm above the ground (\emph{"Tmin_5cm"}); the air temperature 2 m above ground (minimum - \emph{"Tmin"},
#' mean - \emph{"Tmean"}, and maximum - \emph{"Tmax"}); the relative humidity (\emph{"RH"}); and the vapour pressure deficit
#' (\emph{"VPD"}).
#' 
#' @param latitude is a numeric parameter defining the latitude (in decimal degrees) of the location of interest.
#' 
#' @param longitude is a numeric parameter defining the longitude (in decimal degrees) of the location of interest.
#' 
#' @param begin is a numeric parameter representing the start date for the period required. This must be
#' specified in YEARMODA format.
#' 
#' @param end is a numeric parameter representing the end date for the period required. This must be
#' specified in YEARMODA format.
#' 
#' @param number_of_stations is a numeric parameter defininf the numbers of stations to select from.
#' 
#' @param complete_list is a boolean parameter. If \emph{"download_weather"} option has been used, this allows to skip the first
#' weather station when using the \emph{"download_weather_list"} option. It avoids include a repeated dataframe for the
#' first weather station
#'  
#' @details If \emph{"list_stations"} is used, the function returns a dataframe (9 columns x \code{number_of_stations} rows) containing
#' information such as the name, latitude, longitude, begin, end and distance of the weather stations.
#' If \emph{"download_weather"} is chosen, it downloads the weather data from the CDC website. The output is
#' a dataframe (in \code{\link{chillR}} format) containing daily records from the closest
#' weather station. If \emph{"download_weather_list"} option is used, the function returns a list of dataframes as the one
#' described above. The length of the list is equal to the \code{number_of_stations} or to the \code{number_of_stations}
#' minus 1 if \code{complete_list = FALSE}.
#' 
#' @examples 
#' 
#' handle_cdc_germany(action = "list_stations", variables = c("Tmin", "Tmax", "Tmean"),
#'                    latitude = 53.5373, longitude = 9.6397, begin = 20000101,
#'                    end = 20101231, number_of_stations = 25, complete_list = FALSE)
#' 
#' @export handle_cdc_germany

handle_cdc_germany <- function(action, variables,  latitude, longitude,
                               begin = 19160101, end = chillR::Date2YEARMODA(Sys.Date()),
                               number_of_stations = 25,
                               complete_list = FALSE){
  
  # Checking if action, dates and variables are valid inputs
  
  if (!action %in% c("list_stations", "download_weather", "download_weather_list")) 
    stop("Please provide a valid action")
  
  if (nchar(begin) != 8 | nchar(end) != 8)
    stop("Invalid date for begin or end parameters. Please introduce a date in format YEARMODA")
  
  if(length(which(!variables %in% c("Wind_speed", "Wind_speed_max", "ATM_pressure", "Rainfall", "Snow",
                                    "Tmin_5cm", "Tmean", "Tmin", "Tmax", "RH", "VPD"))) != 0)
    stop("One or more invalid variable(s) selected. Please provide variables such as:
    Wind_speed, Wind_speed_max, ATM_pressure, Rainfall, Snow, Tmin_5cm, Tmean, Tmin, Tmax,
    RH, VPD.")
  
  # Get the information of the weather stations.
  
  stations <- utils::read.csv("https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/KL_Tageswerte_Beschreibung_Stationen.txt",
                              skip = 2, header = F, colClasses = "character")[,1]
  
  # Delete the extra white spaces at the end of the characters
  
  stations <- trimws(stations)
  
  # Identify the mistakes while importing the data. Those rows starting with numbers instead of letters
  # are the correct rows. This is because the information of the state was passed to the row n+1
  
  wrong_rows <- which(substr(stations, 2, 2) %in% c(LETTERS, letters))
  
  # Take back such information to the row n
  
  stations[wrong_rows - 1] <- paste(stations[wrong_rows - 1], stations[wrong_rows], sep = "")
  
  # Remove the extra values due to these mistakes
  
  stations <- stations[-wrong_rows]
  
  # Remove german characters 
  
  stations <- stringi::stri_enc_toascii(stations)
  
  stations <- stringr::str_replace_all(stations, pattern = "\032", replacement = "ue")
  
  # Make a dataframe of the information about the stations in Germany
  
  stations <- data.frame(Station_name = as.character(trimws(substr(stations, 62, 100))),
                         Region = as.character(trimws(substr(stations, 101, nchar(stations)))),
                         Station_ID = as.character(trimws(substr(stations, 1, 5))),
                         Altitude = as.numeric(trimws(substr(stations, 24, 39))),
                         Latitude = as.numeric(trimws(substr(stations, 40, 51))),
                         Longitude = as.numeric(trimws(substr(stations, 51, 61))))
  
  stations$Station_ID <- as.character(stations$Station_ID)
  
  # As the total number of zip files (containing data) is different from the number given in the
  # station overview list, I had get retrieve the list of zip files in order to identify those
  # stations that have data
  
  zip_files <- utils::read.csv("https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/",
                               skip = 7, colClasses = "character", header = FALSE)
  
  # Remove extra rows by reading an html file as csv
  
  zip_files <- zip_files[c(1 : (length(zip_files$V1) - 2)),]
  
  # Make a dataframe of the information of the zip files to facilitate the downloading process
  
  zip_files <- data.frame(Station_ID = as.character(substr(zip_files, 23, 27)),
                          Begin = as.character(substr(zip_files, 29, 36)),
                          End = as.character(substr(zip_files, 38, 45)))
  
  
  zip_files$Station_ID <- as.character(zip_files$Station_ID)
  zip_files$Begin <- as.character(zip_files$Begin)
  zip_files$End <- as.character(zip_files$End)
  
  # Merge the dataframes on the information of the stations and the dataframe on the zip file information
  # by Station_ID. This keep the rows in both dataframes.
  
  stations <- dplyr::inner_join(stations, zip_files, by = "Station_ID")
  
  # Add the distance to the location of interest
  
  myPoint <- c(longitude, latitude)
  
  stations[, "Distance"] <- round(sp::spDistsN1(
    as.matrix(stations[, c("Longitude", "Latitude")]), myPoint, longlat = TRUE), 2) 
  
  # Order the station according to the distance to the point
  
  stations_sorted <- stations[order(stations$Distance), ]
  
  # Remove those stations which end the record before the begin or those which start after the end
  # defined in the call of the function 
  
  station_in_period <- stations_sorted[-which(stations_sorted$End < begin | stations_sorted$Begin > end),]
  
  # If the procedure above results in no station selected just return the sorted list
  
  if (length(station_in_period$Station_name) == 0) station_in_period <- stations_sorted
  
  
  if (action == "list_stations")
    return(station_in_period[c(1: number_of_stations),])
  
  
  # primer dataframe to include the complete period of interest
  
  primer <- data.frame(YEARMODA = c(begin, end),
                       Year = c(as.numeric(substr(begin, 1, 4)), as.numeric(substr(end, 1, 4))),
                       Month = c(as.numeric(substr(begin, 5, 6)), as.numeric(substr(end, 5, 6))),
                       Day = c(as.numeric(substr(begin, 7, 8)), as.numeric(substr(end, 7, 8))),
                       Tmin = as.numeric(NA),
                       Tmax = as.numeric(NA),
                       Tmean = as.numeric(NA))
  
  # Add all rows for the period
  
  primer <- chillR::make_all_day_table(primer)
  
  # Add YEARMODA value for future use in merging dataframes
  
  primer["YEARMODA"] <- paste(substr(primer$DATE, 1, 4), substr(primer$DATE, 6, 7),
                              substr(primer$DATE, 9, 10), sep = "")
  primer["YEARMODA"] <- as.numeric(primer$YEARMODA)
  
  # Remove Tmin, Tmax and Tmean from the primer dataframe for make it clearer
  
  primer <- primer[, c("DATE", "YEARMODA", "Year", "Month", "Day")]
  
  
  # Downloading the data. Specify the URL where the data is 
  
  master_URL <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/"
  
  # Create a temporary directory for saving the downloaded data
  
  if(!dir.exists("tempdir")) dir.create("tempdir")
  
  # Depending on the action selected when calling the function this allows to get the data for just 1
  # (download_weather) or the total number of stations selected (download_weather_list). It saves a bit of time when the 
  # download_weather option is used
  
  if (action == "download_weather") number_of_stations <- 1 
  if (action == "download_weather_list") number_of_stations <- number_of_stations
  
  # Variable names in the original dataframe
  
  variables_gers <- c("FM", "FX", "PM", "RSK", "SHK_TAG", "TGK", "TMK", "TNK", "TXK", "UPM", "VPM")
  
  # Get the data for the number of stations used by calling the function
  
  download_weather_list <- list()
  for (i in 1 : number_of_stations) {
    
    # URL of the individual station
    
    URL <- paste(master_URL, "tageswerte_KL_", station_in_period[i, "Station_ID"], "_",
                 station_in_period[i, "Begin"], "_", station_in_period[i, "End"], "_hist.zip", sep = "")
    
    # Download the zip file
    
    utils::download.file(URL, destfile = "tempdir/data.zip")
    
    # Extract, in the temporary directory, just the file containing the data
    
    utils::unzip("tempdir/data.zip", files = paste("produkt_klima_tag_", station_in_period[i, "Begin"], "_",
                                                   station_in_period[i, "End"], "_",
                                                   station_in_period[i, "Station_ID"], ".txt", sep = ""),
                 exdir = "tempdir")
    
    # Import such file. It keeps all the columns
    
    data <- utils::read.csv(paste("tempdir/produkt_klima_tag_", station_in_period[i, "Begin"], "_",
                                  station_in_period[i, "End"], "_", station_in_period[i, "Station_ID"],
                                  ".txt", sep = ""), sep = ";",
                            colClasses = c("numeric", "numeric", rep("character", length(variables_gers))), 
                            na.strings = "-999")[c("STATIONS_ID", "MESS_DATUM", variables_gers)]
    
    
    # In the step above, data were imported as character to remove missing values (-999) automatically.
    # This step set all the variables as numeric vectors
    
    for (variables_ger in variables_gers){
      
      data[ ,variables_ger] <- as.numeric(data[, variables_ger])}
    
    # Add colnames in a more or less understandable format
    
    colnames(data) <- c("Station_ID", "YEARMODA", "Wind_speed", "Wind_speed_max", "ATM_pressure",
                        "Rainfall", "Snow", "Tmin_5cm", "Tmean", "Tmin", "Tmax", "RH", "VPD")
    
    # Select just the variables required when calling the function
    
    data <- dplyr::select(data, c("Station_ID", "YEARMODA", variables))
    
    # Merge weather data with primer data by YEARMODA column 
    
    data <- dplyr::left_join(primer, data, by = "YEARMODA")
    
    # Add station name and station ID to the data
    
    data["Station_name"] <- as.character(station_in_period[i, "Station_name"])
    data["Station_ID"] <- as.character(station_in_period[i, "Station_ID"])
    
    # Order the data by columns and variables selected by calling the function
    
    data <- data[, c("Station_name", "Station_ID", "DATE", "YEARMODA", "Year", "Month", "Day", variables)]
    
    data <- list(data)
    
    download_weather_list <- c(download_weather_list, data)}
  
  # Remove the temporary directory
  
  unlink("tempdir", recursive = TRUE, force = TRUE)
  
  if(action == "download_weather")
    return(download_weather_list[[1]])
  
  if(action == "download_weather_list" & complete_list == TRUE)
    return(download_weather_list) else return(download_weather_list[-1])}
