#' Handle historic weather records from stations across Germany
#' 
#' This function access the \href{https://www.dwd.de/EN/climate_environment/cdc/cdc_node.html}{Climate Data Center - CDC} 
#' and allows to:\itemize{\item{1) obtain information of the weather stations from Germany.}
#' \item{2) get weather data for one or more specific places of interest given the station ID.}}
#'    
#' @param action is a character string to decide on 2 modes of action for the function.
#' \emph{"list_stations"} returns a dataframe with the information on close weather stations
#' to the location defined by \code{number_of_stations} and \code{longitude} and \code{latitude} parameters.
#' \emph{"download_weather"} retrieve the records for one or more weather stations defined in the
#' \code{station_ID} parameter.
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
#' @param station_ID is a character vector representing the stations to be used for downloading the data
#' 
#' @param begin is a numeric parameter representing the start date for the period required. This must be
#' specified in YEARMODA format.
#' 
#' @param end is a numeric parameter representing the end date for the period required. This must be
#' specified in YEARMODA format.
#' 
#' @param number_of_stations is a numeric parameter defininf the numbers of stations to select from.
#' 
#' @param add.DATE is a boolean parameter to be passed to \code{\link{make_all_day_table}}
#' 
#' @param quiet is a boolean parameter to be passed to \code{\link[utils:download.file]{download.file}}
#'  
#' @details If \emph{"list_stations"} is used, the function returns a dataframe (9 columns x \code{number_of_stations} rows) containing
#' information such as the name, latitude, longitude, begin, end and distance of the weather stations.
#' \code{latitude} and \code{longitude} must be provided.
#' If \emph{"download_weather"} is chosen, it downloads the weather data from the CDC website. The output is
#' a dataframe (in \code{\link{chillR}} format) containing daily records from the closest
#' weather station. \code{station_ID} must be provided. If \code{station_ID} is longer than one, the function returns a list of dataframes as the one
#' described above.
#' 
#' @examples 
#' 
#' handle_cdc_germany(action = "list_stations", variables = c("Tmin", "Tmax", "Tmean"),
#'                    latitude = 53.5373, longitude = 9.6397, begin = 20000101,
#'                    end = 20101231, number_of_stations = 25)
#' 
#' @export handle_cdc_germany

handle_cdc_germany <- function(action,
                               variables,
                               latitude = NULL,
                               longitude = NULL,
                               station_ID = NULL,
                               begin = 19160101,
                               end = chillR::Date2YEARMODA(Sys.Date()),
                               number_of_stations = 25,
                               add.DATE = FALSE, 
                               quiet = FALSE){
  
  # Checking if action, dates and variables are valid inputs
  
  if (!action %in% c("list_stations", "download_weather")) 
    stop("Please provide a valid action")
  
  if (action == "list_stations"){
    assertthat::assert_that(!is.null(latitude) & !is.null(longitude),
                            msg = "Please provide a valid input for latitude and longitude of the place of interest")
    
    assertthat::assert_that(is.null(station_ID),
                            msg = "Please set 'station_ID' to NULL or set the 'action' to download_weather")}
  
  if (action == "download_weather"){
    assertthat::assert_that(!is.null(station_ID),
                            msg = "Plase provide at least one station_ID to download data from")
    
    assertthat::assert_that(is.null(latitude) & is.null(longitude),
                            msg = "Please set 'latitude' and 'longitude' to NULL or set the 'action' to list_stations")}
  
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
  
  
  # Remove those stations which end the record before the begin or those which start after the end
  # defined in the call of the function 
  
  station_in_period <- stations[-which(stations$End < begin | stations$Begin > end), ]
  
  # If the procedure above results in no station selected just return the sorted list
  
  if (length(station_in_period$Station_name) == 0){
    
    station_in_period <- stations
    warning("No stations selected for the period of interest. The complete list will be provided")}
  
  
  # In case the action is set to list_stations, this will compute the distance to the location of interest.
  # Otherwise, the distance is not important, but I need the dataframe of stations to downloading the data
  
  if (action == "list_stations"){
    
    # Add the distance to the location of interest
    
    myPoint <- c(longitude, latitude)
    
    station_in_period[, "Distance"] <- round(sp::spDistsN1(
      as.matrix(station_in_period[, c("Longitude", "Latitude")]), myPoint, longlat = TRUE), 2) 
    
    # Order the station according to the distance to the point
    
    stations_sorted <- station_in_period[order(station_in_period$Distance), ]
    
    # Return of the function
    
    return(stations_sorted[c(1 : number_of_stations), ])}
  
  
  
  
  # primer dataframe to include the complete period of interest
  
  primer <- data.frame(YEARMODA = c(begin, end),
                       Year = c(as.numeric(substr(begin, 1, 4)), as.numeric(substr(end, 1, 4))),
                       Month = c(as.numeric(substr(begin, 5, 6)), as.numeric(substr(end, 5, 6))),
                       Day = c(as.numeric(substr(begin, 7, 8)), as.numeric(substr(end, 7, 8))),
                       Tmin = as.numeric(NA),
                       Tmax = as.numeric(NA),
                       Tmean = as.numeric(NA))
  
  # Add all rows for the period
  
  primer <- chillR::make_all_day_table(primer, add.DATE = add.DATE)
  
  # Add YEARMODA value for future use in merging dataframes
  
  primer["YEARMODA"] <- paste(substr(primer$DATE, 1, 4), substr(primer$DATE, 6, 7),
                              substr(primer$DATE, 9, 10), sep = "")
  primer["YEARMODA"] <- as.numeric(primer$YEARMODA)
  
  # Remove Tmin, Tmax and Tmean from the primer dataframe for make it clearer
  
  if (add.DATE){
    
    primer <- primer[, c("DATE", "YEARMODA", "Year", "Month", "Day")]} else {
      
      primer <- primer[, c("YEARMODA", "Year", "Month", "Day")]}
  
  
  # Downloading the data. Specify the URL where the data is 
  
  master_URL <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/"
  
  # Create a temporary directory for saving the downloaded data
  
  if(!dir.exists("tempdir")) dir.create("tempdir")
  
  # Depending on the number of elements in the station_ID parameter, this will download the data as a list 
  # or a data frame
  
  if (action == "download_weather") stations_to_download <- as.character(station_ID)
  
  # Variable names in the original dataframe
  
  variables_gers <- c("FM", "FX", "PM", "RSK", "SHK_TAG", "TGK", "TMK", "TNK", "TXK", "UPM", "VPM")
  
  # Get the data for the number of stations used by calling the function
  
  download_weather_list <- list()
  for (i in 1 : length(stations_to_download)) {
    
    # URL of the individual station
    
    URL <- paste(master_URL,
                 "tageswerte_KL_",
                 stations_to_download[i],
                 "_",
                 station_in_period[which(station_in_period$Station_ID == stations_to_download[i]), "Begin"],
                 "_",
                 station_in_period[which(station_in_period$Station_ID == stations_to_download[i]), "End"],
                 "_hist.zip",
                 sep = "")
    
    # Download the zip file
    
    utils::download.file(URL, destfile = "tempdir/data.zip", quiet = quiet)
    
    # Extract, in the temporary directory, just the file containing the data
    
    utils::unzip("tempdir/data.zip", files = paste("produkt_klima_tag_",
                                                   station_in_period[which(station_in_period$Station_ID == stations_to_download[i]), "Begin"],
                                                   "_",
                                                   station_in_period[which(station_in_period$Station_ID == stations_to_download[i]), "End"],
                                                   "_",
                                                   stations_to_download[i],
                                                   ".txt",
                                                   sep = ""),
                 exdir = "tempdir")
    
    # Import such file. It keeps all the columns
    
    data <- utils::read.csv(paste("tempdir/produkt_klima_tag_",
                                  station_in_period[which(station_in_period$Station_ID == stations_to_download[i]), "Begin"],
                                  "_",
                                  station_in_period[which(station_in_period$Station_ID == stations_to_download[i]), "End"],
                                  "_",
                                  stations_to_download[i],
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
    
    data["Station_name"] <- as.character(station_in_period[which(station_in_period$Station_ID == stations_to_download[i]), "Station_name"])
    data["Station_ID"] <- stations_to_download[i]
    
    # Order the data by columns and variables selected by calling the function
    
    if (add.DATE){
      
      data <- data[, c("Station_name", "Station_ID", "DATE", "YEARMODA", "Year", "Month", "Day", variables)]} else {
        
        data <- data[, c("Station_name", "Station_ID", "YEARMODA", "Year", "Month", "Day", variables)]}
    
    
    data <- list(data)
    
    download_weather_list <- c(download_weather_list, data)}
  
  # Remove the temporary directory
  
  unlink("tempdir", recursive = TRUE, force = TRUE)
  
  # Return the downloaded data 
  
  if(length(download_weather_list) == 1)
    return(download_weather_list[[1]]) else return(download_weather_list)}
