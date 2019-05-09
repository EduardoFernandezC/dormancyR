#' Handle historic weather records from stations across Germany
#' 
#' It allows to: 1) obtain information of the weather stations from Germany; 2) get weather data for a
#' place of interest given the coordinates; and 3) get weather data from several weather stations
#' close to a specific location given its coordinates. The functions take data from the "Climatic Data
#' Center (CDC - <https://cdc.dwd.de/portal/>) of Germany.
#' 
#' @param action Character parameter to decide on 3 options. Options are "info_stations", "my_data" and
#' "list_data". "info_stations" returns a dataframe with the information on close weather stations
#' to a location. "my_data" downloads the records for the closest weather station. "list_data" downloads
#' the records for several weather stations which are close to the location of interest.
#' 
#' @param latitude Numeric parameter. The latitude (in decimal degrees) of the location of interest.
#' 
#' @param longitude Numeric parameter. The longitude (in decimal degrees) of the location of interest.
#' 
#' @param begin Numeric parameter. The start date of the period by which the record is required. It has
#' to be specified in YEARMODA format.
#' 
#' @param end Numeric parameter. The end date of the period by which the record is required. It has
#' to be specified in YEARMODA format.
#' 
#' @param number_of_stations Numeric parameter. The numbers of stations to select from the point of 
#' interest.
#' 
#' @param complete_list Boolean parameter. If "my_data" option has been used this allows to skip the first
#' weather station when using the "list_data" option. It avoids include a repetead dataframe for the
#' first weather station.
#'  
#' @details 
#' If "info_stations" is used, the function returns a dataframe (9 columns x number_of_stations) containing
#' information such as the name, latitude, longitude, begin, end and distance of the weather stations.
#' If "my_data" is chosen, it downloads the weather data from the CDC website. The output is a dataframe
#' (in chillR format) containing minimum, maximum and mean daily records from the closest weather station.
#' If "list_data" option is used, the function returns a list of dataframes as that described above.
#' The length of the list is equal to the number of stations or to the number_of_stations minus 1 if 
#' complete_list = FALSE.
#' 
#' @examples 
#'     
#' handle_CDC(action = "info_stations", latitude = 53.5373, longitude = 9.6397, begin = 20000101,
#'            end = 20101231, number_of_stations = 25, complete_list = FALSE)
#'            
#' @export handle_CDC

handle_CDC <- function(action, latitude, longitude, begin = 19160101,
                       end = chillR::Date2YEARMODA(Sys.Date()), number_of_stations = 25,
                       complete_list = FALSE){
  
  # Checking if action and dates are valid inputs
  
  if (!action %in% c("info_stations", "my_data", "list_data")) 
    stop("Please provide a valid action")
  
  if (nchar(begin) != 8 | nchar(end) != 8)
    stop("Invalid date for begin or end parameters. Please introduce a date in format YEARMODA")
  
  # Get the information of the weather stations.
  
  stations <- utils::read.csv("https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/KL_Tageswerte_Beschreibung_Stationen.txt",
                              skip = 2, header = F, colClasses = "character")[,1]
  
  # Delete the extra white spaces at the end of the characters
  
  stations <- trimws(stations)
  
  # Identify the mistakes while importing the data. Those rows starting with letters instead of numbers
  # are the correct rows. This is because the information of the state was passed to the row n+1
  
  wrong_rows <- which(substr(stations, 2, 2) %in% c(LETTERS, letters))
  
  # Take back such information to the row n
  
  stations[wrong_rows - 1] <- paste(stations[wrong_rows - 1], stations[wrong_rows], sep = "")
  
  # Remove the extra values due to these mistakes
  
  stations <- stations[-wrong_rows]
  
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
  
  
  if (action == "info_stations")
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
  
  
  # Downloading the data. Specify the URL where the data is 
  
  master_URL <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/"
  
  # Create a temporary directory for saving the downloaded data
  
  if(!dir.exists("tempdir")) dir.create("tempdir")
  
  # Depending on the action selected when calling the function this allows to get the data for just 1
  # (my_data) or the total number of stations selected (list_data). It saves a bit of time when the 
  # my_data option is used
  
  if (action == "my_data") number_of_stations <- 1 
  if (action == "list_data") number_of_stations <- number_of_stations
  
  # Get the data for the number of stations used by calling the function
  
  list_data <- list()
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
    
    # Import such file. It keeps the important columns only
    data <- utils::read.csv(paste("tempdir/produkt_klima_tag_", station_in_period[i, "Begin"], "_",
                                  station_in_period[i, "End"], "_", station_in_period[i, "Station_ID"],
                                  ".txt", sep = ""), sep = ";")[c("STATIONS_ID", "MESS_DATUM", "TNK", "TXK", "TMK")]
    
    # While we have focused on temperature data so far, consider adding more to the dataset. At least rainfall
    # would be very useful. You can also make this optional by adding a corresponding parameter to the call.
    
    colnames(data) <- c("Station_ID", "YEARMODA", "Tmin", "Tmax", "Tmean")
    
    # Add NA for missing values
    data[which(data$Tmax == -999.0), "Tmax"] <- NA
    data[which(data$Tmin == -999.0), "Tmin"] <- NA
    data[which(data$Tmean == -999.0), "Tmean"] <- NA
    
    # Merge weather data with primer data and remove missing columns
    data <- dplyr::left_join(primer, data, by = "YEARMODA")
    data <- dplyr::select(data, -c("Tmin.x", "Tmax.x", "Tmean.x"))
    
    # Add station name and station ID to the data
    data["Station_name"] <- as.character(station_in_period[i, "Station_name"])
    data["Station_ID"] <- as.character(station_in_period[i, "Station_ID"])
    
    colnames(data)[c(7, 8, 9)] <- c("Tmin", "Tmax", "Tmean")
    
    # Order the data by columns
    data <- data[, c(10, 6, 1, 2, 3, 4, 5, 7, 8, 9)]
    
    data <- list(data)
    
    list_data <- c(list_data, data)}
  
  # Remove the temporary directory
  
  unlink("tempdir", recursive = TRUE, force = TRUE)
  
  if(action == "my_data")
    return(list_data[[1]])
  
  if(action == "list_data" & complete_list == TRUE)
    return(list_data) else return(list_data[-1])}