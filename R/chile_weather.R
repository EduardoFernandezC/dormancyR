#' Get historical records of extreme temperatures for Chile
#'
#' This function allows to obtain information about weather stations located in Chile as well as
#' a data frame containing historical records of minimum and maximum temperatures from those weather
#' stations. This function works with data downloaded from the website of the Center for Climate and
#' Resilience Research (CR)2 sponsored by the University of Chile. The function works for previously
#' downloaded data in ".zip" format. Data can be downloaded from the following links:
#' <http://www.cr2.cl/download/cr2_tasmindaily_2018_ghcn-zip/?wpdmdl=15125> for minimum temperatures and
#' <http://www.cr2.cl/download/cr2_tasmaxdaily_2018_ghcn-zip/?wpdmdl=15126> for maximum temperatures.
#' Function require both zip files in the same folder.
#'
#' @param data Character string input which define the kind of data wanted. There are three options
#' for this parameter. "info_stations" provides a dataframe with information about a given number
#' of weather stations (set in "Number_of_stations" parameter) located close to the ubication
#' established with "latitude" and "longitude" parameters. "station_list" provides a list of
#' dataframes containing minimum and maximum temperature records from each weather station
#' obtained with the "info_stations" option within the period established in the call of the function.
#' Finally, "my_weather" provides the first dataframe of the previous list and represent the
#' data of the closest weather station to the ubication established.
#' @param Initial_Date Character string input in the form "YYYY-MM-DD". This parameter represents the
#' initial date of the period of interest. If it is not provided, the default is established as
#' "1950-01-01" which correspond to the oldest date posible to use.
#' @param End_Date Character string input in the form "YYYY-MM-DD". This parameter represents the
#' final date of the period of interest. If it is not provided, the default is established as
#' "2017-12-31" which correspond to the earliest date posible to use.
#' @param latitude Numerical input. Latitude of the site of interest in decimal format.
#' @param longitude Numerical input. Longitude of the site of interest in decimal format.
#' @param Number_of_stations Numerical input. Number of stations wanted to use as closest stations to
#' the site of interest. Default option is the value 25.
#' @param path_zip_tmin Character string input. Location of the zip file containing minimum
#' temperatures. This input must include the name and extension of the file.
#' @param path_zip_tmax Character string input. Location of the zip file containing maximum
#' temperatures. This input must include the name and extension of the file.
#'
#' @example
#' #Getting the location of zip files
#' path_zip_tmin<-"[Your folder]\\cr2_tasminDaily_2018_ghcn.zip"
#' path_zip_tmax<-"[Your folder]\\cr2_tasmaxDaily_2018_ghcn.zip"
#'
#' #Call of the function
#' chile_weather(data = "my_weather", Initial_Date = "2000-01-01", End_Date = "2017-12-31",
#'               latitude = -32.8958, longitude = -71.2092, Number_of_stations = 25,
#'               path_zip_tmin = path_zip_tmin, path_zip_tmax = path_zip_tmax)


chile_weather <- function(data, Initial_Date = "1950-01-01", End_Date = "2017-12-31",
                          latitude = latitude, longitude = longitude, Number_of_stations = 25,
                          path_zip_tmin = path_zip_tmin, path_zip_tmax = path_zip_tmax){

  actual_WD <- getwd()
  setwd(paste(substr(path_zip_tmin, 1, nchar(path_zip_tmin)-30)))

  path_tmin <- unzip(path_zip_tmin, list = T)
  path_tmax <- unzip(path_zip_tmax, list = T)

  unzip(path_zip_tmin, exdir = paste(substr(path_zip_tmin, 1, nchar(path_zip_tmin)-30)))
  unzip(path_zip_tmax, exdir = paste(substr(path_zip_tmax, 1, nchar(path_zip_tmax)-30)))

  Stations <- read.table(as.character(path_tmin[3,1]), sep = ",", header = T, quote = "\"")
  Stations <- subset(Stations, select = c(1,2,3,4,5,6,7))
  Cod_stations <-as.character(Cod_stations <- Stations$codigo_estacion)


  Cod_Stations = NULL
  for (i in 1:length(Cod_stations)) {
    tempcod <- paste("Cod", "_", Cod_stations[[i]], sep="")
    Cod_Stations <- c(Cod_Stations, tempcod)
  }

  Stations$codigo_estacion <- Cod_Stations
  Cod_Stations <- c("Fecha", Cod_Stations)

  Tmin <- read.table(as.character(path_tmin[2,1]), sep = ",", quote = "", skip=15, blank.lines.skip = F,
                     col.names = Cod_Stations, na.strings = c("-9999","-9999.000"))
  Tmin$Fecha <-as.Date(Tmin$Fecha)
  Tmin <- subset(Tmin, Fecha >= Initial_Date & Fecha <= End_Date)

  Stations_Max <- read.table(as.character(path_tmax[3,1]), sep = ",", header = T, quote = "\"")
  Stations_Max <- subset(Stations_Max, select = c(1,2,3,4,5,6,7))
  Cod_Stations_Max <- as.character(Cod_Stations_Max <- Stations_Max$codigo_estacion)

  Cod_stations_Max = NULL
  for (i in 1:length(Cod_Stations_Max)) {
    tempcod <- paste("Cod", "_", Cod_Stations_Max[[i]], sep="")
    Cod_stations_Max <- c(Cod_stations_Max, tempcod)
  }

  Cod_stations_Max <- c("Fecha", Cod_stations_Max)

  Tmax <- read.table(as.character(path_tmax[2,1]), sep = ",", quote = "", skip=15, blank.lines.skip = F,
                     col.names = Cod_stations_Max, na.strings = c("-9999","-9999.000"))
  Tmax$Fecha <- as.Date(Tmax$Fecha)
  Tmax <- subset(Tmax, Fecha >= Initial_Date & Fecha <= End_Date)

  setwd(actual_WD)

  mypoint <- c(longitude, latitude)
  Stations[,"distance"] <- round(sp::spDistsN1(as.matrix(Stations[,c("longitud","latitud")]), mypoint,
                                             longlat = T), 2)
  Sorted_Stations <- Stations[order(Stations$distance),]
  Sumarized_stations <- Sorted_Stations[c(1:Number_of_stations),]
  colnames(Sumarized_stations) <- c("Cod_Station","Institution","Source","Name","Altitude","Latitude",
                                    "Longitude","Distance")

  primer_data <- data.frame(Year = c(as.numeric(substr(Initial_Date, 1, 4)),
                                     as.numeric(substr(End_Date,1,4))),
                            Month = c(as.numeric(substr(Initial_Date, 6, 7)),
                                      as.numeric(substr(End_Date, 6, 7))),
                            Day = c(as.numeric(substr(Initial_Date, 9, 10)),
                                    as.numeric(substr(End_Date, 9, 10))),
                            JDay = as.numeric(NA),
                            Tmin = as.numeric(NA),
                            Tmax = as.numeric(NA))


  daily_data <- chillR::make_all_day_table(primer_data, add.DATE = FALSE)
  daily_data <- chillR::make_JDay(daily_data)

  dfs <- NULL
  for (i in 1:length(Sumarized_stations$Cod_Station)) {
    daily_data[,"Tmin"]<-Tmin[,Sumarized_stations[i,1]]
    daily_data[,"Tmax"]<-Tmax[,Sumarized_stations[i,1]]
    df<-list(list(data = daily_data, Weather_Station = Sumarized_stations[i, 4]))
    dfs<-c(dfs, df)
  }

  if(data == "station_list")
    return(dfs)

  N_obs<-NULL
  for (i in 1:length(dfs)) {
    if (table(is.na(dfs[[i]][["data"]][,c("Tmin","Tmax")]))[[1]] != length(dfs[[i]][["data"]][,1])*2) {
      NobsTemp <- table(is.na(dfs[[i]][["data"]][,c("Tmin","Tmax")]))[[1]]/2
    } else {
      NobsTemp <- length(dfs[[i]][["data"]][,1])*2
    }

    N_obs<-c(N_obs, NobsTemp)
  }

  Sumarized_stations[,"N_Obs"] <- N_obs
  Sumarized_stations[,"Porc_Completo"] <- round((N_obs / length(dfs[[1]][["data"]][,1])) * 100, 2)

  if(data == "info_stations")
    return(Sumarized_stations)

  my_weather <- dfs[[1]]
  if(data == "my_weather")
    return(my_weather)
}
