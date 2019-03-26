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
#' @param output Character string input which define the kind of data wanted. There are three options
#' for this parameter. "info_stations" provides a dataframe with information about a given number
#' of weather stations (set in "Number_of_stations" parameter) located close to the location
#' established with "latitude" and "longitude" parameters. "station_list_data" provides a list of
#' dataframes containing minimum and maximum temperature records from each weather station
#' obtained with the "info_stations" option within the period established in the call of the function.
#' Finally, "my_data" provides the first dataframe of the previous list and represent the
#' data of the closest weather station to the ubication established.
#' @param Initial_Date Character string input in the form "YYYY-MM-DD". This parameter represents the
#' initial date of the period of interest. If it is not provided, the default is established as
#' "1950-01-01" which correspond to the oldest date posible to use.
#' @param End_Date Character string input in the form "YYYY-MM-DD". This parameter represents the
#' final date of the period of interest. If it is not provided, the default is established as
#' "2017-12-31" which corresponds to the earliest posible date that can be used for the assesment.
#' @param latitude Numerical input. Latitude of the site of interest in decimal format.
#' @param longitude Numerical input. Longitude of the site of interest in decimal format.
#' @param Number_of_stations Numerical input. Number of stations wanted to use as closest stations to
#' the site of interest. Default option is the value 25.
#' @param path_zip_tmin Character string input. Location of the zip file containing minimum
#' temperatures. This input must include the name and extension of the file.
#' @param path_zip_tmax Character string input. Location of the zip file containing maximum
#' temperatures. This input must include the name and extension of the file.
#'
#' @examples
#' #Getting the location of zip files
#' #path_zip_tmin<-"[Your folder]\\cr2_tasminDaily_2018_ghcn.zip"
#' #path_zip_tmax<-"[Your folder]\\cr2_tasmaxDaily_2018_ghcn.zip"
#'
#' #Call of the function
#' #chile_weather(output = "my_data", Initial_Date = "2000-01-01", End_Date = "2017-12-31",
#' #               latitude = -32.8958, longitude = -71.2092, Number_of_stations = 25,
#' #               path_zip_tmin = path_zip_tmin, path_zip_tmax = path_zip_tmax)


chile_weather <- function(output, Initial_Date = "1950-01-01", End_Date = "2017-12-31",
                          latitude = latitude, longitude = longitude, Number_of_stations = 25,
                          path_zip_tmin = path_zip_tmin, path_zip_tmax = path_zip_tmax){

  #Saving the actual work directory
  
    actual_WD <- getwd()
  
  #Setting the work directory to the location of the zip files
    
    setwd(paste(substr(path_zip_tmin, 1, nchar(path_zip_tmin)-30)))
  
  #Getting the names of the files inside the zip document using a dataframe
    
    path_tmin <- utils::unzip(path_zip_tmin, list = T)
    path_tmax <- utils::unzip(path_zip_tmax, list = T)
  
  #"unziping" both Tmin and Tmax files
    
    unzip(path_zip_tmin, exdir = paste(substr(path_zip_tmin, 1, nchar(path_zip_tmin)-30)))
    unzip(path_zip_tmax, exdir = paste(substr(path_zip_tmax, 1, nchar(path_zip_tmax)-30)))
  
  #Loading the dataframe that contains the information about the weather stations available for 
  #Tmin records
    
    Stations_Tmin <- read.table(as.character(path_tmin[3,1]), sep = ",", header = T, quote = "\"")
  
  #Selecting the columns of interest 
    
    Stations_Tmin <- subset(Stations_Tmin, select = c("codigo_estacion","institucion","fuente","nombre",
                                            "altura","latitud","longitud"))
  
  #Extracting the cod labels for each weather station to use as column names for loading the Tmin
  #dataframe
    
    Cod_stations_Tmin <- as.character(Cod_stations_Tmin <- Stations_Tmin$codigo_estacion)
    Cod_stations_Tmin <- paste("Cod_", Cod_stations_Tmin, sep = "")
    
    #Replacing the original Cod label in the dataframe
      
      Stations_Tmin$codigo_estacion <- Cod_stations_Tmin
    
    #Making the column names for loading the Tmin dataframe
      
      Cod_stations_Tmin <- c("Fecha", Cod_stations_Tmin)
    
  #Loading the Tmin dataframe. This step uses as column names the cod labels mentioned above.
  
    Tmin <- read.table(as.character(path_tmin[2,1]), sep = ",", quote = "", skip=15, blank.lines.skip = F,
                     col.names = Cod_stations_Tmin, na.strings = c("-9999","-9999.000"))
    Tmin$Fecha <-as.Date(Tmin$Fecha)
  
  #Selecting only the period of interest
    
    Tmin <- subset(Tmin, Fecha >= Initial_Date & Fecha <= End_Date)
    
  #Loading the dataframe that contains the information about the weather stations available for 
  #Tmax records 
  
    Stations_Tmax <- read.table(as.character(path_tmax[3,1]), sep = ",", header = T, quote = "\"")
    
  #Selecting the columns of interest
    
    Stations_Tmax <- subset(Stations_Tmax, select = c("codigo_estacion","institucion","fuente","nombre",
                                                    "altura","latitud","longitud"))
    
  #Extracting the cod labels for each weather station to use as column names for loading the Tmax
  #dataframe
    
    Cod_stations_Tmax <- as.character(Cod_stations_Tmax <- Stations_Tmax$codigo_estacion)
    Cod_stations_Tmax <- paste("Cod_", Cod_stations_Tmax, sep = "")
    
  #Replacing the original Cod label in the dataframe
    
    Stations_Tmax$codigo_estacion <- Cod_stations_Tmax
    
  #Making the column names for loading the Tmin dataframe
    
    Cod_stations_Tmax <- c("Fecha", Cod_stations_Tmax)
    
  #Loading the Tmin dataframe. This step uses as column names the cod labels mentioned above.
    
    Tmax <- read.table(as.character(path_tmax[2,1]), sep = ",", quote = "", skip=15, blank.lines.skip = F,
                     col.names = Cod_stations_Tmax, na.strings = c("-9999","-9999.000"))
    Tmax$Fecha <- as.Date(Tmax$Fecha)
  
  #Selecting the period of interest
  
    Tmax <- subset(Tmax, Fecha >= Initial_Date & Fecha <= End_Date)
  
  #Changing to the actual working directory
    setwd(actual_WD)
  
  #Setting the location to compute the distance of the weather stations
 
     mypoint <- c(longitude, latitude)
  
  #Adding the distance of each station to the location specified above
     
    Stations_Tmin[,"distance"] <- round(sp::spDistsN1(as.matrix(Stations_Tmin[,c("longitud","latitud")]),
                                                    mypoint, longlat = T), 2)
    
  #Ordering the dataframe according to the distance
    
    Sorted_Stations <- Stations_Tmin[order(Stations_Tmin$distance),]
  
  #Selecting the relevant weather stations according to the number of stations
    
    Sumarized_stations <- Sorted_Stations[c(1:Number_of_stations),]
    colnames(Sumarized_stations) <- c("Cod_Station","Institution","Source","Name","Altitude","Latitude",
                                    "Longitude","Distance")
  
  #Creating a primer data for the period of interest
    
    primer_data <- data.frame(Weather_Station = NA,
                            Year = c(as.numeric(substr(Initial_Date, 1, 4)),
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
  
  #Merging Tmin y Tmax according to the code of the station for the total number of stations setted
    
    dfs <- NULL
    for (i in 1:length(Sumarized_stations$Cod_Station)) {
      daily_data[,"Weather_Station"] <- as.character(Sumarized_stations[i,4])
      daily_data[,"Tmin"]<-Tmin[,Sumarized_stations[i,1]]
      daily_data[,"Tmax"]<-Tmax[,Sumarized_stations[i,1]]
      dfs<-c(dfs, list(daily_data))
    }
    rm(daily_data)
  
    if(output == "station_list_data")
      return(dfs[2:Number_of_stations])
  
    if(output == "my_data")
      return(dfs[[1]])
  
  #Computing the number of observations for the whole period. This value includes Tmin + Tmax observations
  
    N_obs<-NULL
    for (i in 1:length(dfs)) {
      if (table(is.na(dfs[[i]][,c("Tmin","Tmax")]))[[1]] != length(dfs[[i]][,1])*2) {
        NobsTemp <- table(is.na(dfs[[i]][,c("Tmin","Tmax")]))[[1]]
      } else {
        NobsTemp <- length(dfs[[i]][,1])*2
      }
    
      N_obs<-c(N_obs, NobsTemp)
    }
  
  #Adding the number of observations vector to the dataframe of stations  
    
    Sumarized_stations[,"N_Obs"] <- N_obs
  
  #Calculating the percentage of days with complete data  
    
    Sumarized_stations[,"Perc_days_complete"] <- round((N_obs / length(dfs[[1]][,1])) * 100/2, 2)
  
  if(output == "info_stations")
    return(Sumarized_stations)
  
}