#'Convert a weahter file downloaded from the Chilean Agromet website to chillR format
#'
#'Convert downloaded weather data into a data frame that makes running other chillR functions easy.
#'
#'@param downloaded_weather_file full path of a weather file downloaded from the Chilean Agromet website
#'(https://www.agromet.cl/datos-historicos) as a csv file ("Datos / Exportar a csv" option).
#'
#'@examples
#'file <- "path:(x)"
#'
#'chile_agromet3chillR(file)

chile_agromet3chillR <- function(downloaded_weather_file){
  
  #loading the original dataframe downloaded from the agromet website
  
    original_data <- read.table(downloaded_weather_file, sep = ",", header = T,
                                 dec = ",", na.strings = "--")
  
  #crating a new data frame to summarize the output
    
    new_data_frame <- data.frame(Date_Hour = as.character(original_data$Fecha.Hora),
                             Year = as.numeric(substr(original_data$Fecha.Hora, 7, 10)),
                             Month = as.numeric(substr(original_data$Fecha.Hora, 4, 5)),
                             Day = as.numeric(substr(original_data$Fecha.Hora, 1, 2)))
  
  #Adding the JDay to the new data frame
    new_data_frame <- make_JDay(new_data_frame)
    
  #Adding the hours to the new data frame
    
    new_data_frame[,"Hour"] <- as.numeric(substr(original_data$Fecha.Hora, 12, 13 ))
  
  #Adding the hourly temperature to the new data frame
    
    new_data_frame[,"Temp"] <- original_data[,which(colnames(original_data) == "Temp..promedio.aire")]
  
  return(new_data_frame)}
