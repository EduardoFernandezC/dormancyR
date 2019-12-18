#'Convert a weather file downloaded from the Chilean Agromet website to \code{\link{chillR}} format
#'
#'Convert downloaded weather data into a data frame that makes running other \code{\link{chillR}} functions easy.
#'
#'@param downloaded_weather_file full path of a weather file downloaded from the Chilean Agromet website
#'(\url{https://www.agromet.cl/datos-historicos}) as a csv file (\emph{"Datos/Exportar a csv"} option)
#'
#'@examples
#'data <- Agromet_Weather_data
#'chile_agromet3chillR(data)
#'
#'@export chile_agromet3chillR

chile_agromet3chillR <- function(downloaded_weather_file){
  
  #loading the original dataframe downloaded from the agromet website
  
  if (is.character(downloaded_weather_file))
    
    original_data <- utils::read.table(downloaded_weather_file, sep = ",", header = T,
                                       dec = ",", na.strings = "--") else
                                         
      original_data <- downloaded_weather_file
                                       
  #crating a new data frame to summarize the output
                                       
    new_data_frame <- data.frame(Date_Hour = as.character(original_data$Fecha.Hora),
                                 Year = as.numeric(substr(original_data$Fecha.Hora, 7, 10)),
                                 Month = as.numeric(substr(original_data$Fecha.Hora, 4, 5)),
                                 Day = as.numeric(substr(original_data$Fecha.Hora, 1, 2)))
                                       
  #Adding the JDay to the new data frame
    
    new_data_frame <- chillR::make_JDay(new_data_frame)
                                       
  #Adding the hours to the new data frame
                 
    new_data_frame[,"Hour"] <- as.numeric(substr(original_data$Fecha.Hora, 12, 13 ))
                                       
  #Adding the hourly temperature to the new data frame
                                       
    new_data_frame[,"Temp"] <- original_data[,which(colnames(original_data) == "Temp..promedio.aire")]
                                       
  return(new_data_frame)}
