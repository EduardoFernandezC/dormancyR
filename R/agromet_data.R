#' Example of Chilean Agromet weather data
#' 
#' Data downloaded from the Chilean Agromet network to be used as example in the
#' \code{\link{handle_agromet_chile}} function. This data contain hourly records from 01-05-2019 to 31-08-2019.
#' The datafra has been slightly modified (removed the first 5 rows)
#' 
#' @docType data
#' 
#' @usage data(agromet_weather_data)
#' 
#' @format A data frame with 2961 observations and 11 variables
#' 
#' @keywords datasets
#' 
#' @source \url{https://agrometeorologia.cl/}
#' 
#' @examples 
#' 
#' handle_agromet_chile(agromet_weather_data)
#' 
"agromet_weather_data"