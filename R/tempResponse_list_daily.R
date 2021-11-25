#' Daily temperature responses for data in lists
#' 
#' This function allows to compute metrics from models which use daily temperature as inputs, rather than hourly data.
#' It is an extension of the \code{\link{tempResponse_daily}} function.
#' 
#' @param temperature_list List of temperatures as those produced by \code{\link[chillR:temperature_generation]{chillR::temperature_generation}}
#' 
#' @param Start_JDay Numeric input representing the initial date for computing the metrics in Julian Day format
#' This is the number of the day within the year
#' 
#' @param End_JDay Numeric input representing the end date for computing the metrics in Julian Day format
#' This is the number of the day within the year
#' 
#' @param models List of functions to compute the metrics. Default is settled to those models used in
#' \code{\link{tempResponse_daily}} function. This parameter also allows the use of a model specified by the user
#' 
#' @param misstolerance is the threshold defined by the user to include seasons having days with missing data.
#' Default is set to 20
#' 
#' @examples 
#' library(chillR)
#' 
#' tempResponse_list_daily(KA_weather, Start_JDay = 345, End_JDay = 58)
#' 
#' @export tempResponse_list_daily

tempResponse_list_daily <- function (temperature_list, Start_JDay = 1, End_JDay = 366, 
                                     models = list(Rate_of_Chill = rate_of_chill,
                                                   Chill_Days = chill_days,
                                                   Exponential_Chill = exponential_chill,
                                                   Triangula_Chill_Haninnen = triangular_chill_1,
                                                   Triangular_Chill_Legave= triangular_chill_1),
                                     misstolerance = 20) {
  
  if (is.data.frame(temperature_list)) 
    temperature_list <- list(temperature_list)
  
  output <- list()
  for (i in 1:length(temperature_list)) {
    
    if (!("JDay" %in% names(temperature_list[[i]]))) data <- chillR::make_JDay(temperature_list[[i]]) else
      data <- temperature_list[[i]]
    
    output[[i]] <- tempResponse_daily(data, Start_JDay = Start_JDay, 
                                      End_JDay = End_JDay, models = models, misstolerance = misstolerance)
  }
  names(output) <- names(temperature_list)
  return(output)
}