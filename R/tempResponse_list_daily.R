#' Temperature response for data in lists
#' 
#' This function allows to compute metrics from models which use as input daily temperature istead hourly data.
#' It is an extension of the tempResponse_daily function.
#' 
#' @param temperature_list List of temperatures as those produced by temperature_generation from chillR.
#' @param Start_JDay Numeric input representing the initial date for computing the metrics in Julian Day format
#' This is the number of the day within the year.
#' @param End_JDay Numeric input representing the end date for computing the metrics in Julian Day format
#' This is the number of the day within the year.
#' @param models List of functions to compute the metrics. Default is settled to those models used in
#' tempResponse_daily function. This parameter also allows the use of a model specified by the user.
#' @param QControl Boolean parameter to specified if the quality control dataframe is shown.

tempResponse_list_daily <- function (temperature_list, Start_JDay = 1, End_JDay = 366, 
                                     models = list(Rate_of_Chill = rate_of_chill_Chmielewski,
                                                   Chill_Days = chill_days,
                                                   Exponential_Chill = exponential_chill_Tmax,
                                                   Triangula_Chill_Hann = triangular_chill_daily_Hann,
                                                   Triangular_Chill_Lega = triangular_chill_daily_Tmean),
                                     QControl = TRUE) {
  
  if (is.data.frame(temperature_list)) 
    temperature_list <- list(temperature_list)
  
  output <- list()
  for (i in 1:length(temperature_list)) {
    
    if (!("JDay" %in% names(temperature_list[[i]]))) data <- make_JDay(temperature_list[[i]]) else
      data <- temperature_list[[i]]
    
    output[[i]] <- tempResponse_daily(data, Start_JDay = Start_JDay, 
                                      End_JDay = End_JDay, models = models, QControl = QControl)
  }
  names(output) <- names(temperature_list)
  return(output)
}