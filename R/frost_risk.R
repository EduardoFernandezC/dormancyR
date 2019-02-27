#' Compute the number of hours below a given threshold
#'
#' This function computes the numbers of hours below a given threshold. This function allows to attempt the
#' potential risk of spring frost events for deciduous trees. This function is compatible with some 'chillR'
#' functions (i.e tempResponse) to estimate metrics from hourly temperature records.
#'
#' @param HourTemp Vector of hourly temperatures.
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour.
#' @param threshold Numeric vector below which it is considered a potential frost event. Default to 0 celsius degree.
#' @note By using this function as model input for chillR::tempResponse, the threshold parameter is settled to
#' default.
#' 
#' @example 
#' library(chillR) 
#' 
#' data <- stack_hourly_temps(KA_weather, latitude = 50.62)
#' frost_risk(data$hourtemps$Temp, summ = T, threshold = -1)

frost_risk <- function(HourTemp, summ = TRUE, threshold = 0){

  frost_risk_range <- which(HourTemp < threshold)
  frost_risk_weights <- rep(0, length(HourTemp))
  frost_risk_weights[frost_risk_range] <- 1
  if (summ == TRUE)
    return(cumsum(frost_risk_weights))
  else return(frost_risk_weights)
}
