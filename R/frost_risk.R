#' Compute the number of hours below a given threshold
#'
#' This function computes the numbers of hours below a given threshold. This function allows to attempt the
#' potential risk of spring frost events for deciduous trees. This function is compatible with some \code{\link{chillR}} functions
#' (i.e \code{\link[chillR:tempResponse]{tempResponse}}) to estimate metrics from hourly temperature records.
#'
#' @param HourTemp Vector of hourly temperatures
#' 
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour
#' 
#' @param threshold Numeric vector below which it is considered a potential frost event. Default to 0 celsius degree
#' 
#' @note By using this function as model input for \code{\link[chillR:tempResponse]{chillR::tempResponse}}, the threshold parameter is set to
#' the default, 0.
#' 
#' @examples
#' library(chillR) 
#' 
#' data <- stack_hourly_temps(KA_weather, latitude = 50.62)
#' frost_risk(data$hourtemps$Temp, summ = TRUE, threshold = -1)
#' 
#' @export frost_risk

frost_risk <- function(HourTemp, summ = TRUE, threshold = 0){
  
  #Selecting those hours below the threshold
  
    frost_risk_range <- which(HourTemp < threshold)
  
  #Giving a value of zero tho the whole record for then computing the frost risk for those relevant hours
    
    frost_risk_weights <- rep(0, length(HourTemp))
  
  #Frost event assignation
    
    frost_risk_weights[frost_risk_range] <- 1
  
  #End of the function
    
    if (summ == TRUE)
    return(cumsum(frost_risk_weights)) else return(frost_risk_weights)
}
