#' Chilling units (Harrington, 2010)
#'
#' Function to calculate winter chill for deciduous trees according to the Chilling units model proposed by
#' Harrington (2010). This function is compatible with some 'chillR' functions (i.e tempResponse) to
#' estimate metrics from hourly temperature records.
#'
#' @param HourTemp Vector of hourly temperatures
#' 
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour
#'
#' @references Harrington C., Gould P. and St.Clair J. (2010). Modeling the effects of winter environment on
#' dormancy release of Douglas-fir. For. Ecol. Manage. 259(4): 798-808
#' 
#' @examples
#' library(chillR) 
#' 
#' #Example 1
#' data <- stack_hourly_temps(KA_weather, latitude = 50.62)
#' chilling_units(data$hourtemps$Temp, summ = TRUE)
#' 
#' #Example 2
#' tempResponse(data, Start_JDay = 345, End_JDay = 58,
#' models = list(Chill_Harrington = chilling_units))
#' 
#' @export chilling_units

chilling_units <- function(HourTemp, summ = TRUE){

  #Giving a value of zero to all days for then changing according to each situation.
  #This is 0 for hours in which Temp < -4.66 or Temp > 16 C
  
    chilling_weights <- rep(0, length(HourTemp))
  
  #Selecting the hours which fit the condition: -4.66 < Temp < 16
    
    relevant_hours <- which(HourTemp >= -4.66 & HourTemp <= 16)
  
    #Computing chill for such condition
    
      chilling_weights[relevant_hours] <- 3.13 * (((HourTemp[relevant_hours] + 4.66) / 10.93) ^ 2.10) * 
        exp(1)^ - ((HourTemp[relevant_hours] + 4.66) / 10.93) ^ 3.10
  
  #Correction factor for those chill values above 1
      
    chilling_weights[chilling_weights > 1] <- 1
  
  #End of the function
    
    if (summ == TRUE)
    return(cumsum(chilling_weights)) else return(chilling_weights)
}
