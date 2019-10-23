#' Modified Utah model
#'
#' Function to calculate winter chill for deciduos trees according to the Modified Utah model proposed by
#' Linvill (1990). This function is compatible with some 'chillR' functions (i.e tempResponse) to
#' estimate metrics from hourly temperature records.
#'
#' @param HourTemp Vector of hourly temperatures
#' 
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour
#'
#' @references Linvill D. 1990. Calculating chilling hours and chill units from daily maximum and minimum
#' temperature observations. HortScience 25(1): 14-16
#' 
#' @examples 
#' library(chillR) 
#' #Example 1
#' data <- stack_hourly_temps(KA_weather, latitude = 50.62)
#' modified_utah_model(data$hourtemps$Temp, summ = TRUE)
#'
#' #Example 2
#' tempResponse(data, Start_JDay = 345, End_JDay = 58,
#'              models = list(Modified_Units = modified_utah_model))
#'              
#' @export modified_utah_model

modified_utah_model <- function(HourTemp, summ = TRUE){
  
  #Giving a value of zero to the whole record
  
    chilling_weights <- rep(0,length(HourTemp))
  
  #Selecting the hours which fit the condition for chill accumulation (0 < Temp <= 21)
  
    relevant_hours <- which(HourTemp > 0 & HourTemp <= 21)
  
    #Calculating chill values for those hours
    
      chilling_weights[relevant_hours] <- sin((2 *pi * HourTemp[relevant_hours]) / 28)
  
  #For hours above 21 C chill accumulation is reduced in 1 hour
      
    chilling_weights[which(HourTemp > 21)] <- -1
    
  #End of the function
  
    if (summ == TRUE)
      return(cumsum(chilling_weights)) else return(chilling_weights)}
