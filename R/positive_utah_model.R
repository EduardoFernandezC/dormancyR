#' Positive Utah model
#'
#' Function to calculate winter chill for deciduos trees according to the Positive Utah model proposed by
#' Linsley-Noakes and Allan (1994). This function is compatible with some 'chillR' functions (i.e tempResponse)
#' to estimate metrics from hourly temperature records.
#'
#' @param HourTemp Vector of hourly temperatures
#' 
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour
#'
#' @references Linsley-Noakes G. and Allan P. 1994. Comparison of two models for the prediction of rest
#' completion in peaches. Sci. Hortic. 59(2): 107-113
#' 
#' @examples 
#' #Example 1
#' library(chillR)
#' data <- stack_hourly_temps(KA_weather, latitude = 50.62)
#' positive_utah_model(data$hourtemps$Temp, summ = TRUE)
#'
#' #Example 2
#' tempResponse(data, Start_JDay = 345, End_JDay = 58,
#'              models = list(Positive_Units = positive_utah_model))
#' 
#' @export positive_utah_model

positive_utah_model <- function(HourTemp, summ = TRUE){
  return(chillR::step_model(HourTemp,
                            df = data.frame(lower = c(-1000, 1.4, 2.4, 9.1, 12.4),
                                            upper = c(1.4, 2.4, 9.1, 12.4, 1000),
                                            weight = c(0,0.5,1,0.5,0)), summ = summ))}