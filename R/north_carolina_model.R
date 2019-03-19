#' North Carolina model
#'
#' Function to calculate winter chill for deciduos trees according to the North Carolina model proposed by
#' Shaltout and Unrath (1983). This function is compatible with some 'chillR' functions (i.e tempResponse) to
#' estimate metrics from hourly temperature records.
#'
#' @param HourTemp Vector of hourly temperatures.
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour.
#'
#' @references Shaltout A. and Unrath C. 1983. Rest completion prediction model for starkrimsom delicious apples.
#' J. Am. Soc. Hort. Sci. 108(6): 957-961
#' 
#' @examples 
#' #Example 1
#' data <- stack_hourly_temps(KA_weather, latitude = 50.62)
#' north_carolina_model(data$hourtemps$Temp, summ = T)
#'
#' #Example 2
#' tempResponse(data, Start_JDay = 345, End_JDay = 58,
#'              models = list(North_Carolina_Units = north_carolina_model))

north_carolina_model <- function(HourTemp, summ = TRUE){
  return(chillR::step_model(HourTemp,
                            df = data.frame(lower = c(-1000, -1.1, 1.6, 7.2, 13.0, 16.5, 19.0, 20.7, 22.1, 23.3),
                                            upper = c(-1.1, 1.6, 7.2, 13.0, 16.5, 19.0, 20.7, 22.1, 23.3, 1000),
                                            weight = c(0, 0.5, 1, 0.5, 0, -0.5, -1, -1.5, -2, -2)), summ = summ))}
