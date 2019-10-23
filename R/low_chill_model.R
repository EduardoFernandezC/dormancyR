#' Low-chill model
#'
#' Function to compute winter chill for deciduous trees according to the Low-chill model proposed by
#' Gilreath and Buchanan (1981). This function is compatible with some 'chillR' functions (i.e tempResponse) to
#' estimate metrics from hourly temperature records.
#'
#' @param HourTemp Vector of hourly temperatures
#' 
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour
#'
#' @references Gilreath P. and Buchanan D. 1981. Rest prediction model for low-chilling sungold nectarine. J. Am.
#' Soc. Hort. Sci. 106(4): 426 - 429
#' 
#' @examples 
#' library(chillR) 
#' 
#' #Example 1
#' data <- stack_hourly_temps(KA_weather, latitude = 50.62)
#' low_chill_model(data$hourtemps$Temp, summ = TRUE)
#' 
#' #Example 2
#' tempResponse(data, Start_JDay = 345, End_JDay = 58,
#' models = list(Low_Chill = low_chill_model))
#' 
#' @export low_chill_model

low_chill_model <- function (HourTemp, summ = TRUE) 
  return(chillR::step_model(HourTemp,
                    df = data.frame(lower = c(-1000, 1.0, 1.8, 7.9, 13.9, 16.9, 19.4, 21.5),
                                    upper = c(1.0, 1.8, 7.9, 13.9, 16.9, 19.4, 21.5, 1000),
                                    weight = c(0, 0.5, 1, 0.5, 0, -0.5, -1, -1)), summ = summ))

