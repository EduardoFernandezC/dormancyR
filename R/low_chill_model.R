#' Low-chill model
#'
#' Function to compute winter chill for deciduous trees according to the Low-chill model proposed by
#' Gilreath and Buchanan (1981). This function is compatible with some 'chillR' functions (i.e tempResponse) to
#' estimate metrics from hourly temperature records.
#'
#' @param HourTemp Vector of hourly temperatures.
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour.
#'
#' @references Gilreath P. and Buchanan D. 1981. Rest prediction model for low-chilling sungold nectarine. J. Am.
#' Soc. Hort. Sci. 106(4): 426 - 429

low_chill_model <- function(HourTemp, summ = TRUE){
  lower <- c(-1000, 1.8, 7.9, 13.9, 16.9, 19.4, 21.5)
  upper <- c(1.8, 7.9, 13.9, 16.9, 19.4, 21.4, 1000)
  values <- c(0, 0.5, 1, 0.5, 0, -0.5, -1)
  df <- data.frame(Lower = lower, Upper = upper, Value = values)

  vector_of_values<-NULL
  for (i in 1: length(HourTemp)){
    value_in_x_temp<-values[which(HourTemp[i] > lower & HourTemp[i]<= upper)]
    vector_of_values<-c(vector_of_values,value_in_x_temp)}

  if (summ == TRUE)
    return(cumsum(vector_of_values)) else return(vector_of_values)
}
