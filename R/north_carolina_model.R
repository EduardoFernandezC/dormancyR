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

north_carolina_model <- function(HourTemp, summ = TRUE){
  lower <- c(-1000,1.6, 7.2, 13.0, 16.5, 19.0, 20.7, 22.1, 23.3)
  upper <- c(1.6, 7.2, 13.0, 16.5, 19.0, 20.7, 22.1, 23.3,1000)
  values <- c(0,0.5,1,0.5,0,-0.5,-1,-1.5,-2)
  df <- data.frame(Lower = lower, Upper = upper, Value = values)

  vector_of_values<-NULL
  for (i in 1: length(HourTemp)){
    value_in_x_temp<-values[which(HourTemp[i] > lower & HourTemp[i]<= upper)]
    vector_of_values<-c(vector_of_values,value_in_x_temp)}

  if (summ == TRUE)
    return(cumsum(vector_of_values)) else return(vector_of_values)
}
