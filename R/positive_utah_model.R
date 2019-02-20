#' Positive Utah model
#'
#' Function to calculate winter chill for deciduos trees according to the Positive Utah model proposed by
#' Linsley-Noakes and Allan (1994). This function is compatible with some 'chillR' functions (i.e tempResponse)
#' to estimate metrics from hourly temperature records.
#'
#' @param HourTemp Vector of hourly temperatures.
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour.
#'
#' @references Linsley-Noakes G. and Allan P. 1994. Comparison of two models for the prediction of rest
#' competion in peaches. Sci. Hortic. 59(2): 107-113

positive_utah_model <- function(HourTemp, summ = TRUE){
  lower <- c(-1000,1.4, 2.4, 9.1, 12.4)
  upper <- c(1.4, 2.4, 9.1, 12.4, 1000)
  values <- c(0,0.5,1,0.5,0)
  df <- data.frame(Lower = lower, Upper = upper, Value = values)

  vector_of_values<-NULL
  for (i in 1: length(HourTemp)){
    value_in_x_temp<-values[which(HourTemp[i] > lower & HourTemp[i]<= upper)]
    vector_of_values<-c(vector_of_values,value_in_x_temp)}

  if (summ == TRUE)
    return(cumsum(vector_of_values)) else return(vector_of_values)
}
