#' Modified Utah model
#'
#' Function to calculate winter chill for deciduos trees according to the Modified Utah model proposed by
#' Linvill (1990). This function is compatible with some 'chillR' functions (i.e tempResponse) to
#' estimate metrics from hourly temperature records.
#'
#' @param HourTemp Vector of hourly temperatures.
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour.
#'
#' @references Linvill D. 1990. Calculating chilling hours and chill units from daily maximum and minimum
#' temperature observations. HortScience 25(1): 14-16

modified_utah_model <- function(HourTemp, summ = TRUE){
   vector_of_values<-NULL
   for (i in 1:length(HourTemp))
    if (HourTemp[i] <= 0) modified_unit <- 0 else
      if (HourTemp[i] > 0 & HourTemp[i] <= 21) modified_unit <- sin((2*pi*HourTemp[i])/28) else
        modified_unit <- -1
   vector_of_values<-c(vector_of_values, modified_unit)
   if (summ == TRUE)
     return(cumsum(vector_of_values)) else return(vector_of_values)
}
