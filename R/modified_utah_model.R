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
  
  df <- data.frame(Temps = HourTemp, CU = 0)
  df[which(df$Temps > 0 & df$Temps <= 21),"CU"] <- sin((2 *pi * 
                                                          df[which(df$Temps > 0 & df$Temps <= 21),"Temps"]) / 28)
  df[which(df$Temps > 21),"CU"] <- -1
  
  if (summ == TRUE)
    return(cumsum(df$CU)) else return(df$CU)}
