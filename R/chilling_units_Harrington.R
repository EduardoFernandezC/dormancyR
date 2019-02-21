#' Chilling units (Harrington, 2010)
#'
#' Function to calculate winter chill for deciduos trees according to the Chilling units model proposed by
#' Harrington (2010). This function is compatible with some 'chillR' functions (i.e tempResponse) to
#' estimate metrics from hourly temperature records.
#'
#' @param HourTemp Vector of hourly temperatures.
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour.
#'
#' @references Harrington C., Gould P. and St.Clair J. (2010). Modeling the effects of winter environment on
#' dormancy release of Douglas-fir. For. Ecol. Manage. 259(4): 798-808

chilling_units_Harrington <- function(HourTemp, summ = TRUE){

  df <- data.frame(Temps = HourTemp, CU = 0)
  df[which(df$Temps > -4.7 & df$Temps < 16),"CU"] <- 3.13 * (((df[which(df$Temps > -4.7 & df$Temps < 16),"Temps"] + 4.66) / 10.93) ^ 2.10) * 
    exp(1)^ - ((df[which(df$Temps > -4.7 & df$Temps < 16),"Temps"] + 4.66) / 10.93) ^ 3.10
  
  df[which(is.nan(df$CU)),"CU"] <- 0
  df[which(df$CU > 1),"CU"] <- 1

  if (summ == TRUE)
    return(cumsum(df$CU)) else return(df$CU)
}
