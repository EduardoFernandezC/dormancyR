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

  vector_of_values<-NULL
  for (i in 1:length(HourTemp)){
    if (HourTemp[i] < -4.7) chilling_unit <- 0 else
      if (HourTemp[i] > 16) chilling_unit <- 0 else
        if (HourTemp[i] >= -4.7 & HourTemp[i] <= 16)
          chilling_unit <- 3.13*(((HourTemp[i]+4.66)/10.93)^2.10)*exp(1)^-((HourTemp[i]+4.66)/10.93)^3.10
        if (chilling_unit > 1.0) chilling_unit <- 1.0 else chilling_unit <- chilling_unit
  vector_of_values<-c(vector_of_values, chilling_unit)}

  if (summ == TRUE)
    return(cumsum(vector_of_values)) else return(vector_of_values)
}
