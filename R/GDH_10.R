#' Growing degree hours model for base temperature of 10 celsius degree
#' 
#' Modification of the actual model developed to quantify the heat between two periods. In this case the
#' function computes the metric by using as base temperature 10 celsius degree instead of 4 celsius degree 
#' as the original model.
#' 
#' @param HourTemp Vector of hourly temperatures
#' @param summ Boolean parameter to add the cumulative sum as output
#' 
#' @examples  
#' 
#' GDH_10(sample(c(1 : 30), 100, replace = TRUE), summ = TRUE)
#' 
#' @export GDH_10

GDH_10 <- function (HourTemp, summ = TRUE) {
  Stress <- 1
  Tb <- 10
  Tu <- 25
  Tc <- 36
  GDH_weight <- rep(0, length(HourTemp))
  GDH_weight[which(HourTemp >= Tb & HourTemp <= Tu)] <- Stress * 
    (Tu - Tb)/2 * (1 + cos(pi + pi * (HourTemp[which(HourTemp >= 
                                                       Tb & HourTemp <= Tu)] - Tb)/(Tu - Tb)))
  GDH_weight[which(HourTemp > Tu & HourTemp <= Tc)] <- Stress * 
    (Tu - Tb) * (1 + cos(pi/2 + pi/2 * (HourTemp[which(HourTemp > 
                                                         Tu & HourTemp <= Tc)] - Tu)/(Tc - Tu)))
  if (summ) 
    return(cumsum(GDH_weight))
  else return(GDH_weight)
}