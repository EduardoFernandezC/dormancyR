#' Growing degree hours model for Tb = 5 celsius degree
#' 
#' @param HourTemp Vector of hourly temperatures
#' @param summ Boolean parameter to add the cumulative summ as output

GDH_5 <- function (HourTemp, summ = TRUE) {
  Stress <- 1
  Tb <- 5
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