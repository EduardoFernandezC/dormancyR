#' Triangular chilling model (Legave et. al. 2008, 2013)
#'
#' This function computes the chill according to the triangular function proposed by Legave et. al. (2008) and
#' Legave et. al. (2013). This model, which uses Tmean as input, was selected as one of the "bests" over several
#' models proposed by the same authors.
#'
#' @param ExtrDailyTemp Dataframe containing colums "Tmax" and "Tmin". These values must correspond to daily
#' records.
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour.
#'
#' @references Legave J., Farrera I., Almeras T. and Calleja M. 2008. Selecting models of apple flowering time
#' and understanding how global warming has had and impact on this trait. J. Horticult. Sci. Biotechnol. 83(1):
#' 76 - 84
#' Legave J., Blanke M., Christen D., Giovannini D, Mathieu V. and Oger R. 2013. A comprehensive overview of the
#' spatial and temporal variability of apple bud dormancy release and blooming phenology in Western Europe.
#' Int. J. Biometeorol. 57(2): 317 - 331. doi:10.1007/s00484-012-0551-9
#' 
#' @example
#' library(chillR)
#' 
#' tempResponse_daily(data, Start_JDay = 345, End_JDay = 58, models = list(Triangular_Chill_Legave =
#'                                                                         triangular_chill_Legave))

triangular_chill_Legave <- function (ExtrDailyTemp, summ = TRUE) {
  
  if (!("Tmean" %in% names(ExtrDailyTemp)))
    ExtrDailyTemp[,"Tmean"] <- (ExtrDailyTemp["Tmax"] + ExtrDailyTemp["Tmin"]) / 2
  
  threshold <- 1 #abbreviation TC
  temp_interval <- 24 #abbreviation Ic
  
  ExtrDailyTemp[,"Triang_Chill_Legave"] <- 0
  
  rel_days_cond2 <- which(threshold - temp_interval < ExtrDailyTemp$Tmean & 
                            threshold + temp_interval > ExtrDailyTemp$Tmean) # condition 2: TC-Ic < Tmean < TC+Ic
  
  ExtrDailyTemp[rel_days_cond2, "Triang_Chill_Legave"] <- 1 - (abs(ExtrDailyTemp[rel_days_cond2, "Tmean"] - 
                                                                     threshold) / temp_interval)
  
  if (summ == TRUE)
    return(cumsum(ExtrDailyTemp$Triang_Chill_Legave)) else return(ExtrDailyTemp$Triang_Chill_Legave)
}