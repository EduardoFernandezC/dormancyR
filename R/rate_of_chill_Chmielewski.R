#' Rate of chilling according to Chmielewski's model
#'
#' Function to calculate winter chill for deciduos fruit trees according to the model (sequential model) proposed by
#' Chmielewski et al. (2001) in Germany.
#'
#' @param ExtrDailyTemp Dataframe containing colums "Tmax" and "Tmin". These values must correspond to daily
#' records.
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour.
#'
#' @references Chmielewski F., Blumel K., Henniges Y., Blanke M., Weber R. and Zoth M. (2011).
#' Phenological models for the beginning of apple blossom in Germany. Meteorol. Z. 20(5): 487-496.
#' doi:10.1127/0941-2948/2011/0258
#' 
#' @examples 
#' library(chillR)
#' 
#' #Example 1
#' rate_of_chill_Chmielewski(KA_weather, summ = T)
#' 
#' #Example 2
#' tempResponse_daily(data, Start_JDay = 345, End_JDay = 58, models = list(Rate_of_Chill =
#'                                                                         rate_of_chill_Chmielewski))

rate_of_chill_Chmielewski <- function (ExtrDailyTemp, summ = TRUE){
  
  threshold <- 4.2 #abbreviation: TC
  
  if (!("Tmean" %in% names(ExtrDailyTemp)))
    ExtrDailyTemp[,"Tmean"] <- (ExtrDailyTemp["Tmax"] + ExtrDailyTemp["Tmin"]) / 2
  
  ExtrDailyTemp[,"Rate_of_Chill"] <- 0 # condition 1: Tmean <= 0 or Tmean >= 10.0
  
  rel_days_cond2 <- which(ExtrDailyTemp$Tmean > 0 & 
                            ExtrDailyTemp$Tmean <= threshold) # condition 2: 0 < Tmean < TC
  ExtrDailyTemp[rel_days_cond2, "Rate_of_Chill"] <- ExtrDailyTemp[rel_days_cond2, "Tmean"] / threshold 
  
  rel_days_cond3 <- which(ExtrDailyTemp$Tmean > threshold & 
                            ExtrDailyTemp$Tmean < 10.0) # condition 3: TC < Tmean < 10.0
  ExtrDailyTemp[rel_days_cond3, "Rate_of_Chill"] <- (ExtrDailyTemp[rel_days_cond3, "Tmean"] - 10.0) /
    (threshold - 10.0)
  
  if (summ == TRUE)
    return(cumsum(ExtrDailyTemp$Rate_of_Chill)) else return(ExtrDailyTemp$Rate_of_Chill)  
}