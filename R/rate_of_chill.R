#' Rate of chilling according to Chmielewski's model
#'
#' Function to calculate winter chill for deciduous fruit trees according to the model (sequential model) proposed by
#' Chmielewski \emph{et al.} (2001) in Germany.
#'
#' @param ExtrDailyTemp Dataframe containing columns \emph{"Tmax"} and \emph{"Tmin"}. These values must correspond to daily
#' records
#' 
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour
#'
#' @references Chmielewski F., Blumel K., Henniges Y., Blanke M., Weber R. and Zoth M. (2011).
#' Phenological models for the beginning of apple blossom in Germany. Meteorol. Z. 20(5): 487-496.
#' doi:10.1127/0941-2948/2011/0258
#' 
#' @examples 
#' library(chillR)
#' 
#' #Example 1
#' 
#' rate_of_chill(KA_weather, summ = TRUE)
#' 
#' #Example 2
#' 
#' tempResponse_daily(KA_weather, Start_JDay = 345, End_JDay = 58,
#'                    models = list(Rate_of_Chill = rate_of_chill))
#' 
#' @export rate_of_chill

rate_of_chill <- function (ExtrDailyTemp, summ = TRUE){
  
  #Threshold (Tc) reported in the paper 
  
    threshold <- 4.2 
  
  #Computing Tmean from Tmin and Tmax
    
    if (!("Tmean" %in% names(ExtrDailyTemp)))
    ExtrDailyTemp[,"Tmean"] <- (ExtrDailyTemp["Tmax"] + ExtrDailyTemp["Tmin"]) / 2
  
  #Selecting those days which fit the condition 1: Tmean <= 0 or Tmean >= 10.0 and giving a value of 0
    
    ExtrDailyTemp[,"Rate_of_Chill"] <- 0  
  
  #Selecting days which fit the condition 2: 0 < Tmean < Threshold
    
    rel_days_cond2 <- which(ExtrDailyTemp$Tmean > 0 & 
                            ExtrDailyTemp$Tmean <= threshold)
      
    #Computing chill for those days
    
      ExtrDailyTemp[rel_days_cond2, "Rate_of_Chill"] <- ExtrDailyTemp[rel_days_cond2, "Tmean"] / threshold 
  
  #Selecting days which fit the condition 3: Threshold < Tmean < 10.0
     
    rel_days_cond3 <- which(ExtrDailyTemp$Tmean > threshold & 
                            ExtrDailyTemp$Tmean < 10.0)
    
    #Computing chill for those days
    
      ExtrDailyTemp[rel_days_cond3, "Rate_of_Chill"] <- (ExtrDailyTemp[rel_days_cond3, "Tmean"] - 10.0) /
                                                          (threshold - 10.0)
  
  #End of the function
      
    if (summ == TRUE)
      return(cumsum(ExtrDailyTemp$Rate_of_Chill)) else return(ExtrDailyTemp$Rate_of_Chill)  
}