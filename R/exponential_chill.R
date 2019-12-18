#' Exponential chilling model (Legave \emph{et al.} 2008, 2013)
#'
#' This function computes the chill using an exponential function proposed by Legave \emph{et al.} (2008) and Legave \emph{et al.}
#' (2013). This model, which uses Tmax as input an a threshold of 15 Celsius degree, was selected by the authors as one of the
#' "bests" over several alternatives.
#'
#' @param ExtrDailyTemp Dataframe containing columns "Tmax" and "Tmin". These values must correspond to daily
#' records
#' 
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour
#'
#' @references Legave J., Farrera I., Almeras T. and Calleja M. 2008. Selecting models of apple flowering time
#' and understanding how global warming has had and impact on this trait. J. Horticult. Sci. Biotechnol. 83(1):
#' 76 - 84
#' Legave J., Blanke M., Christen D., Giovannini D, Mathieu V. and Oger R. 2013. A comprehensive overview of the
#' spatial and temporal variability of apple bud dormancy release and blooming phenology in Western Europe.
#' Int. J. Biometeorol. 57(2): 317 - 331. doi:10.1007/s00484-012-0551-9
#' 
#' @examples 
#' library(chillR)
#' 
#' #Example 1
#' 
#' exponential_chill(KA_weather, summ = FALSE)
#' 
#' #Example 2
#' 
#' tempResponse_daily(KA_weather, Start_JDay = 345, End_JDay = 58, 
#' models = list(Exp_Chill = exponential_chill))
#' 
#' @export exponential_chill

exponential_chill <- function (ExtrDailyTemp, summ = TRUE) {
  
  #Threshold reported in the paper for which the model works relatively well
  
    threshold <- 15
  
  #Giving a value of 0 to the whole record
    
    exp_chill <- rep(0, length(ExtrDailyTemp$Year))
  
  #Selecting days with data
    
    relevant_days <- which(!is.na(ExtrDailyTemp$Tmax))
    
  #Appliying the function to those days which have data
    
    exp_chill[relevant_days] <- exp(-ExtrDailyTemp[relevant_days, "Tmax"] / threshold)
  
  #End of the function
    if (summ == TRUE)
      return(cumsum(exp_chill)) else return(exp_chill)
}
