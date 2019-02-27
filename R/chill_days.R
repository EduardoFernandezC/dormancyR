#' Chill days (Cesaraccio et al. 2004)
#'
#' This function computes the Chill Days according to the model proposed by Cesaraccio et al. (2004) developed
#' for several tree species including pears, kiwifruit and sweet cherries. In this function chill days are
#' computed without the negative (-) tranformation made in the original paper.
#'
#' @param ExtrDailyTemp Dataframe containing colums "Tmax" and "Tmin". These values must correspond to daily
#' records.
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour.
#'
#' @references Cesaraccio C., Spano D., Snyder R. and Duce P. 2004. Chilling and forcing model to predict
#' bud - burst of crop and forest species. Agric. For. Meteorol. 126(1-2): 1-13.
#' doi:10.1016/j.agrformet.2004.03.002
#' 
#' @examples
#' library(chillR)
#' 
#' #Example 1
#' chill_days(KA_weather, summ = TRUE)
#' 
#' #Example 2
#' data <- KA_weather
#' data[,"Chill_Days"] <- chill_days(data, summ = FALSE)
#' 
#' #Example 3
#' tempResponse_daily(KA_weather, Start_JDay = 345, End_JDay = 58, models = list(Chill_Days = chill_days),
#' QControl = T)

chill_days <- function (ExtrDailyTemp, summ = TRUE){
    
    threshold <- mean(c(7.0, 6.8, 6.9, 7.0, 7.9, 7.5, 7.0, 7.0, 7.3, 7.1, 7.1, 7.2)) # abbreviation: TC
    
    if (!("Tmean" %in% names(ExtrDailyTemp))) 
      ExtrDailyTemp[,"Tmean"] <- (ExtrDailyTemp["Tmax"] + ExtrDailyTemp["Tmin"]) / 2
    
    ExtrDailyTemp[,"Chill_Days"] <- 0 #condition 1: 0 <= TC <= Tmin <= Tmax
    
    rel_days_cond2 <- which(ExtrDailyTemp$Tmin >= 0 & ExtrDailyTemp$Tmin <= threshold &
                              threshold < ExtrDailyTemp$Tmax) # condition 2: 0 <= Tmin <= TC < Tmax
    
      ExtrDailyTemp[rel_days_cond2,"Chill_Days"] <- (ExtrDailyTemp[rel_days_cond2,"Tmean"] - 
                                                       ExtrDailyTemp[rel_days_cond2,"Tmin"]) - 
        ((ExtrDailyTemp[rel_days_cond2,"Tmax"] - threshold)^2) / (2 * (ExtrDailyTemp[rel_days_cond2,"Tmax"] -
                                                                       ExtrDailyTemp[rel_days_cond2,"Tmin"]))
    
    rel_days_cond3 <- which(0 <= ExtrDailyTemp$Tmin & 
                              threshold >= ExtrDailyTemp$Tmax) # condition 3: 0 <= Tmin <= Tmax <= TC
    
      ExtrDailyTemp[rel_days_cond3,"Chill_Days"] <- ExtrDailyTemp[rel_days_cond3,"Tmean"] - 
        ExtrDailyTemp[rel_days_cond3,"Tmin"]
    
    rel_days_cond4 <- which(ExtrDailyTemp$Tmin < 0 & ExtrDailyTemp$Tmax >= 0 & 
                              threshold >= ExtrDailyTemp$Tmax) # condition 4: Tmin <= 0 <= Tmax <= TC
    
      ExtrDailyTemp[rel_days_cond4,"Chill_Days"] <- (ExtrDailyTemp[rel_days_cond4,"Tmax"]^2) /
        (2 * (ExtrDailyTemp[rel_days_cond4,"Tmax"] - ExtrDailyTemp[rel_days_cond4,"Tmin"]))
    
    
    rel_days_cond5 <- which(ExtrDailyTemp$Tmin < 0 & threshold > 0 &
                              threshold < ExtrDailyTemp$Tmax) # condition 5: Tmin < 0 < TC < Tmax
    
      ExtrDailyTemp[rel_days_cond5,"Chill_Days"] <- (ExtrDailyTemp[rel_days_cond5,"Tmax"]^2) /
        (2 * (ExtrDailyTemp[rel_days_cond5,"Tmax"] - ExtrDailyTemp[rel_days_cond5,"Tmin"])) -
        (((ExtrDailyTemp[rel_days_cond5,"Tmax"] - threshold)^2) / (2 * (ExtrDailyTemp[rel_days_cond5,"Tmax"] - 
                                                                        ExtrDailyTemp[rel_days_cond5,"Tmin"])))
    if (summ == TRUE)
      return(cumsum(ExtrDailyTemp$Chill_Days)) else return(ExtrDailyTemp$Chill_Days)
}