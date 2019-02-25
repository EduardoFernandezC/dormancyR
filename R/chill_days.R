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

chill_days <- function (ExtrDailyTemp, summ = TRUE){
  
  Tmean <- (ExtrDailyTemp["Tmax"] + ExtrDailyTemp["Tmin"]) / 2
  colnames(Tmean) <- "Tmean"
  
  threshold <- mean(c(7.0, 6.8, 6.9, 7.0, 7.9, 7.5, 7.0, 7.0, 7.3, 7.1, 7.1, 7.2))
  
  vector_of_values<-NULL
  for (i in 1:length(Tmean$Tmean)){
    if (is.na(ExtrDailyTemp$Tmin[i]) | is.na(ExtrDailyTemp$Tmax[i]) | ExtrDailyTemp$Tmax[i] < 0 ) chill_day <- NA else
      if (threshold >= 0 & threshold <= ExtrDailyTemp$Tmin[i]) chill_day <- 0 else
        
        if (0 <=  ExtrDailyTemp$Tmin[i] & threshold >= ExtrDailyTemp$Tmin[i] & threshold < ExtrDailyTemp$Tmax[i]) {
          chill_day <- (Tmean$Tmean[i] - ExtrDailyTemp$Tmin[i]) - ((ExtrDailyTemp$Tmax[i] - threshold)^2) /
            (2 * (ExtrDailyTemp$Tmax[i] - ExtrDailyTemp$Tmin[i]))} else
              
              if (0 <= ExtrDailyTemp$Tmin[i] & threshold >= ExtrDailyTemp$Tmax[i]) {chill_day <- Tmean$Tmean[i] -
                ExtrDailyTemp$Tmin[i]} else
                  
                  if (ExtrDailyTemp$Tmin[i] < 0 & ExtrDailyTemp$Tmax[i] >= 0 & threshold >= ExtrDailyTemp$Tmax[i]) {
                    chill_day <- (ExtrDailyTemp$Tmax[i]^2) / (2 * (ExtrDailyTemp$Tmax[i] - ExtrDailyTemp$Tmin[i]))} else
                      
                      if (ExtrDailyTemp$Tmin[i] < 0 & threshold > 0 & threshold < ExtrDailyTemp$Tmax[i]) {
                        chill_day <- (ExtrDailyTemp$Tmax[i]^2) / (2 * (ExtrDailyTemp$Tmax[i] - ExtrDailyTemp$Tmin[i])) -
                          (((ExtrDailyTemp$Tmax[i] - threshold)^2) /
                             (2 * (ExtrDailyTemp$Tmax[i] - ExtrDailyTemp$Tmin[i])))}
      
      vector_of_values<-c(vector_of_values, chill_day)}
  
  if (summ == TRUE)
    return(cumsum(vector_of_values)) else return(vector_of_values)
}
