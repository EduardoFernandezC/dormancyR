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
rate_of_chill_Chmielewski <- function (ExtrDailyTemp, summ = TRUE){

  ExtrDailyTemp[,"Tmean"] <- (ExtrDailyTemp["Tmax"] + ExtrDailyTemp["Tmin"]) / 2

  vector_of_values<-NULL
  for (i in 1:length(ExtrDailyTemp$Tmean)){
    if (is.na(ExtrDailyTemp$Tmean[i])) rate_of_chill <- NA else
      if (ExtrDailyTemp$Tmean[i] <= 0 | ExtrDailyTemp$Tmean[i] >= 10.0) rate_of_chill <- 0 else
        if (ExtrDailyTemp$Tmean[i] > 0 & ExtrDailyTemp$Tmean[i] <= 4.2)
          rate_of_chill <- ExtrDailyTemp$Tmean[i] / 4.2  else
            rate_of_chill <- (ExtrDailyTemp$Tmean[i] - 10.0) / (4.2 - 10.0)

          vector_of_values<-c(vector_of_values, rate_of_chill)}

  if (summ == TRUE)
    return(cumsum(vector_of_values)) else return(vector_of_values)
}
