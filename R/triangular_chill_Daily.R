#' Triangular chill function (daily extremes input)
#'
#' Function to calculate winter chill for deciduos trees according to the Triangular chill function proposed by
#' Hanninen (1990) and Krammer (1994).
#'
#' @param ExtrDailyTemp Dataframe containing colums "Tmax" and "Tmin". These values must correspond to daily
#' records.
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour.
#'
#' @references Hanninen H. 1990. Modelling bud dormancy release in trees from cool and temperate regions.
#' Acta For. Fenn. 213: 1-47 doi:10.14214/aff.7660
#' Kramer K. 1994. Selecting a model to predict the onset of growth of \emph{Fagus sylvatica}.
#' J. Appl. Ecol. 31(1): 172-181

triangular_chill_daily_Hann <- function (ExtrDailyTemp, summ = TRUE){

  Tmean <- (ExtrDailyTemp["Tmax"] + ExtrDailyTemp["Tmin"]) / 2
  colnames(Tmean) <- "Tmean"

  vector_of_values<-NULL
  for (i in 1:length(Tmean$Tmean)){
    if (is.na(Tmean$Tmean[i])) triangular_chill_unit <- NA else
      if (Tmean$Tmean[i] <= -3.4) triangular_chill_unit <- 0 else
        if (Tmean$Tmean[i] > 10.4) triangular_chill_unit <- 0 else
          if (Tmean$Tmean[i] > -3.4 & Tmean$Tmean[i] <= 3.5)
            triangular_chill_unit <- 0.159 * Tmean$Tmean[i] + 0.506 else
              triangular_chill_unit <- -0.159 * Tmean$Tmean[i] + 1.621

            vector_of_values<-c(vector_of_values, triangular_chill_unit)}

  if (summ == TRUE)
    return(cumsum(vector_of_values)) else return(vector_of_values)
}
