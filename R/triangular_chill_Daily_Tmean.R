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

triangular_chill_daily_Tmean <- function (ExtrDailyTemp, summ = TRUE) {

  Tmean <- (ExtrDailyTemp["Tmax"] + ExtrDailyTemp["Tmin"]) / 2
  colnames(Tmean) <- "Tmean"

  threshold <- 1
  temp_interval <- 24

  vector_of_values<-NULL
  for (i in 1:length(Tmean$Tmean)){
    if (is.na(Tmean$Tmean[i])) triangular_chill <- NA else

      if (threshold - temp_interval < Tmean$Tmean[i] &  threshold + temp_interval > Tmean$Tmean[i]) {
        triangular_chill <- 1 - (abs(Tmean$Tmean[i] - threshold) / temp_interval)} else
          triangular_chill <- 0

    vector_of_values<-c(vector_of_values, triangular_chill)}

  if (summ == TRUE)
    return(cumsum(vector_of_values)) else return(vector_of_values)
}
