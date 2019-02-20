#' Exponential chilling model (Legave et. al. 2008, 2013)
#'
#' This function computes the chill in an exponential way proposed by Legave et. al. (2008) and Legave et. al.
#' (2013). This model, which uses Tmax as input an a threshold of 15 celsius degree, was selected as one of the
#' "bests" over several models proposed by the same authors.
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

exponential_chill_Tmax <- function (ExtrDailyTemp, summ = TRUE) {

  vector_of_values<-NULL
  for (i in 1:length(ExtrDailyTemp$Tmax)){
    if (is.na(ExtrDailyTemp$Tmax[i])) exponential_chill <- NA else {
      exponential_chill <- exp(-ExtrDailyTemp$Tmax[i] / 15)}

    vector_of_values<-c(vector_of_values, exponential_chill)}

  if (summ == TRUE)
    return(cumsum(vector_of_values)) else return(vector_of_values)
}
