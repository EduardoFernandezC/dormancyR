#' Triangular chill function (hourly input)
#'
#' Function to calculate winter chill for deciduos trees according to the Triangular chill function proposed by
#' Hanninen (1990) and Krammer (1994). Unlike original model, this function computes the output (Triangular chill
#' units) from hourly records instead using daily mean temperatures. This function is compatible with some 'chillR'
#' functions (i.e tempResponse) to estimate metrics from hourly temperature records.
#'
#' @param HourTemp Vector of hourly temperatures.
#' @param summ Boolean parameter indicating whether the computed metric should be provided as cumulative values
#' over the period or as the actual accumulation for each hour.
#'
#' @references Hanninen H. 1990. Modelling bud dormancy release in trees from cool and temperate regions.
#' Acta For. Fenn. 213: 1-47 doi:10.14214/aff.7660
#' Kramer K. 1994. Selecting a model to predict the onset of growth of \emph{Fagus sylvatica}.
#' J. Appl. Ecol. 31(1): 172-181

triangular_chill_Hourly <- function(HourTemp, summ = TRUE){
  vector_of_values<-NULL
  for (i in 1:length(HourTemp)){
    if (HourTemp[i] <= -3.4) triangular_chill_unit <- 0 else
      if (HourTemp[i] > 10.4) triangular_chill_unit <- 0 else
        if (HourTemp[i] > -3.4 & HourTemp[i] <= 3.5)
          triangular_chill_unit <- 0.159 * HourTemp[i] + 0.506 else
            triangular_chill_unit <- -0.159 * HourTemp[i] + 1.621

            vector_of_values<-c(vector_of_values, triangular_chill_unit)}

  if (summ == TRUE)
    return(cumsum(vector_of_values)) else return(vector_of_values)
}
