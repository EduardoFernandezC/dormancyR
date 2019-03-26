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
#' 
#' @examples 
#' library(chillR)
#' 
#' tempResponse_daily(KA_weather, Start_JDay = 345, End_JDay = 58, models = list(Triangular_Chill =
#'                                                                             triangular_chill_Hanninen))

triangular_chill_Hanninen <- function (ExtrDailyTemp, summ = TRUE){
  
  #Calculating Tmean from Tmax and Tmin
  
    if (!("Tmean" %in% names(ExtrDailyTemp)))
      ExtrDailyTemp[,"Tmean"] <- (ExtrDailyTemp["Tmax"] + ExtrDailyTemp["Tmin"]) / 2
  
  #As for condition 1 (Tmean <= -3.4 or Tmean > 10.4) chill unit is equal to 0 I have set all the values
  #a 0. From this I will extract those events in which chill unit has a value above 0
  
    ExtrDailyTemp[,"Triang_Chill_Hann"] <- 0 
  
  #Relevant days which fit the condition 2: -3.4 < Tmean <= 3.5
  
    rel_days_cond2 <- which(ExtrDailyTemp$Tmean > -3.4 & 
                            ExtrDailyTemp$Tmean <= 3.5)
  
    #Value of chill unit for days which fit the condition 2
      
      ExtrDailyTemp[rel_days_cond2, "Triang_Chill_Hann"] <- 0.159 * ExtrDailyTemp[rel_days_cond2, "Tmean"] + 0.506
  
  #Relevant days which fit the condition 3: 3.5 < Tmean <= 10.4
    
    rel_days_cond3 <- which(ExtrDailyTemp$Tmean > 3.5 & 
                            ExtrDailyTemp$Tmean <= 10.4) 
  
    #Value of chill unit for days which fit the condition 2
    
      ExtrDailyTemp[rel_days_cond3, "Triang_Chill_Hann"] <- -0.159 * ExtrDailyTemp[rel_days_cond3, "Tmean"] + 
                                                           1.621
  #End of the function
      
  if (summ == TRUE)
    return(cumsum(ExtrDailyTemp$Triang_Chill_Hann)) else return(ExtrDailyTemp$Triang_Chill_Hann)
}

