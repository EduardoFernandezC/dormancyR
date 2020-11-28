#' Compute the percentage of data complete within a dataframe
#' 
#' Often it is relevant to compute the percentage of data complete in a dataframe to have an idea of the 
#' quality of the data. This is specially important for weather data where climate databases often have missing 
#' days. This function only works for dataframes. For lists, see details.
#' 
#' @param x a dataframe normally produced through weather functions retrieving climate data. However, this is also
#' possible to use with any dataframe.
#' 
#' @details For list of dataframes, it is useful to combine this function with \code{\link[base]{lapply}}. The output is a dataframe
#' containing two columns (variable and percentage) and rows equal to the number of columns in the input dataframe.
#' 
#' @examples
#' 
#' df <- data.frame(year = rep(2011, 5),
#'                  month = rep(1, 5),
#'                  day = seq(1, 5),
#'                  Tmin = c(10, 12, 11, NA, 21))
#'                  
#' perc_complete(df)           
#' 
#' @export perc_complete
#' 

perc_complete <- function (x) {
  
  data <- data.frame(Variable = colnames(x), Percentage = NA,
                     Total_missing = NA)
  
  data$Variable <- as.character(data$Variable)
  
  for (i in 1 : length(data$Variable)) {
    
    data[i, "Percentage"] <- length(which(!is.na(x[, i])))/length(x[[i]]) * 100
    
    data[i, "Total_missing"] <- length(which(is.na(x[, i])))
  }
  
  return(data)
}

