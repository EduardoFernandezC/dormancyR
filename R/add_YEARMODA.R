#' Add a column for YEARMODA (or YEARMODAHO) to a data frame
#' 
#' This function allows users to add a column called YEARMODA or YEARMODAHO to any \code{\link{chillR}}-formated 
#' data frame
#' 
#' @param data is a data frame at least containing the columns \code{Year}, \code{Month}, and \code{Day} 
#' in the case of daily records; and \code{Year}, \code{Month}, \code{Day}, and \code{Hour} in the case
#' of hourly records. For now, column names must be spelled as shown in lines above
#' 
#' @param add_hour is a boolean parameter defining whether the output should add the \code{YEARMODAHO} column.
#' Default is set to \code{FALSE} so, even in case of hourly records, the ouput data frame will only show the 
#' \code{YEARMODA} column
#' 
#' @return 
#' A data frame 
#' 
#' @examples 
#' 
#' df <- data.frame(Year = rep(2011, 10),
#'                  Month = rep(1, 10),
#'                  Day = seq(1, 10),
#'                  Tmin = c(10, 12, 11, NA, 21, NA, NA, 10, 20, 23))
#'                  
#' add_YEARMODA(data = df, add_hour = FALSE)
#' 
#' @export add_YEARMODA

add_YEARMODA <- function(data, add_hour = FALSE){
  
  # Extract the column names in data for further use
  
  colnames <- colnames(data)
  
  # Check if the call in the function is adequate with the format of the data
  
  if (add_hour & !("Hour" %in% colnames))
    
    stop("It seems 'data' is not formated in hourly basis. 'Hour' column is missing in colnames(data).\n  Set 'add_hour' parameter to FALSE or provide an adequate 'Hour' column")
  
  
  if (add_hour)
    
    # Depending on the add_hour parameter, compute either YEARMODAHO or YEARMODA vectors
    
    YEARMODAHO <- data$Year * 1000000 + data$Month * 10000 + data$Day * 100 + data$Hour else
      
      YEARMODA <- data$Year * 10000 + data$Month * 100 + data$Day
    
    
    if (add_hour)
      
      return(data.frame(YEARMODAHO = YEARMODAHO, data)) else
        
        return(data.frame(YEARMODA = YEARMODA, data))
    
    
}