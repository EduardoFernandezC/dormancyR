#' Summarize chill responses from daily chill models
#'
#' This function produces a summarized dataframe of chill responses computed by several chill models which
#' estimate daily chill instead of the metric for each hour. Five models are predetermined, but this function
#' also allows to include models predefined by the user. The only request is that such model function must
#' uses daily mean, max or min temperatures as input.
#'
#' @param data can be a list of two or more elements. This list should contain a dataframe that might be
#' called \emph{"weather"} if this is obtained from the \code{\link[chillR:fix_weather]{chillR::fix_weather}} function.
#' A second option is just a dataframe. In any case, the dataframe either contained in the list or introduced
#' directly as dataframe, must contain the columns \emph{"Year"}, \emph{"Month"}, \emph{"Day"}, \emph{"JDay"},
#' \emph{"Tmin"} and \emph{"Tmax"}
#' 
#' @param Start_JDay is the start date for computing the outputs. This is the number of the day within the year (DOY)
#' or Julian day
#' 
#' @param End_JDay is the end date for computing the outputs. This is the number of the day within the year (DOY) or
#' Julian day
#' 
#' @param models is list of chill functions to compute the metric for the period of interest. Each model
#' should be named. Default provides five models (see details)
#' 
#' @param misstolerance is the threshold defined by the user to include seasons having days with missing data.
#' Default is set to 20
#' 
#' @details
#' \code{tempResponse_daily} is an extension of \code{\link[chillR:chillR-package]{chillR}}
#' that returns temperature responses by using daily chill models. These models are \code{\link{rate_of_chill}},
#' \code{\link{chill_days}}, \code{\link{exponential_chill}}, \code{\link{triangular_chill_1}},
#' \code{\link{triangular_chill_2}}.
#' 
#' @examples 
#' library(chillR)
#' 
#' tempResponse_daily(KA_weather, Start_JDay = 335, End_JDay = 58)
#' 
#' @export tempResponse_daily

tempResponse_daily <- function (data, Start_JDay = 1, End_JDay = 366,
                                models = list(Rate_of_Chill = rate_of_chill,
                                              Chill_Days = chill_days,
                                              Exponential_Chill = exponential_chill,
                                              Triangula_Chill_Haninnen = triangular_chill_1,
                                              Triangular_Chill_Legave= triangular_chill_2),
                                misstolerance = 20){
  
  #Evaluating if the data is a dataframe or a list obtained from the fix_weather or patch_daily_temperatures
  #functions of chillR
  
  if (is.data.frame(data)) {
    QC <- NULL
    weather <-  data} else {
      if("weather" %in% names(data)) {
        QC <- data$QC
        weather <- data$weather}}
  
  #Evaluating if the dataframe contains a column called JDay, which is necessary for the following steps
  
  if ("JDay" %in% names(weather)) weather <- weather else
    weather <- chillR::make_JDay(weather)
  
  #Identifying the season within the dataframe for each year
  
  if (Start_JDay < End_JDay) {
    weather[which(weather$JDay >= Start_JDay & weather$JDay <=
                    End_JDay), "sea"] <- weather[which(weather$JDay >=
                                                         Start_JDay & weather$JDay <= End_JDay), "Year"]
  } else {
    weather[which(weather$JDay >= Start_JDay), "sea"] <- weather[which(weather$JDay >=
                                                                         Start_JDay), "Year"] + 1
    weather[which(weather$JDay <= End_JDay), "sea"] <- weather[which(weather$JDay <=
                                                                       End_JDay), "Year"]}
  
  
  #Selecting the relevant days for which the chill metrics must be computed
  
  temps <- weather[which(!is.na(weather[, "sea"])), ]
  
  #Computing the chill responses according to the models defined in the call of the fucntion  
  
  for (m in 1 : length(models)) {
    
    temps[, names(models)[m]] <- do.call(models[[m]], list(temps, summ = F))}
  
  #Identifying the seasons
  
  seasons <- as.numeric(unique(weather$sea))
  seasons <- seasons[!is.na(seasons)]
  
  #Making a dataframe to save the summary of the chill responses
  
  output <- data.frame(Season = paste(seasons - 1, "/", seasons, sep = ""), End_Year = seasons)
  
  #Computing the number of days for the season of interest
  
  if (Start_JDay > End_JDay) {
    season_days <- NULL
    
    for (season in seasons){
      season_day <- chillR::JDay_count(Start_JDay, End_JDay,leap_year = chillR::leap_year(season - 1)) + 1
      
      season_days <- c(season_days, season_day)}} else {
        season_days <- (End_JDay - Start_JDay) + 1}
  
  #Adding the season days to the output
  
  output[, "Season_days"] <- season_days
  
  #Computing the number of days with data
  
  data_days <- NULL
  
  for (i in 1: length(seasons)){
    
    days <- length(which(temps$sea == seasons[i])) - 
      
      length(which(is.na(temps[which(temps$sea == seasons[i]), "Tmin"] +
                           
                           temps[which(temps$sea == seasons[i]), "Tmin"])))
    
    data_days <- c(data_days, days)}
  
  #Adding information to the output
  
  output[, "Data_days"] <- data_days
  output[, "Perc_complete"] <- (output$Data_days / output$Season_days ) * 100
  
  #Adding the results of the chill responses to the output. This is the sum of chill according to each model
  #used to compute the responses
  
  for (i in 1 : length(seasons))
    for (m in 1 : length(models)){
      output[i, names(models)[m]] <- sum(temps[which(temps$sea == seasons[i]),names(models)[m]],na.rm = T)}
  
  return(output[output$Perc_complete >= 100 - misstolerance, ])
}
