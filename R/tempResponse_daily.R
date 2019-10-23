#' Summarize chill responses from daily chill functions
#'
#' This function produces a summarized dataframe of chill responses computed by several chill models which
#' estimate daily chill instead of the metric for each hour. Five models are predetermined, but this function
#' also allows to include models predifined by the user. The only request is that such model function must
#' uses daily mean, max or min temperatures as input.
#'
#' @param data Could be a list of two or more elements. This list should contain a dataframe that could be
#' called "weather" if this is obtained from the fix_weather function in chillR or "data" is the list is
#' obtained from the chile_weather function in "eduaRdo" package. A third option is just a dataframe.
#' In any case, dataframe should contains colums "Year", "Month", "Day", "JDay", "Tmin" and "Tmax"
#' 
#' @param Start_JDay The start date for computing the outputs. This is the number of the day within the year
#' 
#' @param End_JDay The end date for computing the outputs. This is the number of the day within the year
#' 
#' @param models A list of chill functions to compute the metric for the period of interest. Each model
#' should be named. Default provides five models (See datails)
#' 
#' @examples 
#' library(chillR)
#' 
#' tempResponse_daily(KA_weather, Start_JDay = 335, End_JDay = 58)
#' 
#' @export tempResponse_daily

tempResponse_daily <- function (data, Start_JDay = 1, End_JDay = 366,
                                models = list(Rate_of_Chill = rate_of_chill_Chmielewski,
                                              Chill_Days = chill_days,
                                              Exponential_Chill = exponential_chill_Tmax,
                                              Triangula_Chill_Haninnen = triangular_chill_Hanninen,
                                              Triangular_Chill_Legave= triangular_chill_Legave)){
  
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
  
  #Identifiying the season within the dataframe for each year
  
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
  
    temps <- weather[which(!is.na(weather[,"sea"])),]
  
  #Computing the chill responses according to the models defined in the call of the fucntion  
  
    for (m in 1:length(models)) {
      temps[,names(models)[m]] <- do.call(models[[m]],list(temps, summ = F))}
  
  #Identifying the seasons
    
    seasons <- as.numeric(unique(weather$sea))
    seasons <- seasons[!is.na(seasons)]
  
  #Making a dataframe to save the summary of the chill responses
    
    output <- data.frame(Season = paste(seasons - 1, "/", seasons, sep = ""), End_Year = seasons)
  
  #Computing the number of days for the season of interest
    
    if (Start_JDay > End_JDay) {
      season_days <- NULL
        for (season in seasons){
          season_day <- chillR::JDay_count(Start_JDay,End_JDay,leap_year = chillR::leap_year(season-1)) + 1
      
          season_days <- c(season_days, season_day)}} else {
            season_days <- (End_JDay - Start_JDay) + 1}
  
  #Adding the season days to the output
    
    output[,"Season_days"] <- season_days
  
  #Computing the number of days with data
    
    data_days <- NULL
    for (i in 1: length(seasons)){
      days <- length(which(temps$sea == seasons[i])) - 
              length(which(is.na(temps[which(temps$sea == seasons[i]),"Tmin"] + 
                                     temps[which(temps$sea == seasons[i]),"Tmin"])))
      data_days <- c(data_days, days)}
  
  #Adding information to the output
    
    output[,"Data_days"] <- data_days
    output[,"Perc_complete"] <- (output$Data_days / output$Season_days ) * 100
  
  #Adding the results of the chill responses to the output. This is the sum of chill according to each model
  #used to compute the responses
    
    for (i in 1: length(seasons))
      for (m in 1: length(models)){
        output[i, names(models)[m]] <- sum(temps[which(temps$sea == seasons[i]),names(models)[m]],na.rm = T)}
  
  return(output)
}
