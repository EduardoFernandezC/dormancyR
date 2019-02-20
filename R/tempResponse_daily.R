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
#' In any case, dataframe should contains colums "Year", "Month", "Day", "JDay", "Tmin" and "Tmax".
#' @param Start_JDay The start date for computing the outputs. This is the number of the day within the year.
#' @param End_JDay The end date for computing the outputs. This is the number of the day within the year.
#' @param models A list of chill functions to compute the metric for the period of interest. Each model
#' should be named. Default provides five models (See datails).
#'
#' @details This function produces a list with two dataframes, "Chill" and "Missing_Days". "Chill" correspond
#' to a summary of winter chill for each complete season in the input dataframe according to the models
#' provided by the user in "models" parameter. The second dataframe ("Missing_Days") is a quality control in
#' which it is specified the number of days in the season in which a model fails in computing chill due to the
#' presence of gaps in the input dataframe.



tempResponse_daily <- function (data, Start_JDay = 1, End_JDay = 366,
                                models = list(Rate_of_Chill = rate_of_chill_Chmielewski,
                                              Chill_Days = chill_days,
                                              Exponential_Chill = exponential_chill_Tmax,
                                              Triangula_Chill_Hann = triangular_chill_daily_Hann,
                                              Triangular_Chill_Lega = triangular_chill_daily_Tmean)){

  if (is.data.frame(data)) {
    QC <- NULL
    weather <-  data} else {
      if("weather" %in% names(data)) {
        QC <- data$QC
        weather <- data$weather} else {
          if ("data" %in% names(data))
            weather_station <- data$Weather_Station
          weather <- data$data }}

  if (Start_JDay < End_JDay) {
    weather[which(weather$JDay >= Start_JDay & weather$JDay <=
                    End_JDay), "sea"] <- weather[which(weather$JDay >=
                                                         Start_JDay & weather$JDay <= End_JDay), "Year"]
  } else {
    weather[which(weather$JDay >= Start_JDay), "sea"] <- weather[which(weather$JDay >=
                                                                         Start_JDay), "Year"] + 1
    weather[which(weather$JDay <= End_JDay), "sea"] <- weather[which(weather$JDay <=
                                                                       End_JDay), "Year"]}

  if (Start_JDay < End_JDay) {
    relevant_days <- Start_JDay:End_JDay} else {
      relevant_days <- c(Start_JDay:366, 1:End_JDay)}

  temps <- weather[which(!is.na(weather[,"sea"])),]

  for (m in 1:length(models)) {
    temps[,names(models)[m]] <- do.call(models[[m]],list(temps, summ = F))}

  seasons <- as.numeric(unique(weather$sea))
  seasons <- seasons[!is.na(seasons)]

  output <- data.frame(Season = paste(seasons - 1, "/", seasons, sep = ""), End_Year = seasons)

  if (Start_JDay > End_JDay) {
    season_days <- NULL
    for (season in seasons){
      season_day <- chillR::JDay_count(Start_JDay,End_JDay,leap_year = chillR::leap_year(season)) + 1

      season_days <- c(season_days, season_day)}} else {
        season_days <- (End_JDay - Start_JDay) + 1}

  output[,"Season_days"] <- season_days

  data_days <- NULL
  for (i in 1: length(seasons)){
    days <- length(which(temps$sea == seasons[i]))
    data_days <- c(data_days, days)}

  output[,"Data_days"] <- data_days
  QC <- output

  for (i in 1: length(seasons))
    for (m in 1: length(models)){
      output[i, names(models)[m]] <- sum(temps[which(temps$sea == seasons[i]),names(models)[m]],na.rm = T)
      QC[i, names(models)[m]] <- length(which(is.na(temps[which(temps$sea == seasons[i]),names(models)[m]]) == T))}

  output <- output[which((output$Season_days - output$Data_days) <= 1),]
  QC <- QC[which((QC$Season_days - QC$Data_days) <= 1),]

  return(list (Chill = output, Missing_Days = QC))
}
