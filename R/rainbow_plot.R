#' Visualizing phenology response to temperatures during two phases
#' 
#' The timing of many developmental stages of temperate trees may depend on temperatures during two
#' phases (e.g. bloom dates depend on the temperature during both the chilling and forcing phase of dormancy).
#' `rainbow_plot()` illustrates this dependency as a colored surface with contour lines by applying an
#' interpolating procedure with functions in the \code{\link{fields}} package. The plot is implemented
#' through functions in the \code{\link{ggplot2}} package.
#' 
#' @param pheno_data is a data frame that contains information on the timing of phenological events by year.
#' It should consist of two columns called `Year` and `pheno`. Data in the `pheno` column should be in
#' Julian date (day of the year)
#' 
#' @param weather_data is a dataframe containing daily minimum and maximum temperature data (in columns
#' called `Tmin` and `Tmax`, respectively). There also has to be a column for `Year` and one for `JDay`
#' (the Julian date, or day of the year). Alternatively, the date can also be given in three columns
#' (`Year`, `Month` and `Day`)
#' 
#' @param split_month is an integer representing the last month of the growing season. This procedure analyzes
#' data by phenological year, which can start and end in any month during the calendar year (currently only
#' at the beginning of a month). This variable indicates the last month (e.g. 5 for May) that should be
#' included in the record for a given phenological year. All subsequent months are assigned to the 
#' following phenological year.
#' 
#' @param chilling_phase is a vector of integers representig the start and end for the chilling period
#' in temperate trees. Numbers must be provided in Julian date (day of the year).
#' 
#' @param forcing_phase is a vector of integers representig the start and end for the forcing period
#' in temperate trees. Numbers must be provided in Julian date (day of the year).
#' 
#' @param Krig_warn is a boolean parameter passed to the \code{\link[fields:Krig]{Krig}} function. Default is
#' set `TRUE` following the recomendation of the authors. For detailed information, please see the documentation
#' of the function.
#' 
#' @details The generation of the color surface is based on the Kriging technique, which is typically used for 
#' interpolation of spatial data. The use for this particular purpose is a bit experimental.
#' 
#' @return `rainbow_plot()` is expected to return a plot as the following
#' 
#' \figure{rainbow_plot_example.png}{options: align='bottom' width='100\%' alt='rainbow_plot example'}
#' 
#' @examples 
#'  
#' rainbow_plot(pheno_data = chillR::KA_bloom,
#'              weather_data = chillR::KA_weather,
#'              chilling_phase = c(306, 350),
#'              forcing_phase = c(355, 60))
#' 
#' @export rainbow_plot

rainbow_plot <- function(pheno_data, weather_data, split_month = 6, chilling_phase, forcing_phase,
                         Krig_warn = TRUE){
  
  # REMEMBER TO ADD MORE CHECKS AND POSSIBLY THE CLASS THING
  
  if (!is.numeric(pheno_data$pheno)) 
    pheno_data$pheno <- suppressWarnings(as.numeric(as.character(pheno_data$pheno)))
  
  
  # Define the seasons in the weather dataframe according to the split_month argument
  
  weather_data[weather_data$Month <= split_month,
               "Season"] <- weather_data[weather_data$Month <= split_month, "Year"]
  
  weather_data[weather_data$Month > split_month,
               "Season"] <- weather_data[weather_data$Month > split_month, "Year"] + 1
  
  
  # Get the common years in both the weather and pheno dataframe
  
  common_seasons <- intersect(unique(weather_data$Season), unique(pheno_data$Year))
  
  
  # Remove the missing rows in the pheno data frame
  
  pheno_data <- stats::na.omit(pheno_data)
  
  
  # Keep data for the same years in both dataframes
  
  weather_data <- weather_data[weather_data$Year %in% common_seasons, ]
  
  pheno_data <- pheno_data[pheno_data$Year %in% common_seasons, ]


  # Chilling and forcing phases
  
  if (chilling_phase[1] > chilling_phase[2])
    
    chill_days <- c(chilling_phase[1] : 366, 1 : chilling_phase[2]) else
      chill_days <- chilling_phase[1] : chilling_phase[2]
  
  
  if (forcing_phase[1] > forcing_phase[2]) 
    
    forcing_days <- c(forcing_phase[1] : 366, 1 : forcing_phase[2]) else
      forcing_days <- forcing_phase[1] : forcing_phase[2]
  
  
  # Add the Jday column
  
  weather_data[, "JDay"] <- strptime(paste(weather_data$Month, 
                                           "/", weather_data$Day, "/", weather_data$Year, 
                                           sep = ""), "%m/%d/%Y")$yday + 1
  
  
  # This for loop will check if the chilling season contains all days or not...
  # I may check if there is a more clever way to do this task...
  
  chilling_period <- NULL
  
  for (common_season in common_seasons){
    
    
    chill_season <- weather_data[weather_data$Season == common_season &
                                   weather_data$JDay %in% chill_days, ]
    
    if (nrow(chill_season) == length(chill_days) + 1 |
        nrow(chill_season) == length(chill_days) |
        nrow(chill_season) == length(chill_days) - 1)
      
      if (!is.null(chilling_period))
        
        chilling_period <- dplyr::bind_rows(chilling_period, chill_season) else
          chilling_period <- chill_season}
    
  
  # Same for the forcing period
  
  forcing_period <- NULL
  
  for (common_season in common_seasons){
    
    
    forcing_season <- weather_data[weather_data$Season == common_season &
                                      weather_data$JDay %in% forcing_days, ]
    
    if (nrow(forcing_season) == length(forcing_days) + 1 |
        nrow(forcing_season) == length(forcing_days) |
        nrow(forcing_season) == length(forcing_days) - 1)
      
      if (!is.null(forcing_period))
        
        forcing_period <- dplyr::bind_rows(forcing_period, forcing_season) else
          forcing_period <- forcing_season}
    
  
  # Summarize the data to obtain the mean temperature during the chilling and forcing phase respectively
  
  chilling_period <- suppressMessages(chilling_period %>% dplyr::mutate(Tmean = (Tmin + Tmax) /2) %>% 
                                      dplyr::group_by(Season) %>% 
                                      dplyr::summarise(Tmean_chilling_period = mean(Tmean)))
  
  
  # The same for forcing data frame
  
  forcing_period <- suppressMessages(forcing_period %>% dplyr::mutate(Tmean = (Tmin + Tmax) /2) %>%  
                                     dplyr::group_by(Season) %>% 
                                     dplyr::summarise(Tmean_forcing_period = mean(Tmean)))
  
  
  # Merge both data frames 
  
  mean_temp_phase <- dplyr::left_join(chilling_period, forcing_period, by = "Season")
  
  
  # Add the bloom date
  
  mean_temp_phase <- dplyr::left_join(mean_temp_phase, pheno_data, by = c("Season" = "Year"))
  

  # Remove NAs for kriging surface
  
  mean_temp_phase <- stats::na.omit(mean_temp_phase)
  
  # Implement the Kriging interpolation by using the function Krig() from the fields package.
  # This will assume a linear additive model (see Krig() help for more information)
  
  interpolated_surface <- fields::Krig(x = as.matrix(mean_temp_phase[, c("Tmean_chilling_period",
                                                                         "Tmean_forcing_period")]),
                                       Y = mean_temp_phase$pheno,
                                       give.warnings = Krig_warn)
  
 
  # Rainbow plot with ggplot2
  
  # Predict the surface from the Krig element
  
  surface <- fields::predictSurface(interpolated_surface)
  
  
  # Transform the matrix containing the "z" information into a data frame
  
  fill_data <- as.data.frame(surface$z)
  
  
  # Set the names of the columns as the temperature in the forcing period (y)
  
  colnames(fill_data) <- surface$y
  
  
  # Add a column for the temperature in the chilling phase (x)
  
  fill_data <- data.frame(Chilling = surface$x,
                          fill_data)
  
  
  # Use pivot_longer to get the data into ggplot-useful format
  
  fill_data <- tidyr::pivot_longer(fill_data, -Chilling, names_to = c("X", "Forcing"),
                                   values_to = "Bloom", names_sep = "X")[-2]
  
  
  # Set the column for forcing data as.numeric()
  
  fill_data$Forcing <- suppressWarnings(as.numeric(fill_data$Forcing))
  
  
  # Remove missing values from the data frame
  
  fill_data <- stats::na.omit(fill_data)
  
  
  # Plot
  
  plot <- ggplot2::ggplot(fill_data, ggplot2::aes(Chilling, Forcing)) +
    ggplot2::geom_raster(ggplot2::aes(fill = Bloom)) +
    ggplot2::geom_contour(ggplot2::aes(z = Bloom), data = fill_data, size = 0.7,
                          color = "black") +
    metR::geom_text_contour(ggplot2::aes(z = Bloom), skip = 1,
                            data = fill_data, rotate = FALSE, stroke = 0.1,
                            stroke.color = "white") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.1)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 0.1)) +
    ggplot2::scale_fill_gradientn(colors = c("darkblue", "blue", "cyan2",
                                             "green2", "yellow", "red", "red4")) +
    ggplot2::geom_point(ggplot2::aes(Tmean_chilling_period,
                                     Tmean_forcing_period), data = mean_temp_phase,
                        color = "black") +
    ggplot2::labs(x = paste0("Mean temperature during the chilling phase (",
                            "\U00B0", "C)"),
                  y = paste0("Mean temperature during the Forcing phase (",
                             "\U00B0", "C)"),
                  fill = "Bloom date\n(DOY)") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title.align = 0.5)
  
  return(plot)

}


