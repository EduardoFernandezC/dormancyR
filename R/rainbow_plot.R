#' Visualizing phenology response to temperatures during two phases
#' 
#' The timing of many developmental stages of temperate trees may depend on temperatures during two
#' phases (e.g. bloom dates depend on the temperature during both the chilling and forcing phase of dormancy).
#' \code{rainbow_plot()} illustrates this dependency as a colored surface with contour lines by applying an
#' interpolating procedure with functions in the \code{\link{fields}} package. The plot is implemented
#' through functions in the \code{\link{ggplot2}} package.
#' 
#' @param pheno_data is a data frame that contains information on the timing of phenological events by year.
#' It should consist of two columns called \code{Year} and \code{pheno}. Data in the \code{pheno} column should be in
#' Julian date (day of the year).
#' 
#' @param weather_data is a dataframe containing daily minimum and maximum temperature data (in columns
#' called \code{Tmin} and \code{Tmax}, respectively). There also has to be a column for \code{Year}, one for
#' \code{Month} and one for \code{Day}. It can also contain a column for \code{JDay}
#' (the Julian date, or day of the year).
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
#' set to \code{TRUE} following the recommendation of the authors. For detailed information, please see the documentation
#' of the function.
#' 
#' @param x_axis_name is a character string that allows the user modifying the default label used in the x axis.
#' 
#' @param y_axis_name is a character string that allows the user modifying the default label used in the y axis.
#' 
#' @param legend_name is a character string that allows the user modifying the default label used in the legend.
#' 
#' @param contour_line_color is a character string representing the color used to draw the contour lines.
#' Default is set to black. If \code{NA} is used, the function will remove the contour lines with a warning.
#' 
#' @param point_color is a character string representing the color used to draw the points for actual
#' observations. Default is set to black. If \code{NA} is used, the function will remove the points with
#' a warning.
#' 
#' @param point_shape is a numeric input representing the point shape used to draw the points for actual
#' observations. Default is set to 19 (filled point). If \code{NA} is used, the function will remove the points with
#' a warning.
#' 
#' @param legend_colors is a character string representing the color scale used in the surface plot.
#' Default is set to \code{NULL} to let the function use the rainbow colors.
#' 
#' @param base_size is a numeric input representing the relative size of the elements in plot. \code{base_size}
#' is passed to \code{\link[ggplot2:ggtheme]{ggplot2::theme_bw}} as well as used to determine the size of the
#' points and contour lines.
#' 
#' @param ... accepts arguments passed to \code{\link[ggplot2:theme]{ggplot2::theme}} 
#' 
#' 
#' @details The generation of the color surface is based on the Kriging technique, which is typically used for 
#' interpolation of spatial data. The use for this particular purpose is a bit experimental.
#' 
#' @return \code{rainbow_plot()} is expected to return an object of class \code{gg} and \code{ggplot}. This
#' means that the plot can be later modified by using the syntax \code{'+'} from the
#' \code{\link{ggplot2}} package (see examples). The plot returned in the function should look as the following:
#' 
#' \if{html}{\figure{rainbowplotexample.png}{options: align='bottom' width='100\%' alt='rainbow_plot example'}}
#' \if{latex}{\figure{rainbowplotexample.png}{options: width=5in}}
#' 
#' @examples 
#' 
#' # Run a simple plot
#' 
#' rainbow_plot(pheno_data = chillR::KA_bloom,
#'              weather_data = chillR::KA_weather,
#'              chilling_phase = c(306, 350),
#'              forcing_phase = c(355, 60))
#'              
#' # Customize the aspects of the plot and save it as 'plot'
#' 
#' plot <- rainbow_plot(pheno_data = chillR::KA_bloom,
#'                      weather_data = chillR::KA_weather,
#'                      chilling_phase = c(306, 350),
#'                      forcing_phase = c(355, 60),
#'                      x_axis_name = "Temperatura en el periodo de frio (Celsius)",
#'                      y_axis_name = "Temperatura en el periodo de forzado (Celsius)",
#'                      legend_name = "Fecha de floracion\n(dia juliano)",
#'                      contour_line_color = "white",
#'                      point_color = "blue4",
#'                      point_shape = 4,
#'                      legend_colors = NULL,
#'                      base_size = 14,
#'                      legend.position = "bottom",
#'                      axis.title = ggplot2::element_text(family = "serif"))
#' 
#' plot
#' 
#' # Modify the plot object with the syntax from ggplot2.
#' # Be aware that the following code overrides the modifications
#' # done by the argument '...' in the main function
#' 
#' plot + ggplot2::theme_classic(base_size = 14)
#' 
#' @export rainbow_plot

rainbow_plot <- function(pheno_data,
                         weather_data,
                         split_month = 6,
                         chilling_phase,
                         forcing_phase,
                         Krig_warn = TRUE,
                         x_axis_name = NULL,
                         y_axis_name = NULL,
                         legend_name = NULL,
                         contour_line_color = "black",
                         point_color = "black",
                         point_shape = 19,
                         legend_colors = NULL,
                         base_size = 11,
                         ...){
  
  # REMEMBER TO ADD MORE CHECKS AND POSSIBLY THE CLASS THING
  
  # Check the class of pheno dataframe
  assertthat::assert_that("data.frame" %in% class(pheno_data),
                          msg = "The 'pheno_data' input must be an element of class data.frame, please use a valid argument.")
  
  # Check colnames in the pheno dataframe
  assertthat::assert_that(all(c("Year", "pheno") %in% colnames(pheno_data)),
                          msg = "The 'pheno_data' input must be an element of class data.frame, please use a valid argument.")
  
  # Check the pheno column in the pheno dataset and transform it to numeric in case this is TRUE
  if (!is.numeric(pheno_data$pheno)) 
    pheno_data$pheno <- suppressWarnings(as.numeric(as.character(pheno_data$pheno)))
  
  
  # Check the class of weather dataframe
  assertthat::assert_that("data.frame" %in% class(weather_data),
                          msg = "The 'weather_data' input must be an element of class data.frame, please use a valid argument.")
  
  # Check colnames in the weather dataframe
  assertthat::assert_that(all(c("Year", "Month", "Day") %in% colnames(weather_data)),
                          msg = "The 'weather_data' input must be an element of class data.frame, please use a valid argument.")
  
  # Check the columns in the weather dataset and transform it to numeric in case this is TRUE
  for (colname in c("Year", "Month", "Day")){
    if (!is.numeric(weather_data[[colname]])){
      warning(paste("The column", paste0("'", colname, "'"), "in the weather dataframe is not numeric. The function will coerce this column into\n numeric using the function 'as.numeric()'. Please be aware that this function may introduce NA's in the dataframe"))
    }
    
    weather_data[[colname]] <- suppressWarnings(as.numeric(as.character(weather_data[[colname]])))
  }
  
  # Check split month
  assertthat::assert_that(is.numeric(split_month) & split_month %in% 1 : 12,
                          msg = "'split_month' must be a numeric input (integer) between 1 and 12.\n Please provide a valid argument.")
  
  # Check the chilling phase
  assertthat::assert_that(is.numeric(chilling_phase) & all(chilling_phase %in% 1 : 366),
                          msg = "'chilling_phase' must be a numeric vector input (integers) with values in the range 1 and 366.\n Please provide a valid argument.")
  
  
  # Check the forcing phase
  assertthat::assert_that(is.numeric(forcing_phase) & all(forcing_phase %in% 1 : 366),
                          msg = "'forcing_phase' must be a numeric vector input (integers) with values in the range 1 and 366.\n Please provide a valid argument.")
  
  
  
  
  
  
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
  
  
  # Define some customizations to be used in the plot
  
  # Legend colors
  if (is.null(legend_colors)) legend_colors <- c("darkblue", "blue", "cyan2",
                                                 "green2", "yellow", "red", "red4")
  
  # x_axis_name
  if (is.null(x_axis_name)) 
    x_axis_name <- paste0("Mean temperature during the chilling phase (", "\U00B0", "C)")
  
  # y_axis_name
  if (is.null(y_axis_name)) 
    y_axis_name <- paste0("Mean temperature during the forcing phase (", "\U00B0", "C)")
  
  # legend_name
  if (is.null(legend_name)) 
    legend_name <- "Bloom date\n(DOY)"
  
  # Plot
  
  plot <- ggplot2::ggplot(fill_data, ggplot2::aes(Chilling, Forcing)) +
    ggplot2::geom_raster(ggplot2::aes(fill = Bloom)) +
    ggplot2::geom_contour(ggplot2::aes(z = Bloom), data = fill_data, size = base_size / 25,
                          color = contour_line_color) +
    metR::geom_text_contour(ggplot2::aes(z = Bloom), skip = 1,
                            data = fill_data, rotate = FALSE, stroke = 0.1,
                            stroke.color = "white", size = base_size * 0.3) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.1)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 0.1)) +
    ggplot2::scale_fill_gradientn(colors = legend_colors) +
    ggplot2::geom_point(ggplot2::aes(Tmean_chilling_period,
                                     Tmean_forcing_period), data = mean_temp_phase,
                        color = point_color,
                        size = base_size / 8,
                        shape = point_shape) +
    ggplot2::labs(x = x_axis_name,
                  y = y_axis_name,
                  fill = legend_name) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(legend.title.align = 0.5) +
    ggplot2::theme(...)
  
  return(plot)
  
}
