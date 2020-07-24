#' Plot historic and future scenarios for climate-related metrics
#' 
#' Visualize outputs from the \code{\link[chillR:temperature_generation]{temperature_generation}} function used
#' in climate-related assessments. These outputs are usually compiled with the
#' \code{\link[chillR:make_climate_scenario]{make_climate_scenario}} function.
#' 
#' @param scenario_list is a list of lists containing information and data about the scenarios to be plotted. These
#'  lists must have:\itemize{
#'  
#'   \item{an element named \code{data}, which should be a list contain one or more named \code{data.frames} with a column
#'    named the same as the \code{metric} argument. This row must contain (\code{numeric}) information to be plotted.
#'    \code{data.frames} of climate-related metrics can be obtained with the 
#'    \code{\link[chillR:tempResponse_daily_list]{tempResponse_daily_list}} function. For
#'    past scenarios, the names of the dataframes can be the reference years used to generate the
#'    scenarios. These names will be recicled and used in the x-axis of the historic panel. For future 
#'    scenarios, the names of the dataframes can be the models used in the projections. These names
#'    will appear in the legend for future panels.}
#'   \item{an element named \code{caption} containing information about the scenario which the list
#'   is related to.}
#'   \item{an element named \code{historic_data} which represents a data frame for 
#'   actual observations in past scenarios.}
#'   \item{\code{time_series} is an optional argument that defines whether the scenario contains
#'   a time series.}
#'   \item{\code{labels} is an optional vector that usually contains the names of the elements used for
#'   \code{metric_summary} in \code{\link[chillR:make_climate_scenario]{make_climate_scenario}}.}}
#' 
#' @param metric is a character string corresponding to the name of the column that contains the data of interest
#' in the \code{data.frame} of the \code{scenario_list} (and, if applicable, in the
#' \code{historic_data}).
#' 
#' @param add_historic is a boolean parameter to define whether the plot should include the actual observations
#' of historic climated-related metrics.
#' 
#' @param ... accepts arguments that can be passed to \code{\link[ggplot2:layer]{layer}} and are 
#' commonly used outside the aesthetic function for different geoms. Options can be \code{size}, 
#' \code{color}, among others.
#' 
#' @param outlier_shape is the optional shape to replace the outliers in the boxplots. To show no oultiers
#'  use NA. See \code{\link[ggplot2:aes_linetype_size_shape]{shape}} for shape options.
#' 
#' @param historic_color is a character string corresponding to the color used to fill the boxplots in simulated
#' historic scenarios. Supported options are those provided by \code{\link[grDevices]{colors}}.
#' 
#' @details \code{Plot_scenarios} uses the \code{\link{ggplot2}} syntax for producing separated
#' plots for historic and future scenarios. Later, the plots are merged into one by using the
#' \code{\link{patchwork}} library.
#' 
#' @return A plot of classes \code{'patchwork'}, \code{'gg'}, and \code{'ggplot'}. This allows 
#' continue editing some features of the plots through the syntax (i.e. \code{'&'},
#' and \code{'+'}) from both libraries (see examples).
#' 
#' \figure{plot_scenarios_example.png}{options: align='bottom' width='100\%' alt='plot_scenarios example'}
#' 
#' @examples
#' 
#' # Make 3 identical objects as scenarios; let's assume these represent the
#' # years 2000, 2005 and 2010.
#' 
#' library(chillR)
#' 
#' # Compute chill responses for KA_weather data
#' 
#' chill <- tempResponse(stack_hourly_temps(
#'                       fix_weather(KA_weather[which(KA_weather$Year > 2003), ]),
#'                       latitude = 50.4), Start_JDay = 305, End_JDay = 60)
#' 
#' 
#' # Simulated scenarios labels
#' 
#' past_labels <- c(2000, 2005, 2010)
#' 
#' # Models labels
#' 
#' models_labels <- c("Climate model 1", "Climate model 2",
#'                    "Climate model 3")
#' 
#' # Add named elements to past and future scenarios
#' 
#' scenario_results_past <- list(`2000` = chill,
#'                               `2005` = chill,
#'                               `2010` = chill)
#'                               
#' scenario_results_future <- list(`Climate model 1` = chill,
#'                                 `Climate model 2` = chill,
#'                                 `Climate model 3` = chill)
#' 
#' # Define the climate scenario  
#' 
#' climate_scenario_list <- list(list(data = scenario_results_past,
#'                                    caption = c("Historic", "data"),
#'                                    time_series = TRUE,
#'                                    labels = past_labels,
#'                                    historic_data = chill),
#'                               list(data = scenario_results_future,
#'                                    caption = c("Scenario", "1"),
#'                                    labels = models_labels),
#'                               list(data = scenario_results_future,
#'                                    caption = c("Scenario", "2"),
#'                                    labels = models_labels),
#'                               list(data = scenario_results_future,
#'                                    caption=c("Scenario", "3"),
#'                                    labels = models_labels))
#'                                    
#' # Plot the climate scenarios
#' 
#' plot_scenarios(climate_scenario_list, metric = 'Chill_Portions',
#'                add_historic = TRUE, size = 2, shape = 3, color = 'blue',
#'                outlier_shape = 12, historic_color = 'skyblue')
#' 
#' # Since the output is a ggplot object, it is possible to continue
#' # modifying some general aspects of the plot
#' 
#' plot <- plot_scenarios(climate_scenario_list, metric = 'Chill_Portions',
#'                add_historic = TRUE, size = 2, shape = 3, color = 'blue',
#'                outlier_shape = 12, historic_color = 'skyblue')
#'                
#'                
#' # Example to change the color of the GCM scale
#'  
#' plot & ggplot2::scale_fill_brewer(type = 'qual')
#' 
#' # Modify axis title and axis text
#' 
#' plot & ggplot2::theme(axis.title = ggplot2::element_text(size = 14,
#'                                                          family = 'serif'),
#'                       axis.text = ggplot2::element_text(face = 'bold',
#'                                                         color = 'blue'))
#'   
#' @import patchwork
#' @export plot_scenarios

plot_scenarios <- function(scenario_list, metric, add_historic = TRUE, ..., outlier_shape = 19,
                           historic_color = "white"){
  
  
  # Generate an internal function to extract the scenarios from the scenario_list
  
  get_scenario <- function(list_one_level){
    
    scenario <- list_one_level[[which(names(list_one_level) == "caption")]]
    
    scenario_collapsed <- stringr::str_c(scenario, collapse = " ")
    
    scenario_collapsed}
  
  
  # Extract the scenario names
  
  scenarios <- unlist(lapply(scenario_list, get_scenario))
  
  # Identify the position for the Historic scenario within the scenario list
  
  position_historic <- stringr::str_which(scenarios, "Historic")
  
  
  # Extract the data for past simulated scenarios from the list generated by tempResponse function
  
  past_simulated <- dplyr::bind_rows(scenario_list[[position_historic]][["data"]], .id = "Ref_year")
  
  
  past_simulated["Scenario"] <- "Historic"
  
  
  # Extract the historic data from the list
  
  past_observed <- scenario_list[[position_historic]][["historic_data"]]
  
  
  # Small function to extract the data generated for future scenarios
  
  get_future_data <- function(list_one_level){
    
    data <- dplyr::bind_rows(list_one_level[["data"]], .id = "Model")
    
    data["Scenario"] <- stringr::str_c(list_one_level[["caption"]], collapse = " - ")
    
    data}
  
  
  # Apply the function to all elements in the scenario_list less the one for historic scenarios
  
  future_data <- lapply(scenario_list[-position_historic], get_future_data)
  
  # Generate one big dataframe for future projections
  
  future_data <- dplyr::bind_rows(future_data, .id = "")
  
  # Extract the model names
  
  Models <- unique(future_data$Model)
  
  
  # Define the max and min values for the y-scale
  
  max_y <- max(c(past_observed[[metric]],
                 past_simulated[[metric]],
                 future_data[[metric]]))
  
  min_y <- min(c(past_observed[[metric]],
                 past_simulated[[metric]],
                 future_data[[metric]]))
  
  
  # In case the models do not return negative values, we set min_y to 0
  
  if (min_y >= 0) min_y <- 0 else min_y <- min_y - 10
  
  
  # We define the y-axes title according to chill and heat models
  
  if (metric == "GDH") var_label <- "Heat"
  if (metric %in% c("Chill_Portions", "Chilling_Hours", "Utah_Chill_Units")) var_label <- "Chill"
  
  
  # Define the name of the metric according to the model we used 
  
  if (metric == "Chill_Portions") label <- "CP"
  if (metric == "Chilling_Hours") label <- "CH"
  if (metric == "Utah_Chill_Units") label <- "CU"
  if (metric == "GDH") label <- "GDH"
  
  
  # Generate the plot for the past scenarios, historic and simulated
  
  past_plot <- ggplot2::ggplot() + 
    ggplot2::geom_boxplot(ggplot2::aes(Ref_year, !!ggplot2::ensym(metric)), data = past_simulated,
                          outlier.shape = outlier_shape, size = 0.3, outlier.size = 1,
                          fill = historic_color) +
    ggplot2::scale_x_discrete(breaks = unique(past_simulated$Ref_year),
                              expand = ggplot2::expansion(add = 1)) +
    ggplot2::scale_y_continuous(limits = c(min_y, round(max_y + 10)),
                                expand = ggplot2::expansion(add = 0),
                                labels = scales::comma) +
    ggplot2::labs(x = "Year", y = paste(var_label, "accumulation in", label)) +
    ggplot2::facet_grid(~ Scenario) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold"))
  
  # Add the historic chill calculated for all years of the dataframe
  
  if (add_historic)
    
    past_plot <- past_plot + ggplot2::geom_point(ggplot2::aes(as.character(End_year),
                                                              !!ggplot2::ensym(metric)),
                                                 data = past_observed, ...)
  
  
  # Plot for future scenarios
  
  future <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(factor(Model, levels = Models),
                                       !!ggplot2::ensym(metric),
                                       fill = factor(Model, levels = Models)),
                          data = future_data,
                          outlier.shape = outlier_shape, size = 0.3, outlier.size = 1) +
    ggplot2::scale_x_discrete(labels = NULL,
                              expand = ggplot2::expansion(add = 1)) +
    ggplot2::scale_y_continuous(limits = c(min_y, round(max_y + 10)),
                                expand = ggplot2::expansion(add = 0),
                                labels = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(title.position = 'top', title.hjust = 0.5)) +
    ggplot2::labs(x = NULL, y = NULL, fill = "General Circulation Model (GCM)") +
    ggplot2::facet_wrap(~ Scenario, nrow = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(margin = ggplot2::margin(0, 0, 0, 0, "cm")),
                   legend.position = "bottom",
                   legend.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
                   legend.background = ggplot2::element_rect(),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold"),
                   legend.box.spacing = ggplot2::unit(0, "cm"))
  
  
  # Merge plots into one
  
  plot <- (past_plot | future) + patchwork::plot_layout(widths = c(1, length(scenario_list) - 1))
  
  return(plot)}
