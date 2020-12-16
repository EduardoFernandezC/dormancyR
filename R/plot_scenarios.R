#' Plot historic and future scenarios for climate-related metrics
#' 
#' Visualize outputs from the \code{\link[chillR:temperature_generation]{temperature_generation}} function used
#' in climate-related assessments. These outputs are usually compiled with the
#' \code{\link[chillR:make_climate_scenario]{make_climate_scenario}} function.
#' 
#' @param scenario_list is a list of lists containing information and data about the scenarios to be plotted. These
#'  lists must have:\itemize{
#'    
#'   \item{an element named \code{data}, which should be a list contain one or more named dataframes with a column
#'    named the same as the \code{metric} argument. This row must contain (\code{numeric}) information to be plotted.
#'    Dataframes of climate-related metrics can be obtained with the 
#'    \code{\link[chillR:tempResponse_daily_list]{tempResponse_daily_list}} function. For
#'    past scenarios, the names of the dataframes can be the reference years used to generate the
#'    scenarios. These names will be recycled and used in the x-axis of the historic panel. For future 
#'    scenarios, the names of the dataframes can be the models used in the projections. These names
#'    will appear in the legend for future panels.}
#'       
#'   \item{an element named \code{caption} containing information about the scenario which the list
#'   is related to.}
#'      
#'   \item{an element named \code{historic_data} which represents a data frame for 
#'   actual observations in past scenarios. This element can be optional but is mandatory if
#'   \code{add_historic = TRUE}}
#'     
#'   \item{\code{time_series} is an optional argument that defines whether the scenario contains
#'   a time series.}
#'   
#'   \item{\code{labels} is an optional vector that usually contains the names of the elements used for
#'   \code{metric_summary} in \code{\link[chillR:make_climate_scenario]{make_climate_scenario}}.}}
#' 
#' @param metric is a character string corresponding to the name of the column that contains the data of interest
#' in the dataframe of the \code{scenario_list} (and, if applicable, in the
#' \code{historic_data}).
#' 
#' @param add_historic is a boolean parameter to define whether the plot should include the actual observations
#' of historic climate-related metrics.
#' 
#' @param ... accepts arguments that can be passed to \code{\link[ggplot2:layer]{layer}} and are 
#' commonly used outside the aesthetic function for different geoms. In this case, \code{...} is passed to the 
#' \code{\link[ggplot2:geom_point]{geom_point}} function in the case that actual observations of chill or heat
#' are displayed. Options are \code{size}, \code{color}, among others. 
#' 
#' @param outlier_shape is the optional shape to replace the outliers in the boxplots. To show no oultiers
#'  use NA. See \code{\link[ggplot2:aes_linetype_size_shape]{shape}} for shape options.
#' 
#' @param historic_color is a character string corresponding to the color used to fill the boxplots in simulated
#' historic scenarios. Supported options are those provided by \code{\link[grDevices]{colors}}.
#' 
#' @param group_by is a vector of character strings indicating how the plots should be grouped.
#' I.e. by \code{Scenario} and then \code{Year} or viceversa.
#' 
#' @param y_axis_name is a character string representing the title of the y axis in the final plot. Default
#' is set to \code{paste('Accumulated response in', metric)} to let the function obtain the name based on 
#' the \code{metric} argument.
#' 
#' @param x_axis_name is a character string representing the title of the x axis in the 'Historic' panel.
#' Default is set to \code{Year}.
#' 
#' @param legend_title is a character string representing the title of the legend showing the
#' climate models used in the assessment.
#' 
#' @param legend_labels is a vector of character strings that allows the user to modify the names of the climate
#' models used in the projections. The length of the vector must coincide with the number of climate models.
#' Default is set to \code{NULL} to let the function use the labels generated with the
#' \code{\link[chillR:make_climate_scenario]{make_climate_scenario}} function.
#' 
#' @param panel_labels is a list of 3 named objects that allows the user to customize the text in the upper part
#' of the plot. Default is set to \code{NULL} to let the function use the labels generated with the
#' \code{\link[chillR:make_climate_scenario]{make_climate_scenario}} function. If provided, the
#' objects of the list must be:\itemize{
#'  
#'   \item{an element named \strong{Historic} containing the name to be used in the 'Historic' panel.}
#'   
#'   \item{an element named \strong{Scenario} containing the names of the scenarios used for the projections.
#'   If \code{group_by = c("Year", "Scenario")} is used, \code{Scenario} must be a list of named objects 
#'   according to the labels used in the \code{Year} object. See examples.}
#'      
#'   \item{an element named \strong{Year} containing the labels to be used for the time horizons used in the
#'   assessment. If \code{group_by = c("Scenario", "Year")} is used, \code{Year} must be a list of named objects 
#'   according to the labels used in the \code{Scenario} object. See examples.}}
#'   
#' @param base_size is an integer to define the relative size of the text in the final plot. This argument
#' is passed to \code{\link[ggplot2:ggtheme]{ggpplot2::theme_bw}}. Default is set to 11.
#' 
#' @details \code{plot_scenarios} uses the \code{\link{ggplot2}} syntax for producing separated
#' plots for historic and future scenarios. Later, the plots are merged into one final figure by using the
#' \code{\link{patchwork}} library.
#' 
#' @return A plot of classes \code{'patchwork'}, \code{'gg'}, and \code{'ggplot'}. This allows the user to
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
#'                                    caption = c("Scenario 1", "2050"),
#'                                    labels = models_labels),
#'                               list(data = scenario_results_future,
#'                                    caption = c("Scenario 1", "2075"),
#'                                    labels = models_labels),
#'                               list(data = scenario_results_future,
#'                                    caption=c("Scenario 1", "2100"),
#'                                    labels = models_labels),
#'                               list(data = scenario_results_future,
#'                                    caption=c("Scenario 2", "2040"),
#'                                    labels = models_labels),
#'                               list(data = scenario_results_future,
#'                                    caption=c("Scenario 2", "2080"),
#'                                    labels = models_labels))
#'                                    
#' # Plot the climate scenarios
#' 
#' plot_scenarios(climate_scenario_list, metric = 'Chill_Portions',
#'                add_historic = TRUE, size = 2, shape = 3, color = 'blue',
#'                outlier_shape = 12, historic_color = 'skyblue',
#'                group_by = c("Year", "Scenario"))
#'                
#' # Plot scenarios modifying the whole text in the plot
#' 
#' plot_scenarios(scenario_list = climate_scenario_list, metric = 'Chill_Portions',
#'                 add_historic = TRUE, size = 2, shape = 3, color = 'blue',
#'                 outlier_shape = 12, historic_color = 'skyblue',
#'                 group_by = c("Scenario", "Year"),
#'                 y_axis_name = "Acumulacion de frio en CP",
#'                 x_axis_name = "Tiempo",
#'                 legend_title = "Modelo climatico",
#'                 legend_labels = c("Modelo 1", "Modelo 2", "Modelo 3"),
#'                 panel_labels = list(Historic = "Historico",
#'                                     Scenario = c("Escenario 1",
#'                                                  "Escenario 2"),
#'                                     Year = list(`Escenario 1` = c("Futuro cercano",
#'                                                                   "Futuro medio",
#'                                                                   "Future lejano"),
#'                                                 `Escenario 2` = c("Futuro cercano",
#'                                                                   "Futuro medio"))))
#'
#' # Since the output is a ggplot object, it is possible to continue
#' # modifying some general aspects of the plot
#' 
#' # Define the basic plot 
#' plot <- plot_scenarios(climate_scenario_list, metric = 'Chill_Portions',
#'                        add_historic = TRUE, size = 2, shape = 3, color = 'blue',
#'                        outlier_shape = 12, historic_color = 'skyblue')
#' 
#' 
#' # Example to change the color of the climate model scale
#' 
#' plot & ggplot2::scale_fill_brewer(type = 'qual')
#' 
#' # Modify the format of axis title and axis text
#' 
#' plot & ggplot2::theme(axis.title = ggplot2::element_text(size = 14,
#'                                                          family = 'serif'),
#'                       axis.text = ggplot2::element_text(face = 'bold',
#'                                                         color = 'blue'))
#' @import patchwork
#' @export plot_scenarios

plot_scenarios <- function(scenario_list, metric, add_historic = TRUE, ..., outlier_shape = 19,
                           historic_color = "white", group_by = c("Scenario", "Year"),
                           y_axis_name = paste("Accumulated response in ", metric),
                           x_axis_name = "Year",
                           legend_title = "Climate model",
                           legend_labels = NULL,
                           panel_labels = NULL,
                           base_size = 11){
  
  
  ### Some preliminary safety checks ###
  
  # Check that the structure of the scenario list is correct
  
  assertthat::assert_that(is.list(scenario_list) &
    all(c("data", "caption") %in% unique(unlist(lapply(scenario_list, names)))),
    msg = "The input 'scenario_list' seems incorrect. Please check you include all the relevant information in a proper structure.")
  
  
  # Check the add_historic argument
  
  assertthat::assert_that(add_historic %in% c(TRUE, FALSE),
                          msg = "'add_historic' argument must be a boolean parameter, i.e. 'TRUE' or 'FALSE'")
  
  
  # Check that Scenario and Year are included in the group_by argument
  
  assertthat::assert_that(all(c("Scenario", "Year") %in% group_by),
                          msg = "The input 'group_by' seems incorrect. Please check you only include 'Scenario' and 'Year'.")
  
  
  # Check that the metric is in the data frame having the data
  
  assertthat::assert_that(metric %in% unique(unlist(lapply(scenario_list,
                                                           function(x) names(x[["data"]][[1]])))),
                          msg = "The 'metric' is not included in the data frames. Please use the tempResponse function to compute chill or heat accumulation.")
  
  # Check that panel_labels is a list with the elements Historic, Scenario and Year in the case the
  # user wants to provide customized names
  
  if (!is.null(panel_labels)){
    assertthat::assert_that(is.list(panel_labels),
                            msg = "The argument 'panel_labels' must be a list. Please provide a valid input (see documenation for more details).")
  
    assertthat::assert_that(all(c("Historic", "Scenario", "Year") %in% names(panel_labels)),
                            msg = "The argument 'panel_labels' must be a list of named objects. These objects are 'Historic', 'Scenario', and 'Year', each containing the respective labels. See examples for more details.")
  
    # Check that the panel labels list is well formatted in case the order of the plots is 
    # Scenario - Year
    
    if (group_by[1] == "Year")
      assertthat::assert_that(is.list(panel_labels[["Scenario"]]),
                              msg = "'Scenario' object in the 'panel_labels' input must be a list of named objects according to the 'Year' factor since this is your grouping variable.")
      
    if (group_by[1] == "Scenario")
      assertthat::assert_that(is.list(panel_labels[["Year"]]),
                              msg = "'Year' object in the 'panel_labels' input must be a list of named objects according to the 'Scenario' factor since this is your grouping variable.")
    
    }
  
  
  ### Stop safety checks ###
  
  
  
  
  # Generate an internal function to extract the scenarios from the scenario_list
  
  get_scenario <- function(list_one_level){
    
    scenario <- list_one_level[[which(names(list_one_level) == "caption")]]
    
    scenario_collapsed <- stringr::str_c(scenario, collapse = " ")
    
    scenario_collapsed}
  
  
  # Extract the scenario names
  
  scenarios <- unlist(lapply(scenario_list, get_scenario))
  
  # Identify the position for the Historic scenario within the scenario list
  
  position_historic <- stringr::str_which(scenarios, "Historic")
  
  
  
  # Extra check for test whether the historic data is provided in the case of add_historic == TRUE
  
  if (add_historic)
    assertthat::assert_that("historic_data" %in% names(scenario_list[[position_historic]]),
                            msg = "The observed data in the 'Historic' scenario is missing. Please provide a valid named input to the 'Historic' list. Otherwise, set the argument 'add_historic' to FALSE")

  
  # Extract the data for past simulated scenarios from the list generated by tempResponse function
  
  past_simulated <- dplyr::bind_rows(scenario_list[[position_historic]][["data"]], .id = "Ref_year")
  
  
  # Define the facet label for the Historic plot
  
  if (is.null(panel_labels)) past_simulated["Scenario"] <- "Historic" else
    past_simulated["Scenario"] <- panel_labels[["Historic"]]
  
  
  # Extract the historic data from the list
  
  if (add_historic) past_observed <- scenario_list[[position_historic]][["historic_data"]]
  
  
  # Small function to extract the data generated for future scenarios
  
  get_future_data <- function(list_one_level){
    
    data <- dplyr::bind_rows(list_one_level[["data"]], .id = "Model")
    
    data["Scenario"] <- list_one_level[["caption"]][1]
    
    data["Year"] <- list_one_level[["caption"]][2]
    
    data}
  
  
  # Apply the function to all elements in the scenario_list less the one for historic scenarios
  
  future_data <- lapply(scenario_list[-position_historic], get_future_data)
  
  # Generate one big dataframe for future projections
  
  future_data <- dplyr::bind_rows(future_data, .id = "List")
  
  # Extract the model names
  
  Models <- unique(future_data$Model)
  
  
  # Define the max and min values for the y-scale
  
  if (add_historic){
    max_y <- max(c(past_observed[[metric]],
                   past_simulated[[metric]],
                   future_data[[metric]]))
    
    min_y <- min(c(past_observed[[metric]],
                   past_simulated[[metric]],
                   future_data[[metric]]))} else {
                     
                     max_y <- max(c(past_simulated[[metric]],
                                    future_data[[metric]]))
                     
                     min_y <- min(c(past_simulated[[metric]],
                                    future_data[[metric]]))
                     
      
                   }
  
  
  # In case the models do not return negative values, we set min_y to 0
  
  if (min_y >= 0) min_y <- 0 else min_y <- min_y - 10
  
  
  # Define the labels for the legends
  
  if (is.null(legend_labels)) legend_labels <- Models
  
  
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
    ggplot2::labs(x = x_axis_name, y = y_axis_name) +
    ggplot2::facet_grid(~ Scenario) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1,
                                                       size = base_size * 0.8),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold"))
  
  # Add the historic chill calculated for all years of the dataframe
  
  if (add_historic)
    
    past_plot <- past_plot + ggplot2::geom_point(ggplot2::aes(as.character(End_year),
                                                              !!ggplot2::ensym(metric)),
                                                 data = past_observed)
  
  
  # Plots for future scenarios
  
  # Define the main and second factor for grouping the plots
  
  main <- group_by[1]
  
  second <- group_by[2]
  
  
  # Define the vars to filter the future data and obtain different future plots 
  
  vars <- unique(future_data[[main]])
  
  # Define the main grouping labels in the panels
  
  if (is.null(panel_labels)) main_labels <- vars else
    main_labels <- panel_labels[[main]]
  
  # Define a primer for the list of plots
  
  plot_list <- list()
  
  for (i in 1 : length(vars)){
    
    # Subset the temporary dara according to the  main grouping var i
    temp_data <- dplyr::filter(future_data, !!ggplot2::ensym(main) == vars[i])
    
    # Define the labels for the second level of panels
    if (!is.null(panel_labels)) secondary_labels <- panel_labels[[second]][[i]] else
      secondary_labels <- unique(temp_data[[second]])
    
    plot_list[[i]] <- ggplot2::ggplot() +
      ggplot2::geom_boxplot(ggplot2::aes(factor(Model, levels = Models),
                                         !!ggplot2::ensym(metric),
                                         fill = factor(Model, levels = Models,
                                                       labels = legend_labels)),
                            data = temp_data,
                            outlier.shape = outlier_shape, size = 0.3, outlier.size = 1) +
      ggplot2::scale_x_discrete(labels = NULL,
                                expand = ggplot2::expansion(add = 1)) +
      ggplot2::scale_y_continuous(limits = c(min_y, round(max_y + 10)),
                                  expand = ggplot2::expansion(add = 0),
                                  labels = NULL) +
      ggplot2::guides(fill = ggplot2::guide_legend(title.position = 'top', title.hjust = 0.5)) +
      ggplot2::labs(x = NULL, y = NULL, fill = legend_title,
                    subtitle = main_labels[i]) +
      ggplot2::facet_wrap(facets = factor(temp_data[[second]],
                                          labels = secondary_labels), nrow = 1) +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(margin = ggplot2::margin(0, 0, 0, 0, "cm")),
                     legend.position = "bottom",
                     legend.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
                     legend.background = ggplot2::element_rect(),
                     strip.background = ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(face = "bold"),
                     legend.box.spacing = ggplot2::unit(0, "cm"),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5, vjust = -1, size = base_size * 1.05,
                                                           face = "bold"))
    
    
    
  }
  
  
  # Merge plots into one
  
  # Define the relative width according to the number of scenarios used.
  
  widths <- NULL
  
  for (var in vars){
    
    width <-length(unique(future_data[future_data[[main]] == var, second]))
    
    widths <- c(widths, width)}
  
  
  # Use the patchwork package to merge the plots
  
  plot <- ((past_plot + plot_list) +
             patchwork::plot_layout(guides = "collect",
                                    widths = c(1, widths)) &
             ggplot2::theme(legend.position = "bottom"))
  
  return(plot)}
