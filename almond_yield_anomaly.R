#' A function to return the minimum, maximum, and average almond yield anomaly over a given time series.
#' This function was created for EDS 230, Environmental Modeling on April 13, 2025.
#' @author Rachel Swick and Ryan Green
#'
#' @param filepath The file path to a `txt` file with climate data for a time series.
#' The climate data file must include the following columns:
#' @field day day of the month the climate data was recorded on, numeric 
#' @field month number of the month the climate data was recorded in, numeric
#' @field year year the climate data was recorded in, numeric
#' @field tmin_c daily minimum temperature in degrees Celsius, numeric
#' @field precip amount of precipitation per day in millimeters, numeric
#'
#' @return
#' @export summary_yield_anomaly A table with the minimum, maximum, and average almond yield anomaly for a given time series
#'
#' @examples
#' almond_yield_anomaly("data/clim.txt")

almond_yield_anomaly <- function(filepath) {
  
  # Read in the data
  data <- read.table(filepath, header = TRUE, sep = " ")
  
  # Aggregate February data: get the minimum temp (tmin_c) for February per year
  data_february <- data %>%
    group_by(month, year) %>%
    filter(month == 2) %>% 
    summarise(mintemp = min(tmin_c, na.rm = TRUE)) %>% 
    ungroup()
  
  # Aggregate January data: sum of precipitation for January per year
  data_january <- data %>%
    group_by(month, year) %>%
    filter(month == 1) %>% 
    summarise(total_precip = sum(precip, na.rm = TRUE)) %>% 
    ungroup()
  
  # Join the January and February data by the year
  data_clim <- left_join(x = data_january, y = data_february, by = "year")
  
  # Compute the yield anomaly using the formula
  data_clim <- data_clim %>%
    mutate(yield_anomaly = -0.015 * mintemp - 0.0046 * (mintemp^2) - 0.07 * total_precip + 0.0043 * (total_precip^2) + 0.28)
  
  # Summarize the yield anomaly statistics
  summary_yield_anomaly <- data_clim %>%
    summarise(
      min_yield = min(yield_anomaly, na.rm = TRUE),
      mean_yield = mean(yield_anomaly, na.rm = TRUE),
      max_yield = max(yield_anomaly, na.rm = TRUE))
  
  # Print the summary statistics
  return(summary_yield_anomaly)
}


