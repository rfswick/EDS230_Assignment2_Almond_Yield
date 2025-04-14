#' A function to report on my dog's weight
#'
#' @param filepath The file path to a `txt` file with climate data for a time series. 
#' 
#'
#' @return
#' @export summary_yield_anomaly A table with the minimum, maximum, and average almond yield anomaly for a given time series
#'
#' @examples
#' almond_yield_anomaly("data/clim.txt")

almond_yield_anomaly <- function(filepath) {
  
  # This function created by Rachel Swick and Ryan Green
  # For EDS 230
  # 4/13/25
  
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


