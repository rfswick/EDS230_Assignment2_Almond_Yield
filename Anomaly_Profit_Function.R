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
#' almond_yield_year("data/clim.txt")

almond_yield_year <- function(filepath) {
  
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
    mutate(yield_anomaly = -0.015 * mintemp - 0.0046 * 
             (mintemp^2) - 0.07 * total_precip + 0.0043 * (total_precip^2) + 0.28) %>% 
    select(year, mintemp, total_precip, yield_anomaly)
  
  anomaly <- function(mintemp, total_precip) {
    anomaly_value = -0.015 * mintemp - 0.0046 * (mintemp^2) - 0.07 * total_precip + 0.0043 * (total_precip^2) + 0.28
    return(data.frame(
      rand_mintemp = mintemp,  # Renamed mintemp
      rand_total_precip = total_precip,  # Renamed total_precip
      yield_anomaly = anomaly_value
    ))
  }
  
  nsamples <- 100
  deviation <- 0.20
  avg_temp <- mean(data_clim$mintemp, na.rm = TRUE)
  avg_rain <- mean(data_clim$total_precip, na.rm = TRUE)
  
  mintemp <- runif(
    min = avg_temp - deviation * avg_temp,
    max = avg_temp + deviation * avg_temp, n = nsamples
  )
  
  total_precip <- runif(
    min = avg_rain - deviation * avg_rain,
    max = avg_rain + deviation * avg_rain, n = nsamples
  )
  
  parms <- cbind.data.frame(mintemp, total_precip)
  
  all_results <- list()
  
  for(year in unique(data_clim$year)) {
    
    # Filter data for the specific year
    year_data <- data_clim %>% filter(year == year)
    
    # Apply the anomaly function to generate results for each parameter sample
    results <- parms %>%
      rowwise() %>%
      mutate(anomaly_result = list(anomaly(mintemp, total_precip))) %>%
      unnest(anomaly_result)
    
    # Add the year to the results
    year_results <- results %>%
      mutate(year = year)
    
    # Store results in the list
    all_results[[as.character(year)]] <- year_results
  }
  
  combined_results <- bind_rows(all_results)
  
  anomaly_by_year <- ggplot(combined_results, aes(x = as.factor(year), y = yield_anomaly, fill = as.factor(year))) +
    geom_boxplot() +
    labs(y = "Yield Anomaly", x = "Year") +
    theme_minimal() +
    ggtitle("Yield Anomaly by Year with Uncertainty") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  
  # 
  return(list(results = combined_results, plot = anomaly_by_year))
}

almond_yield_year("data/clim.txt")
