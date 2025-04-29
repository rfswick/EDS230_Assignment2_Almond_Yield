#' A function to return the almond yield anomalies year over year for a given time series.
#' This function was created for EDS 230, Environmental Modeling on April 21, 2025.
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
#' @export results A table with the almond yield anomaly results from the informal sensitivity analysis
#' @export plot A plot of the almond yield anomalies with uncertainty
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
  
  # Create a sub-function to calculate anomalies for the informal sensitvity analysis
  compute_anomaly <- function(temp, precip) {
    -0.015 * temp - 0.0046 * temp^2 - 0.07 * precip + 0.0043 * precip^2 + 0.28
  }
  
  # Number of samples
  nsamples <- 100
  
  # 20% variation
  deviation <- 0.20
  
  # Set a list to store anomaly values from the sensitivity analysis
  all_anomalies <- list()
  
  
  for (i in 1:nrow(data_clim)) {
    year_row <- data_clim[i, ]
    year <- year_row$year
    tmin <- year_row$mintemp
    precip <- year_row$total_precip
    
    # Create uncertainities around that year's values
    tmin_samples <- runif(nsamples - 1, tmin * (1 - deviation), tmin * (1 + deviation))
    precip_samples <- runif(nsamples - 1, precip * (1 - deviation), precip * (1 + deviation))
    
    # Include original values as one sample
    tmin_samples <- c(tmin_samples, tmin)
    precip_samples <- c(precip_samples, precip)
    
    # Calculate anomalies
    anomalies <- compute_anomaly(tmin_samples, precip_samples)
    
    # Store with year
    all_anomalies[[as.character(year)]] <- data.frame(year = year, yield_anomaly = anomalies)
  }
  
  combined_results <- bind_rows(all_anomalies)
  
  # Plot
  anomaly_by_year <- ggplot(combined_results, aes(x = as.factor(year), y = yield_anomaly)) +
    geom_boxplot(fill = "steelblue", alpha = 0.7) +
    labs(y = "Yield Anomaly", x = "Year") +
    ggtitle("Yield Anomaly by Year with Uncertainty") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Return the results of the informal sensitivity analysis and the graph of anomalies year over year
  return(list(results = head(combined_results), plot = anomaly_by_year))
}
