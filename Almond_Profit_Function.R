#' A function to return the min, max, and mean profit based on almond yield anomaly.
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
#' @param almond_price the current cost of one ton of almonds, default of 4240 dollars per ton
#' @param farm_acreage almond farm size, default of 100 acres
#' @param acre_yield the annual yield of one acre of almond trees, default of 1.2 tons per acre
#'
#' @return
#' @export profit A table with the minimum, maximum, and average profit based on farm size, and the almond yield anomaly for a given time series.
#'
#' @examples
#' almond_profit(almond_price = 4240, farm_acreage = 100, acre_yield = 1.2, filepath = "data/clim.txt")

almond_profit <- function(almond_price = 4240, farm_acreage = 100, acre_yield = 1.2, filepath) {
  source("almond_yield_anomaly.R")
  profit = almond_price * farm_acreage * (acre_yield + almond_yield_anomaly(filepath))
  return(profit)
}


