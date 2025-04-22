#' A function to return the 
#' This function was created for EDS 230, Environmental Modeling on April 21, 2025.
#' @author Rachel Swick and Ryan Green
#'
#' @param filepath The file path to a `txt` file with climate data for a time series.
#' 
#' @field almond_price the current cost of one ton of almonds, at 2.12 dollars per pound
#' @field farm_acreage almond farm size, set at a default 100 acres
#' @field acre_yield the annual yield of one acre of almond trees 
#'
#' @return
#' @export profit A table with the minimum, maximum, and average almond yield anomaly for a given time series
#'
#' @examples
#' almond_profit(filepath = "data/clim.txt")

almond_profit <- function(almond_price = 4240, farm_acreage = 100, acre_yield = 1.2, filepath) {
  profit = almond_price * farm_acreage * (acre_yield + almond_yield_anomaly(filepath))
  return(profit)
}


