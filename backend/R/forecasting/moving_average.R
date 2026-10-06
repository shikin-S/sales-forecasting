forecast_moving_average <- function(data, horizon) {
  if (nrow(data) < 2) return(forecast_error("Insufficient historical data for moving average forecasting.", "insufficient_data"))
  window <- min(if (detect_frequency(data) == "daily") 7 else 3, nrow(data))
  value <- mean(tail(data$sales, window), na.rm = TRUE)
  standard_forecast_result("Moving Average", data, horizon, rep(value, horizon))
}