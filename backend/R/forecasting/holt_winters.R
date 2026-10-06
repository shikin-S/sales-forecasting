forecast_holt_winters <- function(data, horizon) {
  frequency <- detect_frequency(data)
  seasonal_period <- switch(frequency, monthly = 12, weekly = 52, daily = 7, hourly = 24, yearly = NA_integer_, 1L)
  if (is.na(seasonal_period)) return(forecast_error("Seasonal Holt-Winters is unavailable for annual observations.", "insufficient_data"))
  if (nrow(data) < seasonal_period * 2) return(forecast_error("Insufficient historical data for seasonal Holt-Winters forecasting.", "insufficient_data"))
  series <- stats::ts(data$sales, frequency = seasonal_period)
  if (requireNamespace("forecast", quietly = TRUE)) {
    model <- tryCatch(forecast::hw(series, h = horizon, seasonal = "additive"), error = function(error) NULL)
    if (is.null(model)) return(forecast_error("Holt-Winters model fitting failed."))
    return(standard_forecast_result("Holt-Winters", data, horizon, as.numeric(model$mean), model$lower[, 1], model$upper[, 1]))
  }
  model <- tryCatch(stats::HoltWinters(series, seasonal = "additive"), error = function(error) NULL)
  if (is.null(model)) return(forecast_error("Holt-Winters model fitting failed."))
  prediction <- stats::predict(model, n.ahead = horizon, prediction.interval = TRUE)
  standard_forecast_result("Holt-Winters", data, horizon, as.numeric(prediction[, "fit"]), as.numeric(prediction[, "lwr"]), as.numeric(prediction[, "upr"]))
}