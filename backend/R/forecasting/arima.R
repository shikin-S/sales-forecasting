forecast_arima <- function(data, horizon) {
  if (nrow(data) < 3) return(forecast_error("Insufficient historical data for ARIMA forecasting.", "insufficient_data"))
  series <- stats::ts(data$sales, frequency = if (detect_frequency(data) == "monthly") 12 else 1)
  model <- tryCatch({
    if (requireNamespace("forecast", quietly = TRUE)) forecast::auto.arima(series, seasonal = detect_frequency(data) == "monthly") else stats::arima(series, order = c(1, 1, 1))
  }, error = function(error) NULL)
  if (is.null(model)) return(forecast_error("ARIMA model fitting failed."))
  if (requireNamespace("forecast", quietly = TRUE)) {
    prediction <- tryCatch(forecast::forecast(model, h = horizon), error = function(error) NULL)
    if (is.null(prediction)) return(forecast_error("ARIMA forecast generation failed."))
    return(standard_forecast_result("ARIMA", data, horizon, as.numeric(prediction$mean), prediction$lower[, 1], prediction$upper[, 1]))
  }
  prediction <- tryCatch(stats::predict(model, n.ahead = horizon), error = function(error) NULL)
  if (is.null(prediction)) return(forecast_error("ARIMA forecast generation failed."))
  standard_forecast_result("ARIMA", data, horizon, as.numeric(prediction$pred), prediction$pred - 1.96 * prediction$se, prediction$pred + 1.96 * prediction$se)
}