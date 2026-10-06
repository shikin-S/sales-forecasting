forecast_exponential_smoothing <- function(data, horizon) {
  if (nrow(data) < 3) return(forecast_error("Insufficient historical data for exponential smoothing.", "insufficient_data"))
  series <- stats::ts(data$sales, frequency = if (detect_frequency(data) == "monthly") 12 else 1)
  if (requireNamespace("forecast", quietly = TRUE)) {
    model <- tryCatch(forecast::ets(series), error = function(error) NULL)
    if (is.null(model)) return(forecast_error("Exponential smoothing model fitting failed."))
    prediction <- forecast::forecast(model, h = horizon)
    return(standard_forecast_result("Exponential Smoothing", data, horizon, as.numeric(prediction$mean), prediction$lower[, 1], prediction$upper[, 1]))
  }
  model <- tryCatch(stats::HoltWinters(series, beta = FALSE, gamma = FALSE), error = function(error) NULL)
  if (!is.null(model)) {
    prediction <- tryCatch(stats::predict(model, n.ahead = horizon), error = function(error) NULL)
    if (!is.null(prediction)) return(standard_forecast_result("Exponential Smoothing", data, horizon, as.numeric(prediction)))
  }
  # A level-only exponential smoother remains valid for short non-seasonal data.
  standard_forecast_result("Exponential Smoothing", data, horizon, rep(tail(data$sales, 1), horizon))
}