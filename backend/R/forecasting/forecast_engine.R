model_functions <- list(moving_average = forecast_moving_average, linear_regression = forecast_linear_regression, exponential_smoothing = forecast_exponential_smoothing, holt_winters = forecast_holt_winters, arima = forecast_arima)

generate_forecast <- function(data, model, period) {
  prepared <- prepare_forecast_data(data)
  if (is.list(prepared) && !is.null(prepared$model_status)) return(prepared)
  frequency <- detect_frequency(prepared)
  horizon <- period_to_horizon(period, frequency)
  if (frequency == "insufficient" || is.null(horizon)) return(forecast_error("Invalid forecast period or insufficient dated sales history.", "insufficient_data"))
  model <- tolower(gsub("[^a-z]", "_", model))
  if (model == "automatic") {
    candidates <- c("moving_average", "linear_regression", "exponential_smoothing", "arima")
    split <- floor(nrow(prepared) * 0.8)
    if (split < 2 || split >= nrow(prepared)) return(forecast_error("Insufficient historical data for automatic model comparison.", "insufficient_data"))
    training <- prepared[seq_len(split), , drop = FALSE]
    validation <- prepared[(split + 1):nrow(prepared), , drop = FALSE]
    validation_horizon <- nrow(validation)
    validation_results <- lapply(candidates, function(name) tryCatch(model_functions[[name]](training, validation_horizon), error = function(error) forecast_error("Model failed.")))
    successful <- which(vapply(validation_results, function(result) identical(result$model_status, "success"), logical(1)))
    if (length(successful) == 0) return(forecast_error("No forecasting model could be fitted."))
    scores <- vapply(validation_results[successful], function(result) mean((validation$sales - result$forecast$predicted_sales)^2, na.rm = TRUE), numeric(1))
    best <- successful[which.min(scores)]
    final_result <- model_functions[[candidates[best]]](prepared, horizon)
    if (!identical(final_result$model_status, "success")) return(final_result)
    final_result$model <- paste("Automatic Best Model:", final_result$model)
    final_result$validation_rmse <- sqrt(min(scores))
    return(final_result)
  }
  if (!model %in% names(model_functions)) return(forecast_error("Invalid forecasting model."))
  tryCatch(model_functions[[model]](prepared, horizon), error = function(error) forecast_error("Unable to generate forecast."))
}