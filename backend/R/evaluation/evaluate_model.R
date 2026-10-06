evaluate_model <- function(data, model, test_size = NULL) {
  prepared <- prepare_forecast_data(data)
  if (is.list(prepared) && !is.null(prepared$model_status)) return(list(model = model, status = "insufficient_data", message = prepared$message))
  split <- create_train_test_split(prepared, test_size)
  if (nrow(split$training) < 2 || !nrow(split$testing)) return(list(model = model, status = "insufficient_data", message = "Insufficient historical observations for evaluation."))
  forecast_result <- tryCatch({
    if (!model %in% names(model_functions)) return(forecast_error("Invalid forecasting model."))
    model_functions[[model]](split$training, nrow(split$testing))
  }, error = function(error) NULL)
  if (is.null(forecast_result) || !identical(forecast_result$model_status, "success")) return(list(model = model, status = if (!is.null(forecast_result$model_status) && identical(forecast_result$model_status, "insufficient_data")) "insufficient_data" else "failed", message = if (!is.null(forecast_result$message)) forecast_result$message else "Model evaluation failed."))
  predicted <- as.numeric(forecast_result$forecast$predicted_sales); actual <- split$testing$sales; dates <- split$testing$date
  if (!validate_predictions(predicted, actual, dates)) return(list(model = model, status = "failed", message = "Invalid model predictions."))
  metrics <- calculate_all_metrics(actual, predicted)
  list(model = model, status = "success", mae = metrics$mae, rmse = metrics$rmse, mape = metrics$mape, zero_actuals_excluded = metrics$zero_actuals_excluded, training_size = nrow(split$training), testing_size = nrow(split$testing), actual_vs_predicted = data.frame(date = format(dates, "%Y-%m-%d"), actual = actual, predicted = predicted))
}