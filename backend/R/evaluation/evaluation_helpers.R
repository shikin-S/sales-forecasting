create_train_test_split <- function(data, test_size = NULL) {
  data <- data[order(data$date), , drop = FALSE]
  if (is.null(test_size)) test_size <- max(1, floor(nrow(data) * 0.2))
  test_size <- min(as.integer(test_size), nrow(data) - 2)
  split_at <- nrow(data) - test_size
  list(training = data[seq_len(split_at), , drop = FALSE], testing = data[(split_at + 1):nrow(data), , drop = FALSE])
}

validate_predictions <- function(predictions, actual, dates) {
  length(predictions) == length(actual) && length(actual) == length(dates) && length(actual) > 0 && all(is.finite(predictions)) && all(is.finite(actual)) && all(!is.na(dates))
}

select_best_model <- function(results) {
  valid <- results[vapply(results, function(result) identical(result$status, "success") && is.finite(result$mae) && is.finite(result$rmse), logical(1))]
  if (!length(valid)) return(list(name = NULL, metric = NULL, reason = "No forecasting model could be evaluated with the available data."))
  mape_valid <- valid[vapply(valid, function(result) is.finite(result$mape), logical(1))]
  if (length(mape_valid)) {
    scores <- vapply(mape_valid, function(result) result$mape, numeric(1)); metric <- "MAPE"; reason <- "Lowest valid MAPE among evaluated models"
  } else {
    scores <- vapply(valid, function(result) result$rmse, numeric(1)); metric <- "RMSE"; reason <- "Lowest valid RMSE because MAPE was unavailable"
  }
  best_name <- if (metric == "MAPE") mape_valid[[which.min(scores)]]$model else valid[[which.min(scores)]]$model
  list(name = best_name, metric = metric, reason = reason)
}