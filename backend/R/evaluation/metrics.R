metric_pairs <- function(actual, predicted) {
	actual <- as.numeric(actual); predicted <- as.numeric(predicted)
	valid <- is.finite(actual) & is.finite(predicted)
	list(actual = actual[valid], predicted = predicted[valid])
}

calculate_mae <- function(actual, predicted) {
	pairs <- metric_pairs(actual, predicted)
	if (!length(pairs$actual)) return(NA_real_)
	mean(abs(pairs$actual - pairs$predicted))
}

calculate_rmse <- function(actual, predicted) {
	pairs <- metric_pairs(actual, predicted)
	if (!length(pairs$actual)) return(NA_real_)
	sqrt(mean((pairs$actual - pairs$predicted)^2))
}

calculate_mape <- function(actual, predicted) {
	pairs <- metric_pairs(actual, predicted)
	non_zero <- pairs$actual != 0
	if (!any(non_zero)) return(NA_real_)
	mean(abs((pairs$actual[non_zero] - pairs$predicted[non_zero]) / pairs$actual[non_zero])) * 100
}

calculate_all_metrics <- function(actual, predicted) {
	list(mae = calculate_mae(actual, predicted), rmse = calculate_rmse(actual, predicted), mape = calculate_mape(actual, predicted), zero_actuals_excluded = sum(as.numeric(actual) == 0, na.rm = TRUE))
}
