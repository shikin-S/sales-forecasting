forecast_linear_regression <- function(data, horizon) {
  if (nrow(data) < 2) return(forecast_error("Insufficient historical data for linear regression forecasting.", "insufficient_data"))
  model <- tryCatch(stats::lm(sales ~ time, data.frame(sales = data$sales, time = seq_len(nrow(data)))), error = function(error) NULL)
  if (is.null(model)) return(forecast_error("Linear regression model fitting failed."))
  predictions <- predict(model, newdata = data.frame(time = (nrow(data) + 1):(nrow(data) + horizon)))
  standard_forecast_result("Linear Regression", data, horizon, predictions)
}