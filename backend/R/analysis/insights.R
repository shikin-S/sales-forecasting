generate_business_insights <- function(analysis_results, forecast_results = NULL, evaluation_results = NULL) {
  insights <- list()
  trend <- analysis_results$trend$direction
  insights$sales_trend <- switch(trend, Increasing = "Historical sales are showing an increasing trend.", Decreasing = "Historical sales are showing a decreasing trend.", Stable = "Historical sales are relatively stable.", "Insufficient historical data to determine a reliable sales trend.")

  products <- analysis_results$product_analysis$top_products
  if (isTRUE(analysis_results$product_analysis$available) && length(products) > 0) insights$top_product <- paste0(products[[1, "product"]], " generated the highest sales during the analyzed period.")
  categories <- analysis_results$category_analysis$category_sales
  if (isTRUE(analysis_results$category_analysis$available) && length(categories) > 0) insights$top_category <- paste0(categories[[1, "category"]], " was the highest-performing category by sales.")
  time <- analysis_results$time_analysis
  if (isTRUE(time$available)) insights$best_month <- paste0(time$best_month, " recorded the highest historical sales.")
  regions <- analysis_results$regional_analysis$region_sales
  if (isTRUE(analysis_results$regional_analysis$available) && length(regions) > 0) insights$regional_performance <- paste0(regions[[1, "region"]], " recorded the highest regional sales.")
  if (!is.null(forecast_results) && isTRUE(forecast_results$success)) insights$forecast <- "Forecast results may support future inventory planning." else insights$forecast <- "No forecast generated yet."
  if (!is.null(evaluation_results) && isTRUE(evaluation_results$success) && isTRUE(evaluation_results$available)) insights$model_performance <- paste0(evaluation_results$best_model$name, " was selected using the lowest valid ", evaluation_results$best_model$metric, ".")
  if (!is.null(evaluation_results) && isTRUE(evaluation_results$success) && !isTRUE(evaluation_results$available)) insights$model_performance <- evaluation_results$message
  insights
}