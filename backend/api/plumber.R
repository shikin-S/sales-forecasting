# Plumber API for dataset upload and basic validation.

library(plumber)
library(jsonlite)

resolve_project_root <- function() {
  working_directory <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  candidates <- unique(c(
    working_directory,
    normalizePath(file.path(working_directory, ".."), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(working_directory, "..", ".."), winslash = "/", mustWork = FALSE),
    if (basename(working_directory) == "backend") normalizePath(dirname(working_directory), winslash = "/", mustWork = FALSE) else NULL,
    if (basename(working_directory) == "api") normalizePath(file.path(working_directory, "..", ".."), winslash = "/", mustWork = FALSE) else NULL
  ))

  for (candidate in candidates) {
    if (is.null(candidate) || !nzchar(candidate)) next
    if (dir.exists(file.path(candidate, "backend")) && dir.exists(file.path(candidate, "data"))) {
      return(candidate)
    }
  }

  stop("Unable to resolve the project root from the current working directory. Start the API from the project root and run plumb('backend/api/plumber.R').", call. = FALSE)
}

source_backend_file <- function(relative_path) {
  full_path <- file.path(project_root, relative_path)
  if (!file.exists(full_path)) {
    stop(sprintf("Missing required backend file: %s", full_path), call. = FALSE)
  }
  source(full_path)
}

project_root <- resolve_project_root()
backend_directory <- file.path(project_root, "backend")
r_directory <- file.path(backend_directory, "R")
data_directory <- file.path(project_root, "data")
raw_data_directory <- file.path(data_directory, "raw")
processed_directory <- file.path(data_directory, "processed")
reports_directory <- file.path(project_root, "reports")

# Create required project folders without changing the global working directory.
dir.create(raw_data_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_directory, recursive = TRUE, showWarnings = FALSE)

message("Plumber API startup diagnostics:")
message("  Project root: ", project_root)
message("  R directory: ", r_directory)
message("  Processed data directory: ", processed_directory)

source_backend_file("backend/R/preprocessing/date_parser.R")
source_backend_file("backend/R/preprocessing/column_detector.R")
source_backend_file("backend/R/preprocessing/schema_mapper.R")
source_backend_file("backend/R/preprocessing/preprocess.R")
source_backend_file("backend/R/analysis/analysis.R")
source_backend_file("backend/R/analysis/insights.R")
source_backend_file("backend/R/forecasting/forecast.R")
source_backend_file("backend/R/forecasting/forecast_helpers.R")
source_backend_file("backend/R/forecasting/moving_average.R")
source_backend_file("backend/R/forecasting/linear_regression.R")
source_backend_file("backend/R/forecasting/exponential_smoothing.R")
source_backend_file("backend/R/forecasting/holt_winters.R")
source_backend_file("backend/R/forecasting/arima.R")
source_backend_file("backend/R/forecasting/forecast_engine.R")
source_backend_file("backend/R/evaluation/metrics.R")
source_backend_file("backend/R/evaluation/evaluation_helpers.R")
source_backend_file("backend/R/evaluation/evaluate_model.R")
source_backend_file("backend/R/evaluation/compare_models.R")
source_backend_file("backend/R/utils/helpers.R")

error_response <- function(res, message, status = 400) {
  res$status <- status
  list(success = FALSE, message = message)
}

make_processed_filename <- function() {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("processed_sales_", timestamp, ".csv")
  counter <- 1
  while (file.exists(file.path(processed_directory, filename))) {
    filename <- paste0("processed_sales_", timestamp, "_", counter, ".csv")
    counter <- counter + 1
  }
  filename
}

#* Allow local frontend requests during development.
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (identical(req$REQUEST_METHOD, "OPTIONS")) {
    res$status <- 204
    return(list())
  }
  plumber::forward()
}

#* Upload, validate, preprocess, and summarize a CSV dataset.
#* @post /upload
#* @serializer json list(auto_unbox = TRUE)
function(req, res) {
  uploaded_file <- req$body$file
  if (is.null(uploaded_file) && !is.null(req$files)) uploaded_file <- req$files$file
  if (is.null(uploaded_file) && !is.null(req$files) && length(req$files) > 0) uploaded_file <- req$files[[1]]

  uploaded_path <- NULL
  if (is.list(uploaded_file) && !is.null(uploaded_file$datapath)) {
    uploaded_path <- uploaded_file$datapath
  } else {
    uploaded_content <- uploaded_file
    if (is.list(uploaded_file)) {
      uploaded_content <- uploaded_file$parsed
      if (is.null(uploaded_content)) uploaded_content <- uploaded_file$content
    }
    if (!is.null(uploaded_content) && (is.raw(uploaded_content) || is.character(uploaded_content))) {
      uploaded_path <- tempfile(fileext = ".csv")
      if (is.raw(uploaded_content)) writeBin(uploaded_content, uploaded_path) else writeLines(uploaded_content, uploaded_path, useBytes = TRUE)
      on.exit(unlink(uploaded_path), add = TRUE)
    }
  }

  if (is.null(uploaded_path) || !file.exists(uploaded_path)) {
    return(error_response(res, "Unable to read the dataset."))
  }
  if (is.na(file.info(uploaded_path)$size) || file.info(uploaded_path)$size == 0) {
    return(error_response(res, "The uploaded dataset contains no records."))
  }

  dataset <- tryCatch(
    readr::read_csv(uploaded_path, show_col_types = FALSE, name_repair = "minimal"),
    error = function(error) NULL
  )
  if (is.null(dataset)) return(error_response(res, "Unable to read the dataset."))
  if (ncol(dataset) == 0) return(error_response(res, "Unable to read the dataset."))
  if (nrow(dataset) == 0) return(error_response(res, "The uploaded dataset contains no records."))
  if (any(is.na(names(dataset))) || any(trimws(names(dataset)) == "")) {
    return(error_response(res, "Unable to read the dataset."))
  }
  preprocessing <- tryCatch(preprocess_data(dataset), error = function(error) error)
  if (inherits(preprocessing, "error")) return(error_response(res, conditionMessage(preprocessing), 422))

  processed_filename <- make_processed_filename()
  processed_path <- file.path(processed_directory, processed_filename)
  saved <- tryCatch({
    readr::write_csv(preprocessing$cleaned_data, processed_path, na = "")
    TRUE
  }, error = function(error) FALSE)
  if (!saved) return(error_response(res, "Unable to process the uploaded dataset.", 500))

  list(
    success = TRUE,
    message = "Dataset successfully processed",
    dataset = list(
      original_rows = preprocessing$processing_summary$original_rows,
      original_columns = preprocessing$processing_summary$original_columns,
      final_rows = preprocessing$processing_summary$final_rows,
      final_columns = preprocessing$processing_summary$final_columns,
      dataset_type = preprocessing$profile$dataset_type
    ),
    preprocessing = preprocessing$processing_summary,
    detected_columns = preprocessing$detected_columns,
    field_confidence = preprocessing$field_confidence,
    mapped_fields = preprocessing$mapped_fields,
    profile = preprocessing$profile,
    date_column = preprocessing$date_column,
    warnings = preprocessing$warnings,
    processed_file = processed_filename
  )
}

latest_processed_file <- function() {
  files <- list.files(processed_directory, pattern = "^processed_sales_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(NULL)
  files[which.max(file.info(files)$mtime)]
}

evaluate_dataset_models <- function(dataset) {
  model_names <- c(moving_average = "Moving Average", linear_regression = "Linear Regression", exponential_smoothing = "Exponential Smoothing", holt_winters = "Holt-Winters", arima = "ARIMA")
  prepared <- tryCatch(prepare_forecast_data(dataset), error = function(error) error)
  reason <- if (inherits(prepared, "error")) {
    conditionMessage(prepared)
  } else if (is.list(prepared) && !is.null(prepared$model_status)) {
    prepared$message
  } else if (nrow(prepared) < 3) {
    "At least three dated sales observations are required for chronological model evaluation."
  } else {
    NULL
  }

  if (!is.null(reason)) {
    models <- lapply(names(model_names), function(name) list(model = unname(model_names[[name]]), status = "insufficient_data", message = reason, mae = NULL, rmse = NULL, mape = NULL, actual_vs_predicted = list()))
    return(list(success = TRUE, available = FALSE, message = reason, evaluation = list(training_size = 0L, testing_size = 0L), models = models, best_model = list(name = "Not available", metric = NULL, reason = reason)))
  }

  test_size <- max(1, floor(nrow(prepared) * 0.2))
  results <- lapply(names(model_names), function(model_name) evaluate_model(dataset, model_name, test_size))
  valid_results <- results[vapply(results, function(result) identical(result$status, "success"), logical(1))]
  comparison_table <- lapply(results, function(result) {
    model_index <- match(result$model, names(model_names))
    list(
      model = if (!is.na(model_index)) unname(model_names[[model_index]]) else result$model,
      status = result$status,
      mae = if (!is.null(result$mae)) result$mae else NULL,
      rmse = if (!is.null(result$rmse)) result$rmse else NULL,
      mape = if (!is.null(result$mape)) result$mape else NULL,
      message = if (!is.null(result$message)) result$message else NULL,
      zero_actuals_excluded = if (!is.null(result$zero_actuals_excluded)) result$zero_actuals_excluded else NULL,
      actual_vs_predicted = if (!is.null(result$actual_vs_predicted)) result$actual_vs_predicted else list()
    )
  })
  if (!length(valid_results)) {
    reason <- "No forecasting model could be evaluated with the available observations."
    return(list(success = TRUE, available = FALSE, message = reason, evaluation = list(training_size = nrow(prepared) - test_size, testing_size = test_size), models = comparison_table, best_model = list(name = "Not available", metric = NULL, reason = reason)))
  }

  comparison <- compare_models(results)
  best_index <- match(comparison$best_model$name, names(model_names))
  best_label <- if (!is.na(best_index)) unname(model_names[[best_index]]) else "Not available"
  list(success = TRUE, available = TRUE, evaluation = list(training_size = nrow(prepared) - test_size, testing_size = test_size), models = comparison_table, best_model = list(name = best_label, metric = comparison$best_model$metric, reason = comparison$best_model$reason))
}

#* Analyze the latest cleaned sales dataset.
#* @get /analysis
#* @serializer json list(auto_unbox = TRUE)
function(req, res) {
  processed_path <- latest_processed_file()
  if (is.null(processed_path)) return(error_response(res, "No processed dataset available. Please upload and process a sales dataset first.", 404))
  dataset <- tryCatch(readr::read_csv(processed_path, show_col_types = FALSE, name_repair = "minimal"), error = function(error) NULL)
  if (is.null(dataset) || nrow(dataset) == 0 || ncol(dataset) == 0) return(error_response(res, "Dataset could not be analyzed. Please check the processed CSV.", 422))
  analysis <- tryCatch(analyze_sales_data(dataset), error = function(error) NULL)
  if (is.null(analysis)) return(error_response(res, "An error occurred while analyzing the dataset.", 500))
  analysis$processed_file <- basename(processed_path)
  analysis
}

#* Generate a forecast from the latest cleaned dataset.
#* @post /forecast
#* @serializer json list(auto_unbox = TRUE)
function(req, res) {
  request <- req$body
  model <- if (is.list(request)) request$model else NULL
  period <- if (is.list(request)) request$period else NULL
  if (is.null(model) || !nzchar(as.character(model))) return(error_response(res, "Please select a forecasting model.", 400))
  if (is.null(period) || !is.finite(suppressWarnings(as.numeric(period))) || as.numeric(period) <= 0) return(error_response(res, "Invalid forecast period.", 400))
  processed_path <- latest_processed_file()
  if (is.null(processed_path)) return(error_response(res, "No processed dataset available. Please upload and process a sales dataset first.", 404))
  dataset <- tryCatch(readr::read_csv(processed_path, show_col_types = FALSE, name_repair = "minimal"), error = function(error) NULL)
  if (is.null(dataset)) return(error_response(res, "Unable to load the processed dataset.", 422))
  result <- generate_forecast(dataset, as.character(model), as.numeric(period))
  if (!identical(result$model_status, "success")) {
    res$status <- if (identical(result$model_status, "insufficient_data")) 422 else 400
    return(list(success = FALSE, message = result$message, model_status = result$model_status))
  }
  list(success = TRUE, model = result$model, frequency = result$frequency, horizon = result$horizon, historical = result$historical, forecast = result$forecast, model_status = result$model_status, negative_adjustments = result$negative_adjustments, processed_file = basename(processed_path))
}

#* Evaluate all forecasting models on a chronological holdout period.
#* @get /evaluation
#* @serializer json list(auto_unbox = TRUE)
function(req, res) {
  processed_path <- latest_processed_file()
  if (is.null(processed_path)) return(error_response(res, "No processed dataset available. Please upload and process a sales dataset first.", 404))
  dataset <- tryCatch(readr::read_csv(processed_path, show_col_types = FALSE, name_repair = "minimal"), error = function(error) NULL)
  if (is.null(dataset)) return(error_response(res, "Unable to load the processed dataset.", 422))
  result <- evaluate_dataset_models(dataset)
  result$processed_file <- basename(processed_path)
  result
}

load_latest_dataset <- function() {
  processed_path <- latest_processed_file()
  if (is.null(processed_path)) return(NULL)
  dataset <- tryCatch(readr::read_csv(processed_path, show_col_types = FALSE, name_repair = "minimal"), error = function(error) NULL)
  if (is.null(dataset) || nrow(dataset) == 0 || ncol(dataset) == 0) return(NULL)
  list(path = processed_path, data = dataset)
}

#* Return the combined dashboard data and conservative business insights.
#* @get /dashboard
#* @serializer json list(auto_unbox = TRUE)
function(req, res) {
  latest <- load_latest_dataset()
  if (is.null(latest)) return(error_response(res, "No sales dataset is available.", 404))
  analysis <- tryCatch(analyze_sales_data(latest$data), error = function(error) NULL)
  if (is.null(analysis)) return(error_response(res, "Unable to analyze the processed dataset.", 422))

  evaluation <- tryCatch(evaluate_dataset_models(latest$data), error = function(error) NULL)
  insights <- generate_business_insights(analysis, NULL, evaluation)
  list(success = TRUE, dataset = list(file = basename(latest$path), rows = nrow(latest$data), columns = ncol(latest$data)), analysis = analysis, evaluation = evaluation, forecast = NULL, insights = insights)
}