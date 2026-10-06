forecast_error <- function(message, status = "failed") {
  list(model_status = status, message = message)
}

prepare_forecast_data <- function(data) {
  date_column <- intersect(c("date", "order_date", "transaction_date", "sales_date"), names(data))[1]
  sales_column <- intersect(c("total_sales", "sales", "revenue"), names(data))[1]
  if (is.na(date_column) || is.na(sales_column)) return(forecast_error("Forecasting requires a valid date and sales field.", "insufficient_data"))
  raw_dates <- data[[date_column]]
  dates <- if (inherits(raw_dates, "POSIXt")) {
    as.POSIXct(raw_dates, tz = "UTC")
  } else if (inherits(raw_dates, "Date")) {
    raw_dates
  } else {
    values <- trimws(as.character(raw_dates))
    has_time <- any(grepl("\\d{1,2}:\\d{2}", values), na.rm = TRUE)
    if (has_time) suppressWarnings(as.POSIXct(values, tz = "UTC")) else suppressWarnings(as.Date(values))
  }
  sales <- suppressWarnings(as.numeric(data[[sales_column]]))
  valid <- !is.na(dates) & is.finite(sales)
  if (!any(valid)) return(forecast_error("Forecasting requires a valid date and sales field.", "insufficient_data"))
  prepared <- aggregate(sales[valid], by = list(date = dates[valid]), FUN = sum, na.rm = TRUE)
  names(prepared)[2] <- "sales"
  prepared <- prepared[order(prepared$date), , drop = FALSE]
  row.names(prepared) <- NULL
  prepared
}

detect_frequency <- function(data) {
  if (is.list(data) && !is.null(data$model_status)) return("insufficient")
  if (nrow(data) < 2) return("insufficient")
  date_gaps <- as.numeric(diff(data$date))
  if (!length(date_gaps)) return("insufficient")
  gap <- median(date_gaps, na.rm = TRUE)
  if (inherits(data$date, "POSIXt")) {
    if (gap <= 5400) return("hourly")
    if (gap <= 3 * 86400) return("daily")
    if (gap <= 10 * 86400) return("weekly")
    if (gap >= 27 * 86400 && gap <= 35 * 86400) return("monthly")
    if (gap >= 300 * 86400) return("yearly")
  } else {
    if (gap <= 2) return("daily")
    if (gap >= 5 && gap <= 10) return("weekly")
    if ((all(as.integer(format(data$date, "%d")) == 1L) || gap >= 27) && gap < 300) return("monthly")
    if (gap >= 300) return("yearly")
  }
  "daily"
}

period_to_horizon <- function(period, frequency) {
  period <- suppressWarnings(as.numeric(period))
  if (!is.finite(period) || period <= 0) return(NULL)
  if (frequency == "monthly") {
    if (period %in% c(6, 12)) return(as.integer(period))
    if (period %in% c(7, 30, 90)) return(as.integer(round(period / 30)))
  }
  if (frequency == "weekly" && period %in% c(7, 30, 90)) return(as.integer(round(period / 7)))
  if (frequency == "yearly" && period == 12) return(1L)
  if (frequency == "hourly" && period %in% c(7, 30, 90)) return(as.integer(period * 24))
  if (frequency == "daily" && period %in% c(7, 30, 90)) return(as.integer(period))
  if (frequency == "daily" && period == 6) return(182L)
  if (frequency == "daily" && period == 12) return(365L)
  NULL
}

future_dates <- function(last_date, horizon, frequency) {
  if (frequency == "monthly") {
    seq(lubridate::`%m+%`(as.Date(format(last_date, "%Y-%m-01")), lubridate::months(1)), by = "month", length.out = horizon)
  } else if (frequency == "weekly") {
    first_date <- if (inherits(last_date, "POSIXt")) last_date + 7 * 86400 else last_date + 7
    seq(first_date, by = "7 days", length.out = horizon)
  } else if (frequency == "yearly") {
    seq(lubridate::`%m+%`(as.Date(format(last_date, "%Y-01-01")), lubridate::years(1)), by = "year", length.out = horizon)
  } else if (frequency == "hourly") {
    seq(last_date + 3600, by = "hour", length.out = horizon)
  } else {
    first_date <- if (inherits(last_date, "POSIXt")) last_date + 86400 else last_date + 1
    seq(first_date, by = "day", length.out = horizon)
  }
}

validate_forecast_output <- function(values, dates, horizon) {
  if (length(values) != horizon || length(dates) != horizon || any(!is.finite(values)) || any(is.na(dates))) return(FALSE)
  TRUE
}

standard_forecast_result <- function(model, data, horizon, values, lower = NULL, upper = NULL, model_status = "success", message = NULL, negative_adjustments = 0L) {
  frequency <- detect_frequency(data)
  forecast_dates <- future_dates(max(data$date), horizon, frequency)
  if (!validate_forecast_output(values, forecast_dates, horizon)) return(forecast_error("Invalid forecast output.", "failed"))
  negative_count <- sum(values < 0)
  values <- pmax(0, as.numeric(values))
  date_format <- if (frequency == "hourly") "%Y-%m-%d %H:%M:%S" else "%Y-%m-%d"
  rows <- data.frame(date = format(forecast_dates, date_format), predicted_sales = values)
  rows$lower <- if (!is.null(lower)) pmax(0, as.numeric(lower)) else NA_real_
  rows$upper <- if (!is.null(upper)) pmax(0, as.numeric(upper)) else NA_real_
  list(model = model, frequency = frequency, horizon = horizon, model_status = model_status, message = message, negative_adjustments = negative_adjustments + negative_count, historical = data.frame(date = format(data$date, date_format), sales = data$sales), forecast = rows)
}