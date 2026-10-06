is_empty_value <- function(value) {
  normalized <- toupper(trimws(as.character(value)))
  is.na(value) | normalized %in% c("", "NA", "N/A", "NULL", "NAN")
}

count_missing_values <- function(data) {
  if (ncol(data) == 0) return(0L)
  as.integer(sum(vapply(data, function(column) sum(is_empty_value(column)), integer(1))))
}

missing_by_column <- function(data) {
  setNames(vapply(data, function(column) as.integer(sum(is_empty_value(column))), integer(1)), names(data))
}

preprocess_data <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) {
    stop("The uploaded dataset contains no records.", call. = FALSE)
  }
  original <- data
  missing_before <- count_missing_values(original)
  data <- data[, !duplicated(names(data)), drop = FALSE]
  source_names <- names(data)
  names(data) <- standardize_column_names(names(data))
  empty_rows <- if (nrow(data)) apply(data, 1, function(row) all(is_empty_value(row))) else logical(0)
  empty_rows_removed <- sum(empty_rows)
  data <- data[!empty_rows, , drop = FALSE]
  if (nrow(data) == 0) stop("The uploaded dataset contains no non-empty records.", call. = FALSE)

  normalized_names <- normalize_for_matching(names(data))
  month_index <- which(normalized_names %in% c("month", "ordermonth", "salesmonth", "transactionmonth"))[1]
  year_index <- which(normalized_names %in% c("year", "orderyear", "salesyear", "transactionyear"))[1]
  has_month_year <- length(month_index) && length(year_index) && !is.na(month_index) && !is.na(year_index)
  if (has_month_year && !any(normalized_names %in% c("date", "orderdate", "transactiondate", "purchasedate", "invoicedate", "salesdate", "timestamp", "datetime"))) {
    month_values <- trimws(as.character(data[[month_index]]))
    year_values <- trimws(as.character(data[[year_index]]))
    month_number <- suppressWarnings(as.integer(month_values))
    month_number[is.na(month_number)] <- match(tolower(month_values[is.na(month_number)]), tolower(month.name))
    month_number[is.na(month_number)] <- match(tolower(month_values[is.na(month_number)]), tolower(month.abb))
    valid_month_year <- !is.na(month_number) & month_number >= 1 & month_number <= 12 & grepl("^\\d{4}$", year_values)
    if (any(valid_month_year)) {
      data$.month_year_date <- NA_character_
      data$.month_year_date[valid_month_year] <- sprintf("%04d-%02d-01", as.integer(year_values[valid_month_year]), month_number[valid_month_year])
    }
  }
  detection <- detect_semantic_columns(data)
  if (".month_year_date" %in% names(data)) {
    detection$columns$date <- ".month_year_date"
    detection$confidence["date"] <- 0.98
  }
  if (is.null(detection$columns$total_sales) &&
      !(all(c("quantity", "unit_price") %in% names(detection$columns)))) {
    stop("Sales/revenue field could not be identified. Include a sales amount/revenue column or both quantity and unit price.", call. = FALSE)
  }
  cleaned <- map_sales_schema(data, detection)
  if (!"total_sales" %in% names(cleaned) || !any(is.finite(cleaned$total_sales))) {
    stop("A sales/revenue field was detected, but it contains no usable numeric values.", call. = FALSE)
  }

  exact_duplicates <- duplicated(data) | duplicated(data, fromLast = TRUE)
  exact_duplicate_rows <- sum(duplicated(data))
  transaction_column <- detection$columns$transaction_id
  duplicate_transaction_ids <- 0L
  if (!is.null(transaction_column) && transaction_column %in% names(data)) {
    identifiers <- trimws(as.character(data[[transaction_column]]))
    duplicate_transaction_ids <- sum(duplicated(identifiers[!is_empty_value(identifiers)]))
  }
  parse_result <- attr(cleaned, "date_parse")
  if (is.null(parse_result)) {
    parse_result <- list(valid = 0L, missing = 0L, invalid = 0L, format = NULL, confidence = 0, error = NULL)
  }
  derived_fields <- attr(cleaned, "derived_fields")
  derived_counts <- attr(cleaned, "derived_counts")
  if (is.null(derived_counts)) derived_counts <- integer(0)
  date_column <- if ("date" %in% names(cleaned)) "date" else NULL
  dates <- if (!is.null(date_column)) cleaned$date[!is.na(cleaned$date)] else NULL
  profile <- profile_sales_dataset(data, detection, exact_duplicate_rows)
  profile$rows <- nrow(original)
  profile$columns <- ncol(original)
  original_profile_data <- original[, !duplicated(names(original)), drop = FALSE]
  if (length(dates)) profile$date_range <- paste(format(min(dates), "%Y-%m-%d"), "to", format(max(dates), "%Y-%m-%d"))
  profile$column_names <- source_names
  profile$data_types <- vapply(original_profile_data, function(column) class(column)[1], character(1))
  profile$sample_values <- lapply(original_profile_data, function(column) head(as.character(column), 3))
  profile$missing_by_column <- missing_by_column(original_profile_data)
  profile$unique_by_column <- vapply(original_profile_data, function(column) as.integer(length(unique(column[!is.na(column)]))), integer(1))
  profile$numeric_columns <- names(original_profile_data)[vapply(original_profile_data, is.numeric, logical(1))]
  profile$categorical_columns <- names(original_profile_data)[vapply(original_profile_data, function(column) is.character(column) || is.factor(column), logical(1))]
  display_fields <- lapply(detection$columns, function(source) {
    source_index <- match(source, names(data))
    if (!is.na(source_index) && source == ".month_year_date") return("Month + Year")
    if (!is.na(source_index)) source_names[source_index] else source
  })
  names(display_fields) <- names(detection$columns)
  profile$detected_fields <- display_fields
  profile$mapped_fields <- display_fields
  if (!is.null(detection$columns$date)) profile$date_candidates <- display_fields$date
  profile$id_candidates <- unlist(display_fields[intersect(c("transaction_id", "customer"), names(display_fields))], use.names = FALSE)
  profile$field_confidence <- as.list(detection$confidence)
  profile$date_format <- parse_result$format
  profile$date_confidence <- parse_result$confidence
  profile$forecasting_available <- !is.null(date_column) && length(dates) > 0 && "total_sales" %in% names(cleaned)
  profile$duplicate_rows_retained <- as.integer(exact_duplicate_rows)
  profile$duplicate_transaction_ids <- as.integer(duplicate_transaction_ids)

  invalid_numeric <- 0L
  invalid_counts <- c(total_sales = 0L, quantity = 0L, unit_price = 0L, profit = 0L, discount = 0L, cost = 0L)
  numeric_field_names <- intersect(c("total_sales", "quantity", "unit_price", "profit", "discount", "cost"), names(cleaned))
  for (field in numeric_field_names) {
    source <- detection$columns[[field]]
    if (is.null(source) || !source %in% names(data)) next
    raw_values <- data[[source]]
    meaningful <- !is_empty_value(raw_values)
    invalid_count <- sum(meaningful & !is.finite(parse_sales_numeric(raw_values)), na.rm = TRUE)
    invalid_counts[field] <- invalid_count
    invalid_numeric <- invalid_numeric + invalid_count
  }
  sales_values_calculated <- if ("total_sales" %in% names(derived_counts)) derived_counts[["total_sales"]] else 0L
  missing_after <- count_missing_values(cleaned)
  warnings <- character(0)
  if (is.null(date_column)) {
    warnings <- c(warnings, "No usable date field was detected. Sales analysis is available, but time-series analysis and forecasting are unavailable.")
  } else if (!any(!is.na(cleaned$date))) {
    warnings <- c(warnings, "The detected date field has no usable values. Sales analysis is available, but time-series analysis and forecasting are unavailable.")
  }
  if (!length(detection$columns$product)) warnings <- c(warnings, "Product analysis is unavailable because no product field was detected.")
  if (!length(detection$columns$category)) warnings <- c(warnings, "Category analysis is unavailable because no category field was detected.")
  if (!length(detection$columns$region)) warnings <- c(warnings, "Regional analysis is unavailable because no region field was detected.")

  summary <- list(
    original_rows = nrow(original),
    original_columns = ncol(original),
    original_column_names = names(original),
    original_data_types = vapply(original, function(column) class(column)[1], character(1)),
    empty_rows_removed = as.integer(empty_rows_removed),
    duplicates_found = as.integer(exact_duplicate_rows),
    duplicates_removed = 0L,
    duplicate_rows_retained = as.integer(exact_duplicate_rows),
    duplicate_transaction_ids = as.integer(duplicate_transaction_ids),
    missing_values_before = missing_before,
    missing_values_after = missing_after,
    missing_by_column = missing_by_column(original),
    values_imputed = 0L,
    valid_dates = as.integer(parse_result$valid),
    invalid_dates = as.integer(parse_result$invalid),
    missing_dates = as.integer(parse_result$missing),
    invalid_quantity_count = invalid_counts[["quantity"]],
    invalid_price_count = invalid_counts[["unit_price"]],
    invalid_sales_count = invalid_counts[["total_sales"]],
    invalid_numeric_values = as.integer(invalid_numeric),
    sales_values_calculated = as.integer(sales_values_calculated),
    inconsistent_sales_records = 0L,
    derived_fields = derived_fields,
    rows_retained = nrow(cleaned),
    rows_removed = as.integer(empty_rows_removed),
    final_rows = nrow(cleaned),
    final_columns = ncol(cleaned)
  )
  list(
    cleaned_data = cleaned,
    processing_summary = summary,
    detected_columns = display_fields,
    field_confidence = detection$confidence,
    mapped_fields = display_fields,
    date_column = date_column,
    profile = profile,
    warnings = warnings
  )
}
