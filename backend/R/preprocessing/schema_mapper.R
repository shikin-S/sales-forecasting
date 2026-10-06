standardize_column_names <- function(column_names) {
  standardized <- tolower(trimws(column_names))
  standardized <- gsub("[^a-z0-9]+", "_", standardized)
  standardized <- gsub("^_|_$", "", standardized)
  standardized <- gsub("_+", "_", standardized)
  make.unique(standardized, sep = "_")
}

parse_sales_numeric <- function(values) {
  original <- trimws(as.character(values))
  missing <- date_values_are_missing(original)
  parenthesized_negative <- grepl("^\\s*\\(.*\\)\\s*$", original)
  cleaned <- gsub("[,$€£₹¥%[:space:]]", "", original)
  cleaned <- gsub("[()]", "", cleaned)
  cleaned <- gsub("[^0-9.\\-]", "", cleaned)
  converted <- suppressWarnings(as.numeric(cleaned))
  converted[parenthesized_negative & !is.na(converted)] <- -abs(converted[parenthesized_negative & !is.na(converted)])
  converted[missing] <- NA_real_
  converted
}

map_sales_schema <- function(data, detection) {
  source_fields <- detection$columns
  mapped <- data
  canonical_fields <- c(
    date = "date", product = "product", category = "category", quantity = "quantity",
    unit_price = "unit_price", total_sales = "total_sales", profit = "profit",
    discount = "discount", customer = "customer", region = "region", store = "store",
    transaction_id = "transaction_id", cost = "cost"
  )
  for (field in names(canonical_fields)) {
    source <- source_fields[[field]]
    if (is.null(source) || !source %in% names(data)) next
    target <- canonical_fields[[field]]
    if (field == "date") {
      parsed <- parse_sales_dates(data[[source]], source)
      if (!is.null(parsed$error)) stop(parsed$error, call. = FALSE)
      if (parsed$valid == 0L && parsed$invalid > 0L) {
        stop(sprintf("Date field '%s' could not be parsed using the supported date and month/year formats.", source), call. = FALSE)
      }
      mapped[[target]] <- parsed$values
      attr(mapped, "date_parse") <- parsed
    } else if (field %in% c("total_sales", "quantity", "unit_price", "profit", "discount", "cost")) {
      mapped[[target]] <- parse_sales_numeric(data[[source]])
    } else {
      mapped[[target]] <- trimws(as.character(data[[source]]))
      mapped[[target]][mapped[[target]] == ""] <- NA_character_
    }
  }

  derived_fields <- character(0)
  derived_counts <- integer(0)
  if (!"total_sales" %in% names(mapped) && all(c("quantity", "unit_price") %in% names(mapped))) {
    mapped$total_sales <- NA_real_
  }
  if (all(c("total_sales", "quantity", "unit_price") %in% names(mapped))) {
    valid <- !is.finite(mapped$total_sales) & is.finite(mapped$quantity) & is.finite(mapped$unit_price)
    mapped$total_sales[valid] <- mapped$quantity[valid] * mapped$unit_price[valid]
    if (any(valid)) {
      derived_fields <- c(derived_fields, "total_sales")
      derived_counts["total_sales"] <- sum(valid)
    }
  }
  if (!"unit_price" %in% names(mapped) && all(c("total_sales", "quantity") %in% names(mapped))) {
    mapped$unit_price <- NA_real_
  }
  if (all(c("total_sales", "quantity", "unit_price") %in% names(mapped))) {
    valid <- !is.finite(mapped$unit_price) & is.finite(mapped$total_sales) & is.finite(mapped$quantity) & mapped$quantity != 0
    mapped$unit_price[valid] <- mapped$total_sales[valid] / mapped$quantity[valid]
    if (any(valid)) {
      derived_fields <- c(derived_fields, "unit_price")
      derived_counts["unit_price"] <- sum(valid)
    }
  }
  if (!"quantity" %in% names(mapped) && all(c("total_sales", "unit_price") %in% names(mapped))) {
    mapped$quantity <- NA_real_
  }
  if (all(c("total_sales", "quantity", "unit_price") %in% names(mapped))) {
    valid <- !is.finite(mapped$quantity) & is.finite(mapped$total_sales) & is.finite(mapped$unit_price) & mapped$unit_price != 0
    mapped$quantity[valid] <- mapped$total_sales[valid] / mapped$unit_price[valid]
    if (any(valid)) {
      derived_fields <- c(derived_fields, "quantity")
      derived_counts["quantity"] <- sum(valid)
    }
  }
  if (!"profit" %in% names(mapped) && all(c("total_sales", "cost") %in% names(mapped))) {
    mapped$profit <- NA_real_
  }
  if (all(c("total_sales", "cost", "profit") %in% names(mapped))) {
    valid <- !is.finite(mapped$profit) & is.finite(mapped$total_sales) & is.finite(mapped$cost)
    mapped$profit[valid] <- mapped$total_sales[valid] - mapped$cost[valid]
    if (any(valid)) {
      derived_fields <- c(derived_fields, "profit")
      derived_counts["profit"] <- sum(valid)
    }
  }
  attr(mapped, "derived_fields") <- unique(derived_fields)
  attr(mapped, "derived_counts") <- derived_counts
  mapped
}
