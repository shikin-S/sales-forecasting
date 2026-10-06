date_values_are_missing <- function(values) {
  normalized <- toupper(trimws(as.character(values)))
  is.na(values) | normalized %in% c("", "NA", "N/A", "NULL", "NAN")
}

normalize_parsed_date_text <- function(values) {
  values <- toupper(trimws(as.character(values)))
  values <- sub("[[:space:]]*(Z|UTC)$", "", values)
  gsub("(?<![0-9])0+([0-9])", "\\1", values, perl = TRUE)
}

parse_sales_dates <- function(values, field_name = "", companion_year = NULL) {
  original <- trimws(as.character(values))
  missing <- date_values_are_missing(original)
  present <- original[!missing]
  if (!length(present)) {
    return(list(values = as.Date(rep(NA_character_, length(original))), valid = 0L, missing = sum(missing), invalid = 0L, format = NULL, confidence = 0, error = NULL))
  }

  formats <- c(
    "%Y-%m-%d %H:%M:%OS%z", "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
    "%Y-%m-%dT%H:%M:%OS%z", "%Y-%m-%dT%H:%M:%OS",
    "%d-%m-%Y %H:%M:%OS", "%m-%d-%Y %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%m/%d/%Y %H:%M:%OS",
    "%Y-%m-%d", "%Y/%m/%d", "%Y%m%d", "%d-%m-%Y", "%m-%d-%Y", "%d/%m/%Y", "%m/%d/%Y",
    "%Y-%m", "%Y/%m", "%m-%Y", "%m/%Y", "%b %Y", "%B %Y", "%Y"
  )
  parsed_by_format <- lapply(formats, function(format_string) {
    parsed <- suppressWarnings(as.POSIXct(strptime(present, format = format_string, tz = "UTC")))
    valid <- !is.na(parsed)
    round_trip <- rep(NA_character_, length(parsed))
    round_trip[valid] <- format(parsed[valid], format_string, tz = "UTC")
    valid <- valid & normalize_parsed_date_text(round_trip) == normalize_parsed_date_text(present)
    parsed[!valid] <- NA
    if (all(is.na(parsed))) NULL else parsed
  })
  valid_by_format <- vapply(parsed_by_format, function(parsed) if (is.null(parsed)) 0L else as.integer(sum(!is.na(parsed))), integer(1))
  best_count <- max(valid_by_format)
  if (best_count == 0) {
    return(list(values = as.Date(rep(NA_character_, length(original))), valid = 0L, missing = sum(missing), invalid = length(present), format = NULL, confidence = 0, error = NULL))
  }

  best_indices <- which(valid_by_format == best_count)
  ambiguous_formats <- c("%d-%m-%Y", "%m-%d-%Y", "%d/%m/%Y", "%m/%d/%Y")
  ambiguous_indices <- best_indices[formats[best_indices] %in% ambiguous_formats]
  if (length(ambiguous_indices) > 1) {
    components <- lapply(present, function(value) {
      strsplit(gsub("[-/]", "/", sub("[ T].*$", "", value)), "/", fixed = TRUE)[[1]]
    })
    numeric_components <- Filter(function(parts) length(parts) == 3 && all(grepl("^\\d+$", parts)), components)
    day_first <- length(numeric_components) > 0 && any(vapply(numeric_components, function(parts) as.integer(parts[1]) > 12, logical(1)))
    month_first <- length(numeric_components) > 0 && any(vapply(numeric_components, function(parts) as.integer(parts[2]) > 12, logical(1)))
    if (day_first != month_first) {
      selected <- if (day_first) ambiguous_indices[formats[ambiguous_indices] %in% c("%d-%m-%Y", "%d/%m/%Y")] else ambiguous_indices[formats[ambiguous_indices] %in% c("%m-%d-%Y", "%m/%d/%Y")]
      best_indices <- selected
    } else if (length(unique(present)) > 1) {
      return(list(values = as.Date(rep(NA_character_, length(original))), valid = 0L, missing = sum(missing), invalid = length(present), format = NULL, confidence = 0, error = sprintf("Date field '%s' uses an ambiguous day/month format. Please provide ISO dates (YYYY-MM-DD) or an unambiguous date value.", field_name)))
    } else {
      return(list(values = as.Date(rep(NA_character_, length(original))), valid = 0L, missing = sum(missing), invalid = length(present), format = NULL, confidence = 0, error = sprintf("Date field '%s' uses an ambiguous day/month format. Please provide ISO dates (YYYY-MM-DD) or an unambiguous date value.", field_name)))
    }
  }

  selected_index <- best_indices[1]
  selected <- parsed_by_format[[selected_index]]
  if (best_count < length(present)) {
    return(list(values = as.Date(rep(NA_character_, length(original))), valid = as.integer(best_count), missing = sum(missing), invalid = length(present) - best_count, format = formats[selected_index], confidence = best_count / length(present), error = sprintf("Date field '%s' contains values that cannot be consistently parsed.", field_name)))
  }
  if (formats[selected_index] == "%Y") {
    years <- as.integer(format(selected, "%Y"))
    if (any(is.na(years) | years < 1000 | years > 9999)) {
      return(list(values = as.Date(rep(NA_character_, length(original))), valid = 0L, missing = sum(missing), invalid = length(present), format = NULL, confidence = 0, error = sprintf("Date field '%s' contains invalid year-only values.", field_name)))
    }
    selected <- as.POSIXct(paste0(format(selected, "%Y"), "-01-01"), format = "%Y-%m-%d", tz = "UTC")
  } else if (formats[selected_index] %in% c("%Y-%m", "%Y/%m", "%m-%Y", "%m/%Y", "%b %Y", "%B %Y")) {
    selected <- as.POSIXct(paste0(format(selected, "%Y-%m"), "-01"), format = "%Y-%m-%d", tz = "UTC")
  }

  output <- as.POSIXct(rep(NA_real_, length(original)), origin = "1970-01-01", tz = "UTC")
  output[!missing] <- selected
  is_date_only <- !any(grepl("\\d{1,2}:\\d{2}", present))
  if (is_date_only) output <- as.Date(output, tz = "UTC")
  list(values = output, valid = as.integer(best_count), missing = as.integer(sum(missing)), invalid = 0L, format = formats[selected_index], confidence = 1, error = NULL)
}
