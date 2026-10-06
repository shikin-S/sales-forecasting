normalize_for_matching <- function(name) {
  gsub("[^a-z0-9]", "", tolower(trimws(as.character(name))))
}

sales_field_aliases <- list(
  date = c("orderdate", "transactiondate", "purchasedate", "invoicedate", "salesdate", "date", "shipdate", "timestamp", "datetime", "time", "month", "ordermonth", "transactiontime", "saledt", "transdate", "datekey"),
  total_sales = c("sales", "totalsales", "revenue", "totalrevenue", "amount", "totalamount", "salesamount", "orderamount", "invoiceamount", "netsales", "grosssales", "income", "turnover", "value", "revenueamount", "amt", "total", "netamount", "transactionamount", "subtotal"),
  quantity = c("quantity", "qty", "units", "unitssold", "quantitysold", "itemssold", "numberofitems", "volume", "count", "qtysold", "unitsordered", "items"),
  unit_price = c("unitprice", "sellingprice", "itemprice", "productprice", "price", "rate", "unitcost", "costprice", "priceperunit"),
  product = c("productname", "product", "itemname", "item", "sku", "stockkeepingunit", "productid", "itemid", "productcode", "itemcode", "itm"),
  category = c("category", "productcategory", "subcategory", "department", "division", "segment", "type", "producttype", "class"),
  customer = c("customername", "customer", "clientname", "client", "buyer", "accountname", "customerid", "clientid", "buyerid"),
  region = c("region", "territory", "market", "area", "location", "loc", "geography", "district", "state", "city", "country", "zone"),
  store = c("store", "storename", "shop", "branch", "outlet", "storeid", "shopid", "outletid"),
  transaction_id = c("transactionid", "orderid", "invoiceid", "receiptnumber", "receiptid", "billnumber", "invoicenumber", "transactionnumber", "orderkey", "txnid", "txid"),
  profit = c("profit", "netprofit", "grossprofit", "margin", "profitamount"),
  discount = c("discount", "discountrate", "discountpercent", "discountpercentage", "discountamount"),
  cost = c("cost", "totalcost", "costamount", "unitcost")
)

column_numeric_values <- function(column) {
  value <- trimws(as.character(column))
  value <- gsub("\\(([0-9,.]+)\\)", "-\\1", value)
  value <- gsub("[,$€£₹¥%[:space:]]", "", value)
  value <- gsub("[^0-9.\\-]", "", value)
  suppressWarnings(as.numeric(value))
}

score_column_values <- function(column, field, name) {
  values <- trimws(as.character(column))
  present <- !is.na(values) & nzchar(values)
  if (!any(present)) return(0)
  values <- values[present]
  numeric_values <- column_numeric_values(values)
  numeric_ratio <- mean(is.finite(numeric_values))
  unique_ratio <- length(unique(values)) / length(values)
  name_normalized <- normalize_for_matching(name)
  score <- 0
  if (field == "date") {
    parsed <- parse_sales_dates(values, name)
    return(if (is.null(parsed$error)) parsed$confidence else 0)
  }
  if (field %in% c("total_sales", "unit_price", "profit", "discount", "cost")) {
    if (grepl("date|time|month|year|week", name_normalized) || any(grepl("^[0-9]{1,4}[-/][0-9]{1,2}[-/][0-9]{1,4}", values))) return(0)
    if (grepl("id|key|code|number|postal|zip|row|index", name_normalized)) return(0)
    if (numeric_ratio < 0.7) return(0)
    if (field == "total_sales" && !any(grepl("[$€£₹¥]", values))) return(0)
    if (field %in% c("profit", "discount", "cost") && !score_column_name(name, field)) return(0)
    score <- 0.35 + 0.45 * numeric_ratio + 0.2 * (length(unique(numeric_values[is.finite(numeric_values)])) > 1)
    if (unique_ratio > 0.98 && grepl("id|code|number|key", name_normalized)) score <- score * 0.35
    return(min(score, 1))
  }
  if (field == "quantity") {
    if (grepl("date|time|month|year|week", name_normalized) || any(grepl("^[0-9]{1,4}[-/][0-9]{1,2}[-/][0-9]{1,4}", values))) return(0)
    if (grepl("id|key|code|number|postal|zip|row|index", name_normalized)) return(0)
    if (numeric_ratio < 0.7) return(0)
    finite_values <- numeric_values[is.finite(numeric_values)]
    integer_ratio <- if (length(finite_values)) mean(abs(finite_values - round(finite_values)) < 1e-8) else 0
    return(min(0.3 + 0.35 * numeric_ratio + 0.35 * integer_ratio, 1))
  }
  if (field %in% c("product", "category", "customer", "region", "store", "transaction_id")) {
    text_ratio <- mean(!is.finite(numeric_values))
    if (field == "transaction_id" && unique_ratio > 0.6) {
      if (!grepl("transaction|order|invoice|receipt|bill|txn|txid", name_normalized)) return(0)
      identifier_shape <- mean(grepl("[0-9]", values)) > 0.5 || mean(nchar(values) > 6) > 0.8
      if (identifier_shape) return(min(0.4 + unique_ratio * 0.5, 0.95))
      return(0)
    }
    if (field == "store" && score_column_name(name, field) == 0) return(0)
    if (field %in% c("category", "region", "store") && unique_ratio < 0.9) return(min(0.35 + (1 - unique_ratio) * 0.5, 0.9))
    if (field %in% c("product", "customer") && length(unique(values)) > 1) return(min(0.35 + (1 - unique_ratio) * 0.35 + text_ratio * 0.2, 0.85))
  }
  0
}

score_column_name <- function(name, field) {
  normalized <- normalize_for_matching(name)
  aliases <- sales_field_aliases[[field]]
  alias_position <- match(normalized, aliases)
  if (!is.na(alias_position)) return(max(0.85, 0.99 - (alias_position - 1) * 0.006))
  patterns <- switch(field,
    date = "date|time|month|year",
    total_sales = "sales|revenue|amount|turnover|income|value|amt",
    quantity = "quantity|qty|units|volume|items",
    unit_price = "price|rate|unitcost|costper",
    product = "product|item|sku|itm",
    category = "categor|department|division|segment|class|type",
    customer = "customer|client|buyer|account",
    region = "region|location|territory|market|country|state|city|zone|geograph|district|area",
    store = "store|shop|branch|outlet",
    transaction_id = "transaction|order|invoice|receipt|bill",
    profit = "profit|margin",
    discount = "discount",
    cost = "cost",
    ""
  )
  if (nzchar(patterns) && grepl(patterns, normalized)) return(0.82)
  0
}

detect_semantic_columns <- function(data) {
  detected <- list()
  confidence <- numeric(0)
  sources <- names(data)
  normalized_names <- make.unique(normalize_for_matching(sources))
  for (field in names(sales_field_aliases)) {
    candidates <- lapply(seq_along(sources), function(index) {
      name_score <- score_column_name(sources[index], field)
      value_score <- score_column_values(data[[index]], field, sources[index])
      score <- if (name_score > 0) 0.85 * name_score + 0.15 * value_score else 0.8 * value_score
      list(index = index, score = score)
    })
    scores <- vapply(candidates, `[[`, numeric(1), "score")
    if (!length(scores) || max(scores) < 0.55) next
    best_index <- which.max(scores)
    detected[[field]] <- sources[best_index]
    confidence[field] <- round(min(scores[best_index], 0.99), 2)
  }
  if (!is.null(detected$store) && identical(detected$store, detected$region)) {
    detected$region <- NULL
    confidence <- confidence[names(confidence) != "region"]
  }
  assigned_sources <- unlist(detected, use.names = FALSE)
  for (source in unique(assigned_sources[duplicated(assigned_sources)])) {
    fields <- names(detected)[vapply(detected, identical, logical(1), source)]
    winning_field <- fields[which.max(confidence[fields])]
    losing_fields <- setdiff(fields, winning_field)
    detected[losing_fields] <- NULL
    confidence <- confidence[!names(confidence) %in% losing_fields]
  }

  # A named sales column is preferred; otherwise choose a non-identifier numeric
  # measure with varied values when a usable date field anchors the dataset.
  if (is.null(detected$total_sales) && !is.null(detected$date)) {
    numeric_candidates <- which(vapply(data, function(column) mean(is.finite(column_numeric_values(column))) >= 0.9, logical(1)))
    excluded_fields <- c("date", "quantity", "unit_price", "profit", "discount", "cost")
    excluded_sources <- unlist(detected[intersect(excluded_fields, names(detected))], use.names = FALSE)
    numeric_candidates <- setdiff(numeric_candidates, match(excluded_sources, sources))
    numeric_candidates <- numeric_candidates[!grepl("id|key|code|number|date|time|year|month|day|week|age|height|weight|postal|zip|index|row", normalized_names[numeric_candidates])]
    if (length(numeric_candidates)) {
      variability <- vapply(numeric_candidates, function(index) {
        values <- column_numeric_values(data[[index]])
        values <- values[is.finite(values)]
        if (length(unique(values)) < 2) return(0)
        uniqueness <- length(unique(values)) / length(values)
        id_penalty <- if (uniqueness > 0.98 && grepl("id|key|code|number", normalized_names[index])) 0.1 else 1
        id_penalty * (0.4 + min(uniqueness, 0.8))
      }, numeric(1))
      if (max(variability) >= 0.5) {
        selected <- numeric_candidates[which.max(variability)]
        detected$total_sales <- sources[selected]
        confidence["total_sales"] <- 0.6
      }
    }
  }
  list(columns = detected, confidence = confidence)
}

profile_sales_dataset <- function(data, detection, duplicate_rows = 0L) {
  detected <- detection$columns
  has_sales <- !is.null(detected$total_sales) || (!is.null(detected$quantity) && !is.null(detected$unit_price))
  has_date <- !is.null(detected$date)
  dimensions <- intersect(c("product", "category", "region", "store", "customer", "quantity", "profit", "discount"), names(detected))
  dataset_type <- if (!has_sales) {
    "UNSUPPORTED_DATASET"
  } else if (has_date && !is.null(detected$store)) {
    "STORE_SALES_DATASET"
  } else if (has_date && !is.null(detected$product)) {
    "PRODUCT_SALES_DATASET"
  } else if (has_date) {
    "TIME_SERIES_SALES_DATASET"
  } else if (!is.null(detected$customer) && !is.null(detected$product)) {
    "E_COMMERCE_DATASET"
  } else if (!is.null(detected$transaction_id)) {
    "TRANSACTION_DATASET"
  } else {
    "SALES_DATASET"
  }
  list(
    dataset_type = dataset_type,
    rows = nrow(data),
    columns = ncol(data),
    column_names = names(data),
    data_types = vapply(data, function(column) class(column)[1], character(1)),
    sample_values = lapply(data, function(column) head(as.character(column), 3)),
    missing_by_column = missing_by_column(data),
    unique_by_column = vapply(data, function(column) length(unique(column[!is.na(column)])), integer(1)),
    numeric_columns = names(data)[vapply(data, is.numeric, logical(1))],
    categorical_columns = names(data)[vapply(data, function(column) is.character(column) || is.factor(column), logical(1))],
    date_candidates = if (!is.null(detected$date)) detected$date else character(0),
    id_candidates = unlist(detected[intersect(c("transaction_id", "customer"), names(detected))], use.names = FALSE),
    duplicate_rows = as.integer(duplicate_rows),
    detected_fields = detected,
    field_confidence = as.list(detection$confidence),
    available_dimensions = dimensions,
    missing_optional_fields = setdiff(c("product", "category", "quantity", "unit_price", "customer", "region", "store", "profit", "discount", "transaction_id"), names(detected)),
    forecasting_available = has_date && !is.null(detected$total_sales),
    sales_analysis_available = has_sales,
    date_range = "Not available"
  )
}
