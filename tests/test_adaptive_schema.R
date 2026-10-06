source("backend/R/preprocessing/date_parser.R")
source("backend/R/preprocessing/column_detector.R")
source("backend/R/preprocessing/schema_mapper.R")
source("backend/R/preprocessing/preprocess.R")
source("backend/R/analysis/analysis.R")
source("backend/R/forecasting/forecast_helpers.R")
source("backend/R/forecasting/moving_average.R")
source("backend/R/forecasting/linear_regression.R")
source("backend/R/forecasting/exponential_smoothing.R")
source("backend/R/forecasting/holt_winters.R")
source("backend/R/forecasting/arima.R")
source("backend/R/forecasting/forecast_engine.R")
source("backend/R/evaluation/metrics.R")
source("backend/R/evaluation/evaluation_helpers.R")
source("backend/R/evaluation/evaluate_model.R")

make_dataset <- function(fields) {
  as.data.frame(fields, check.names = FALSE, stringsAsFactors = FALSE)
}

datasets <- list(
  list(
    name = "Date, Product, Quantity, Unit_Price, Total_Sales",
    data = make_dataset(list(Date = c("2025-01-01", "2025-01-02"), Product = c("Pen", "Book"), Quantity = c(2, 3), Unit_Price = c(1.5, 2), Total_Sales = c(3, 6))),
    fields = c("date", "product", "quantity", "unit_price", "total_sales")
  ),
  list(
    name = "Order Date, Product Name, Qty, Price, Revenue",
    data = make_dataset(list(`Order Date` = c("2025-01-01", "2025-01-02"), `Product Name` = c("Pen", "Book"), Qty = c(2, 3), Price = c(1.5, 2), Revenue = c(3, 6))),
    fields = c("date", "product", "quantity", "unit_price", "total_sales")
  ),
  list(
    name = "Transaction Date, Item, Units Sold, Selling Price, Sales Amount",
    data = make_dataset(list(`Transaction Date` = c("2025-01-01", "2025-01-02"), Item = c("Pen", "Book"), `Units Sold` = c(2, 3), `Selling Price` = c(1.5, 2), `Sales Amount` = c(3, 6))),
    fields = c("date", "product", "quantity", "unit_price", "total_sales")
  ),
  list(
    name = "Date, Category, Sales",
    data = make_dataset(list(Date = c("2025-01-01", "2025-01-02"), Category = c("Food", "Drink"), Sales = c(10, 12))),
    fields = c("date", "category", "total_sales")
  ),
  list(
    name = "Order_Date, Revenue",
    data = make_dataset(list(Order_Date = c("2025-01-01", "2025-01-02"), Revenue = c(10, 12))),
    fields = c("date", "total_sales")
  ),
  list(
    name = "Date, Store, Product, Quantity, Sales, Profit",
    data = make_dataset(list(Date = c("2025-01-01", "2025-01-02"), Store = c("S1", "S2"), Product = c("Pen", "Book"), Quantity = c(2, 3), Sales = c(10, 12), Profit = c(2, 3))),
    fields = c("date", "store", "product", "quantity", "total_sales", "profit")
  ),
  list(
    name = "Transaction_ID, Date, Product, Region, Amount",
    data = make_dataset(list(Transaction_ID = c("T1", "T2"), Date = c("2025-01-01", "2025-01-02"), Product = c("Pen", "Book"), Region = c("North", "South"), Amount = c(10, 12))),
    fields = c("date", "transaction_id", "product", "region", "total_sales")
  ),
  list(
    name = "sale_dt, item_name, qty_sold, amt",
    data = make_dataset(list(sale_dt = c("2025-01-01", "2025-01-02"), item_name = c("Pen", "Book"), qty_sold = c(2, 3), amt = c("$10.50", "₹12.00"))),
    fields = c("date", "product", "quantity", "total_sales")
  ),
  list(
    name = "Date, Store, Item, Sales",
    data = make_dataset(list(Date = c("2025-01-01", "2025-01-02"), Store = c("S1", "S2"), Item = c("Pen", "Book"), Sales = c(10, 12))),
    fields = c("date", "store", "product", "total_sales")
  ),
  list(
    name = "Month, Year, Sales",
    data = make_dataset(list(Month = c("Jan", "Feb", "Mar"), Year = c(2025, 2025, 2025), Sales = c(10, 12, 14))),
    fields = c("date", "total_sales")
  )
)

for (case in datasets) {
  cat(sprintf("Checking schema: %s\n", case$name))
  result <- preprocess_data(case$data)
  missing_fields <- setdiff(case$fields, names(result$cleaned_data))
  if (length(missing_fields)) stop(sprintf("%s did not map: %s", case$name, paste(missing_fields, collapse = ", ")))
  if (!isTRUE(result$profile$sales_analysis_available)) stop(sprintf("%s was not recognized as sales data.", case$name))
  if (!isTRUE(analyze_sales_data(result$cleaned_data)$summary$available)) stop(sprintf("%s did not reach sales analysis.", case$name))
}

currency_values <- parse_sales_numeric(c("1,250", "$1,250.50", "₹1,250", "€1,250", "£1,250"))
stopifnot(identical(currency_values, c(1250, 1250.5, 1250, 1250, 1250)))

monthly <- preprocess_data(datasets[[10]]$data)$cleaned_data
prepared_monthly <- prepare_forecast_data(monthly)
stopifnot(is.data.frame(prepared_monthly), nrow(prepared_monthly) == 3L, identical(detect_frequency(prepared_monthly), "monthly"))

analysis_only <- preprocess_data(make_dataset(list(Product = c("Pen", "Book"), Sales = c(10, 12))))
stopifnot(!analysis_only$profile$forecasting_available, is.list(prepare_forecast_data(analysis_only$cleaned_data)))

ambiguous <- tryCatch(
  preprocess_data(make_dataset(list(Date = c("01/02/2025", "03/04/2025"), Sales = c(10, 12)))),
  error = function(error) error
)
stopifnot(inherits(ambiguous, "error"), grepl("ambiguous day/month", conditionMessage(ambiguous), ignore.case = TRUE))

derived <- preprocess_data(make_dataset(list(Date = c("2025-01-01", "2025-01-02"), Quantity = c(2, NA), Unit_Price = c(5, 5), Total_Sales = c(NA, 15))))
stopifnot(derived$cleaned_data$total_sales[1] == 10, derived$cleaned_data$quantity[2] == 3, derived$processing_summary$sales_values_calculated == 1L)

year_only <- parse_sales_dates(c("2023", "2024"), "Date")
stopifnot(year_only$valid == 2L, format(year_only$values[1], "%Y-%m-%d") == "2023-01-01")

hourly <- preprocess_data(make_dataset(list(Timestamp = c("2025-01-01 10:00:00", "2025-01-01 11:00:00", "2025-01-01 12:00:00"), Revenue = c(10, 12, 14))))
stopifnot(identical(detect_frequency(prepare_forecast_data(hourly$cleaned_data)), "hourly"))

unsupported <- tryCatch(
  preprocess_data(make_dataset(list(Age = c(25, 30), Height = c(170, 180)))),
  error = function(error) error
)
stopifnot(inherits(unsupported, "error"), grepl("Sales/revenue field could not be identified", conditionMessage(unsupported)))

forecast_data <- make_dataset(list(Date = format(as.Date("2025-01-01") + 0:19), Sales = seq(100, 290, by = 10)))
forecast_processed <- preprocess_data(forecast_data)$cleaned_data
forecast_result <- generate_forecast(forecast_processed, "moving_average", 7)
stopifnot(identical(forecast_result$model_status, "success"), nrow(forecast_result$forecast) == 7L)
evaluation_result <- evaluate_model(forecast_processed, "moving_average", 4)
stopifnot(identical(evaluation_result$status, "success"), evaluation_result$training_size == 16L, evaluation_result$testing_size == 4L)

if (file.exists("data/raw/kaggle_superstore_sales.csv")) {
  kaggle_data <- readr::read_csv("data/raw/kaggle_superstore_sales.csv", show_col_types = FALSE, name_repair = "minimal")
  kaggle_result <- preprocess_data(kaggle_data)
  stopifnot(
    kaggle_result$profile$rows == 9800L,
    kaggle_result$profile$date_range == "2015-01-03 to 2018-12-30",
    kaggle_result$profile$forecasting_available,
    identical(kaggle_result$detected_columns$date, "Order Date"),
    identical(kaggle_result$detected_columns$total_sales, "Sales"),
    identical(kaggle_result$detected_columns$product, "Product Name"),
    identical(kaggle_result$detected_columns$category, "Category"),
    identical(kaggle_result$detected_columns$region, "Region"),
    is.null(kaggle_result$detected_columns$quantity)
  )
  kaggle_forecast <- generate_forecast(kaggle_result$cleaned_data, "moving_average", 7)
  stopifnot(identical(kaggle_forecast$model_status, "success"), identical(kaggle_forecast$frequency, "daily"))
  cat("Passed bundled Kaggle Superstore upload, mapping, date parsing, and forecast checks.\n")
}

cat(sprintf("Passed adaptive schema checks for %d Kaggle-style dataset shapes.\n", length(datasets)))
