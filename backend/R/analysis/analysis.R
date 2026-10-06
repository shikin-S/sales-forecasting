# Exploratory sales analysis module. All results are calculated from cleaned data.

analysis_column <- function(data, candidates) {
	matches <- intersect(candidates, names(data))
	if (length(matches) == 0) return(NULL)
	matches[1]
}

numeric_values <- function(data, column_name) {
	if (is.null(column_name) || !column_name %in% names(data)) return(numeric(0))
	values <- suppressWarnings(as.numeric(data[[column_name]]))
	values[is.finite(values)]
}

calculate_summary_statistics <- function(data) {
	sales_column <- analysis_column(data, c("total_sales", "sales", "revenue"))
	quantity_column <- analysis_column(data, c("quantity", "qty"))
	sales <- numeric_values(data, sales_column)
	quantity <- numeric_values(data, quantity_column)
	if (length(sales) == 0) return(list(available = FALSE, message = "Sales analysis cannot be performed because a valid sales field was not detected."))
	profit_column <- analysis_column(data, c("profit"))
	discount_column <- analysis_column(data, c("discount"))
	profit <- numeric_values(data, profit_column)
	discount <- numeric_values(data, discount_column)
	order_column <- analysis_column(data, c("order_id", "transaction_id"))
	unique_orders <- if (!is.null(order_column)) length(unique(data[[order_column]][!is.na(data[[order_column]])])) else "Not available"
	list(available = TRUE, count = length(sales), total_sales = sum(sales), average_sales = mean(sales), minimum_sales = min(sales), maximum_sales = max(sales), median_sales = median(sales), standard_deviation = if (length(sales) > 1) stats::sd(sales) else 0, total_quantity = if (length(quantity) > 0) sum(quantity) else "Not available", total_profit = if (length(profit) > 0) sum(profit) else "Not available", average_discount = if (length(discount) > 0) mean(discount) else "Not available", total_transactions = nrow(data), unique_orders = unique_orders, average_transaction_value = mean(sales), minimum_transaction_value = min(sales), maximum_transaction_value = max(sales))
}

calculate_time_analysis <- function(data) {
	date_column <- analysis_column(data, c("date", "order_date", "transaction_date", "sales_date"))
	sales_column <- analysis_column(data, c("total_sales", "sales", "revenue"))
	quantity_column <- analysis_column(data, c("quantity", "qty"))
	if (is.null(date_column) || is.null(sales_column)) return(list(available = FALSE, message = "Time-based analysis is unavailable because date or sales data is missing.", daily_sales = list(), monthly_sales = list(), yearly_sales = list()))
	dates <- suppressWarnings(as.Date(data[[date_column]]))
	sales <- suppressWarnings(as.numeric(data[[sales_column]]))
	quantity <- if (!is.null(quantity_column)) suppressWarnings(as.numeric(data[[quantity_column]])) else rep(NA_real_, nrow(data))
	valid <- !is.na(dates) & is.finite(sales)
	if (!any(valid)) return(list(available = FALSE, message = "Insufficient data for this analysis.", daily_sales = list(), monthly_sales = list(), yearly_sales = list()))
	quantity[is.na(quantity) | !is.finite(quantity)] <- 0
	work <- data.frame(date = dates[valid], sales = sales[valid], quantity = quantity[valid])
	daily <- aggregate(cbind(sales, quantity) ~ date, work, function(x) sum(x, na.rm = TRUE))
	daily <- daily[order(daily$date), , drop = FALSE]
	daily$quantity[is.nan(daily$quantity)] <- 0
	daily_output <- data.frame(date = format(daily$date, "%Y-%m-%d"), total_sales = daily$sales, total_quantity = daily$quantity)
	work$year <- as.integer(format(work$date, "%Y")); work$month <- as.integer(format(work$date, "%m")); work$month_name <- month.name[work$month]
	monthly <- aggregate(cbind(sales, quantity) ~ year + month + month_name, work, function(x) sum(x, na.rm = TRUE))
	monthly <- monthly[order(monthly$year, monthly$month), , drop = FALSE]; monthly$quantity[is.nan(monthly$quantity)] <- 0
	names(monthly) <- c("year", "month", "month_name", "total_sales", "total_quantity")
	yearly <- aggregate(cbind(sales, quantity) ~ year, work, function(x) sum(x, na.rm = TRUE)); yearly <- yearly[order(yearly$year), , drop = FALSE]; yearly$quantity[is.nan(yearly$quantity)] <- 0
	names(yearly) <- c("year", "total_sales", "total_quantity")
	highest <- daily[which.max(daily$sales), , drop = FALSE]; lowest <- daily[which.min(daily$sales), , drop = FALSE]
	best_month <- monthly[which.max(monthly$total_sales), , drop = FALSE]; worst_month <- monthly[which.min(monthly$total_sales), , drop = FALSE]
	list(available = TRUE, daily_sales = daily_output, monthly_sales = monthly, yearly_sales = yearly, highest_sales_date = format(highest$date, "%Y-%m-%d"), highest_sales_value = highest$sales, lowest_sales_date = format(lowest$date, "%Y-%m-%d"), lowest_sales_value = lowest$sales, best_month = paste(best_month$month_name, best_month$year), best_month_sales = best_month$total_sales, worst_month = paste(worst_month$month_name, worst_month$year), worst_month_sales = worst_month$total_sales)
}

calculate_product_analysis <- function(data) {
	product_column <- analysis_column(data, c("product", "product_name")); sales_column <- analysis_column(data, c("total_sales", "sales", "revenue")); quantity_column <- analysis_column(data, c("quantity", "qty"))
	if (is.null(product_column) || is.null(sales_column)) return(list(available = FALSE, message = "Product analysis is unavailable because product or sales data is missing.", all_products = list(), top_products = list(), bottom_products = list()))
	sales <- suppressWarnings(as.numeric(data[[sales_column]]))
	quantity <- if (!is.null(quantity_column)) suppressWarnings(as.numeric(data[[quantity_column]])) else rep(0, nrow(data))
	quantity[is.na(quantity) | !is.finite(quantity)] <- 0
	valid <- is.finite(sales)
	if (!any(valid)) return(list(available = FALSE, message = "Product analysis is unavailable because no usable sales values were found.", all_products = list(), top_products = list(), bottom_products = list()))
	work <- data.frame(product = as.character(data[[product_column]][valid]), sales = sales[valid], quantity = quantity[valid])
	work$product[is.na(work$product) | trimws(work$product) == ""] <- "Unknown"
	output <- aggregate(cbind(sales, quantity) ~ product, work, function(x) sum(x, na.rm = TRUE)); transactions <- aggregate(sales ~ product, work, function(x) sum(is.finite(x))); names(transactions)[2] <- "transactions"; averages <- aggregate(sales ~ product, work, mean, na.rm = TRUE); names(averages)[2] <- "average_sales"
	output <- merge(output, transactions, by = "product", all = TRUE); output <- merge(output, averages, by = "product", all = TRUE); output$quantity[is.nan(output$quantity)] <- 0; output$sales[is.nan(output$sales)] <- 0; output <- output[order(-output$sales), , drop = FALSE]
	names(output)[names(output) == "sales"] <- "total_sales"; names(output)[names(output) == "quantity"] <- "total_quantity"
	list(available = TRUE, all_products = output, top_products = head(output, 10), bottom_products = head(output[order(output$total_sales), , drop = FALSE], 10))
}

calculate_category_analysis <- function(data) {
	category_column <- analysis_column(data, c("category")); sales_column <- analysis_column(data, c("total_sales", "sales", "revenue")); quantity_column <- analysis_column(data, c("quantity", "qty"))
	if (is.null(category_column) || is.null(sales_column)) return(list(available = FALSE, message = "Category analysis unavailable because Category is not present.", category_sales = list()))
	sales <- suppressWarnings(as.numeric(data[[sales_column]])); quantity <- if (!is.null(quantity_column)) suppressWarnings(as.numeric(data[[quantity_column]])) else rep(0, nrow(data)); quantity[is.na(quantity) | !is.finite(quantity)] <- 0
	valid <- is.finite(sales)
	if (!any(valid)) return(list(available = FALSE, message = "Category analysis is unavailable because no usable sales values were found.", category_sales = list()))
	work <- data.frame(category = as.character(data[[category_column]][valid]), sales = sales[valid], quantity = quantity[valid]); work$category[is.na(work$category) | trimws(work$category) == ""] <- "Unknown"
	output <- aggregate(cbind(sales, quantity) ~ category, work, function(x) sum(x, na.rm = TRUE)); output$transactions <- as.integer(table(work$category)[output$category]); output$average_sales <- aggregate(sales ~ category, work, mean, na.rm = TRUE)$sales; output <- output[order(-output$sales), , drop = FALSE]
	names(output)[names(output) == "sales"] <- "total_sales"; names(output)[names(output) == "quantity"] <- "total_quantity"; list(available = TRUE, category_sales = output)
}

calculate_region_analysis <- function(data) {
	region_column <- analysis_column(data, c("region")); sales_column <- analysis_column(data, c("total_sales", "sales", "revenue"))
	if (is.null(region_column) || is.null(sales_column)) return(list(available = FALSE, message = "Regional analysis is not available because the dataset does not contain a region field.", region_sales = list()))
	sales <- suppressWarnings(as.numeric(data[[sales_column]])); quantity_column <- analysis_column(data, c("quantity", "qty")); quantity <- if (!is.null(quantity_column)) suppressWarnings(as.numeric(data[[quantity_column]])) else rep(0, nrow(data)); quantity[is.na(quantity) | !is.finite(quantity)] <- 0
	valid <- is.finite(sales)
	if (!any(valid)) return(list(available = FALSE, message = "Regional analysis is unavailable because no usable sales values were found.", region_sales = list()))
	work <- data.frame(region = as.character(data[[region_column]][valid]), sales = sales[valid], quantity = quantity[valid]); work$region[is.na(work$region) | trimws(work$region) == ""] <- "Unknown"
	output <- aggregate(cbind(sales, quantity) ~ region, work, sum); output$transactions <- as.integer(table(work$region)[output$region]); output$average_sales <- aggregate(sales ~ region, work, mean)$sales; names(output)[names(output) == "sales"] <- "total_sales"; names(output)[names(output) == "quantity"] <- "total_quantity"
	list(available = TRUE, region_sales = output[order(-output$total_sales), , drop = FALSE])
}

calculate_customer_analysis <- function(data) {
	customer_column <- analysis_column(data, c("customer", "customer_name")); sales_column <- analysis_column(data, c("total_sales", "sales", "revenue"))
	if (is.null(customer_column) || is.null(sales_column)) return(list(available = FALSE, message = "Customer analysis is unavailable because Customer is not present."))
	sales <- suppressWarnings(as.numeric(data[[sales_column]])); valid <- is.finite(sales)
	if (!any(valid)) return(list(available = FALSE, message = "Customer analysis is unavailable because no usable sales values were found."))
	work <- data.frame(customer = as.character(data[[customer_column]][valid]), sales = sales[valid]); work$customer[is.na(work$customer) | trimws(work$customer) == ""] <- "Unknown"; output <- aggregate(sales ~ customer, work, sum); output <- output[order(-output$sales), , drop = FALSE]; names(output)[names(output) == "sales"] <- "total_sales"
	list(available = TRUE, unique_customers = length(unique(work$customer)), sales_per_customer = output, top_customers = head(output, 10))
}

calculate_dimension_analysis <- function(data) {
	sales_column <- analysis_column(data, c("total_sales", "sales", "revenue"))
	if (is.null(sales_column)) return(list())
	dimensions <- intersect(c("product", "category", "region", "store", "customer"), names(data))
	results <- lapply(dimensions, function(dimension) {
		values <- trimws(as.character(data[[dimension]]))
		values[is.na(values) | values == ""] <- "Unknown"
		sales <- suppressWarnings(as.numeric(data[[sales_column]]))
		work <- data.frame(value = values, sales = sales)
		work <- work[is.finite(work$sales), , drop = FALSE]
		if (!nrow(work)) return(list(available = FALSE, values = list()))
		grouped <- aggregate(sales ~ value, work, sum)
		grouped <- grouped[order(-grouped$sales), , drop = FALSE]
		names(grouped) <- c("name", "total_sales")
		list(available = TRUE, values = grouped)
	})
	stats::setNames(results, dimensions)
}

calculate_trend_analysis <- function(time_analysis) {
	if (!isTRUE(time_analysis$available) || nrow(time_analysis$daily_sales) < 2) return(list(direction = "Insufficient data"))
	values <- as.numeric(time_analysis$daily_sales$total_sales); slope <- coef(stats::lm(values ~ seq_along(values)))[2]; direction <- if (is.na(slope) || abs(slope) < max(abs(mean(values)) * 0.001, 1e-8)) "Stable" else if (slope > 0) "Increasing" else "Decreasing"; list(direction = direction)
}

analyze_sales_data <- function(data) {
	summary <- calculate_summary_statistics(data); time_analysis <- calculate_time_analysis(data); product_analysis <- calculate_product_analysis(data); category_analysis <- calculate_category_analysis(data); regional_analysis <- calculate_region_analysis(data); customer_analysis <- calculate_customer_analysis(data)
	store_column <- analysis_column(data, c("store"))
	sales_column <- analysis_column(data, c("total_sales", "sales", "revenue"))
	store_analysis <- if (is.null(store_column) || is.null(sales_column)) list(available = FALSE, message = "Store analysis is unavailable because a store field was not detected.", store_sales = list()) else {
		store_values <- trimws(as.character(data[[store_column]])); store_values[is.na(store_values) | store_values == ""] <- "Unknown"
		store_work <- data.frame(store = store_values, total_sales = as.numeric(data[[sales_column]]))
		store_work <- store_work[is.finite(store_work$total_sales), , drop = FALSE]
		store_sales <- if (nrow(store_work)) aggregate(total_sales ~ store, store_work, sum) else data.frame(store = character(0), total_sales = numeric(0))
		if (nrow(store_sales)) store_sales <- store_sales[order(-store_sales$total_sales), , drop = FALSE]
		list(available = TRUE, store_sales = store_sales)
	}
	profit_column <- analysis_column(data, c("profit"))
	discount_column <- analysis_column(data, c("discount"))
	date_column <- analysis_column(data, c("date", "order_date", "transaction_date", "sales_date")); date_values <- if (!is.null(date_column)) suppressWarnings(as.Date(data[[date_column]])) else as.Date(character())
	list(success = TRUE, dataset = list(rows = nrow(data), columns = ncol(data), date_range = if (length(date_values) > 0 && any(!is.na(date_values))) paste(format(min(date_values, na.rm = TRUE), "%Y-%m-%d"), "to", format(max(date_values, na.rm = TRUE), "%Y-%m-%d")) else "Not available", unique_products = if (isTRUE(product_analysis$available)) length(unique(data[[analysis_column(data, c("product", "product_name"))]])) else "Not available", unique_categories = if (isTRUE(category_analysis$available)) length(unique(data[[analysis_column(data, c("category"))]])) else "Not available", unique_regions = if (isTRUE(regional_analysis$available)) length(unique(data[[analysis_column(data, c("region"))]])) else "Not available"), summary = summary, time_analysis = time_analysis, product_analysis = product_analysis, category_analysis = category_analysis, regional_analysis = regional_analysis, customer_analysis = customer_analysis, store_analysis = store_analysis, profit_analysis = if (is.null(profit_column) || length(numeric_values(data, profit_column)) == 0) list(available = FALSE, message = "Profit analysis is unavailable because no usable profit values were found.") else list(available = TRUE, total_profit = sum(numeric_values(data, profit_column)), average_profit = mean(numeric_values(data, profit_column))), discount_analysis = if (is.null(discount_column) || length(numeric_values(data, discount_column)) == 0) list(available = FALSE, message = "Discount analysis is unavailable because no usable discount values were found.") else list(available = TRUE, average_discount = mean(numeric_values(data, discount_column))), dimension_analysis = calculate_dimension_analysis(data), trend = calculate_trend_analysis(time_analysis))
}
