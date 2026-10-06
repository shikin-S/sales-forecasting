# Sales Forecasting and Analysis Using R

## Project Objective

This project provides a web-based workspace for profiling flexible sales CSV schemas, preprocessing and analyzing sales with R, forecasting supported time series, evaluating forecasting models, and presenting results through a simple HTML, CSS, and JavaScript interface.

## Technology Stack

- **Frontend:** HTML5, CSS3, and vanilla JavaScript
- **Backend and data science:** R and RStudio-compatible scripts
- **Future API layer:** Plumber
- **Future visualization:** Chart.js

No frontend or backend framework is required for the initial setup.

## Planned Modules

1. Dataset Management
2. Data Upload and Validation
3. Data Preprocessing
4. Sales Analysis and EDA
5. Sales Forecasting
6. Model Performance Evaluation
7. Dashboard and Reports

## Current Development Phase

**Adaptive dataset profiling, preprocessing, analysis, and forecasting**

The upload flow profiles columns and values, detects sales fields using semantic names and value characteristics, maps recognized fields to an internal schema, preserves optional dimensions, and saves a cleaned copy. CSVs with sales but no usable date remain available for sales analysis; forecasting requires both a usable date and sales field.

## Project Folder Structure

```text
sales-forecasting-system/
├── frontend/
│   ├── index.html
│   ├── pages/
│   ├── css/
│   │   └── style.css
│   ├── js/
│   │   └── app.js
│   └── assets/
├── backend/
│   ├── R/
│   │   ├── preprocessing/
│   │   │   ├── column_detector.R
│   │   │   ├── date_parser.R
│   │   │   ├── schema_mapper.R
│   │   │   └── preprocess.R
│   │   ├── analysis/
│   │   │   └── analysis.R
│   │   ├── forecasting/
│   │   │   └── forecast.R
│   │   ├── evaluation/
│   │   │   └── metrics.R
│   │   ├── utils/
│   │   │   └── helpers.R
│   │   └── main.R
│   └── api/
│       └── plumber.R
├── data/
│   ├── raw/
│   │   ├── kaggle_superstore_sales.csv
│   │   └── sample_sales.csv
│   └── processed/
├── reports/
├── tests/
├── README.md
└── .gitignore
```

## Opening the Project

1. Open the `sales-forecasting-system` folder in VS Code or RStudio.
2. Install R and RStudio separately if they are not already available.
3. Install the required R packages once:

	```r
	options(repos = c(CRAN = "https://cloud.r-project.org"))
	install.packages(c("plumber", "jsonlite", "dplyr", "tidyr", "lubridate", "readr", "forecast", "tseries"), type = "binary")
	```

4. From the project root, start the API in R or RStudio:

	```r
	source("backend/api/plumber.R")
	api <- plumber::plumb("backend/api/plumber.R")
	api$run(host = "127.0.0.1", port = 8000)
	```

5. Open `frontend/index.html` in a web browser and navigate to Upload Dataset.

The upload page sends files to `http://localhost:8000/upload`. CORS is configured for local development only. The API profiles the input, maps detected fields to a flexible internal schema, cleans numeric and date values, derives totals where possible, and saves the cleaned result in `data/processed/`. The forecast page sends model and period selections to `http://localhost:8000/forecast` and uses the latest processed dataset. The forecasting engine uses the `forecast` package for ETS, Holt-Winters, and ARIMA when installed; base-R `stats` fallbacks remain available for offline environments.

A web server is not required for this initial static page.

## Flexible Sales CSVs

The preprocessing layer does not require a fixed Kaggle dataset name or one specific set of headers. It normalizes column names, combines semantic matching with value-based checks, and maps fields such as dates, revenue, units sold, products, stores, categories, regions, customers, transaction IDs, profit, discounts, and costs when identifiable. It can derive sales from quantity × unit price and parse common currency and date/month-year formats. Ambiguous day/month dates are rejected with an actionable message instead of being guessed.

Date and sales/revenue (or quantity and unit price) are the minimum for forecasting. Sales-only datasets without dates remain suitable for sales analysis. Missing optional dimensions do not block processing. Exact duplicate rows and repeated transaction IDs are reported but retained to avoid silently discarding legitimate records. Upload responses include a dataset profile, source-to-schema mappings, confidence scores, data quality counts, derived fields, and forecasting availability.

Run the adaptive schema checks from the project root with:

```sh
Rscript tests/test_adaptive_schema.R
```

## Sample Kaggle Dataset

The project includes `data/raw/kaggle_superstore_sales.csv`, the training CSV from Kaggle's [Superstore Sales Dataset](https://www.kaggle.com/datasets/rohitsahoo/sales-forecasting), uploaded by Rohit Sahoo and licensed under [GPL-2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html). The dataset contains 9,800 retail orders with order dates, sales, products, customers, and regions. Its `Order Date`, `Sales`, `Product Name`, and `Customer Name` columns are supported by the upload preprocessing pipeline.

To use it, start the API, open the Upload Dataset page, and select this CSV. The original dataset is kept in `data/raw/`; the API creates a cleaned copy in `data/processed/`.

## Future Development Phases

1. Expand supported field inference with additional representative Kaggle schemas.
2. Add finer-grained time-series resampling and configurable aggregation.
3. Add mapping review controls for low-confidence column detections.
