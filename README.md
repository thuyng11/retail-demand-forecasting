# Retail Demand Intelligence

An end-to-end retail demand forecasting project built in R, using the [M5 Forecasting – Accuracy](https://www.kaggle.com/competitions/m5-forecasting-accuracy) dataset. The project covers data cleaning, exploratory analysis, feature engineering, multi-model forecasting, and an interactive Shiny dashboard.

**Research question:** Can we accurately forecast short-term product demand and identify products that may require inventory attention?

---

## Table of Contents

- [Project Overview](#project-overview)
- [Repository Structure](#repository-structure)
- [Dataset](#dataset)
- [Analysis Pipeline](#analysis-pipeline)
- [Forecasting Models](#forecasting-models)
- [Shiny Dashboard](#shiny-dashboard)
- [Getting Started](#getting-started)
- [Key Outputs](#key-outputs)

---

## Project Overview

This project focuses on **Store CA_1** from the M5 dataset — a Walmart store in California. Using 5+ years of daily sales history across three product categories (Food, Hobbies, Household), the project builds and compares three category-level demand forecasting models evaluated on a fixed 28-day holdout period. Results and insights are surfaced through a multi-tab interactive Shiny application.

---

## Repository Structure

```
retail-demand-forecasting/
├── R/
│   ├── 01_data_cleaning.R        # Load, clean, and reshape raw data
│   ├── 02_eda.R                  # Exploratory data analysis & visualizations
│   ├── 03_feature_engineering.R  # Build forecasting-ready features
│   ├── 04_forecasting.R          # Train and evaluate models
│   └── 05_inventory_analysis.R   # Inventory demand analysis
├── app/
│   ├── app.R                     # Shiny application entry point
│   ├── modules/                  # Shiny module files (one per tab)
│   └── www/                      # Static assets (CSS, etc.)
├── data/
│   ├── raw/                      # Original M5 CSV files (not included in repo)
│   └── processed/                # Cleaned and aggregated datasets
├── docs/
│   └── data_dictionary.md        # Variable definitions for all raw files
├── figures/                      # EDA plots (PNG)
├── outputs/                      # Model results, metrics, and feature files
└── retail-demand-forecasting.Rproj
```

---

## Dataset

The project uses three raw files from the M5 Forecasting competition (place them in `data/raw/`):

| File | Description |
|------|-------------|
| `calendar.csv` | Calendar dates, holiday events, and SNAP eligibility flags |
| `sales_train_evaluation.csv` | Daily unit sales in wide format (`d_1` … `d_1941`) |
| `sell_prices.csv` | Weekly product selling prices by store |

See [`docs/data_dictionary.md`](docs/data_dictionary.md) for full variable definitions.

> **Note:** The raw data files are not included in this repository. Download them from the [M5 Forecasting – Accuracy Kaggle competition](https://www.kaggle.com/competitions/m5-forecasting-accuracy/data) and place them in `data/raw/`.

---

## Analysis Pipeline

Run the scripts in order from the project root:

### 1. Data Cleaning (`R/01_data_cleaning.R`)
- Loads `calendar.csv`, `sales_train_evaluation.csv`, and `sell_prices.csv`
- Standardizes column names and checks for missing values and duplicates
- Filters to **Store CA_1** and reshapes sales from wide to long format
- Joins calendar and price data; creates `is_event`, `is_weekend`, and `revenue` columns
- Saves `data/processed/sales_complete_ca1.parquet` and `data/processed/daily_category_sales_ca1.csv`

### 2. Exploratory Data Analysis (`R/02_eda.R`)
- Analyses overall demand trends (daily and 30-day rolling average)
- Examines monthly seasonality, category performance, weekday patterns, holiday effects, SNAP impacts, and price–demand relationships
- Identifies the top 10 products per category for focused analysis
- Saves `data/processed/product_summary_ca1.csv` and `data/processed/selected_product_sales_ca1.parquet`

### 3. Feature Engineering (`R/03_feature_engineering.R`)
- Creates calendar features: month, week, quarter, day-of-month, day-of-year, and cyclic sine/cosine encodings
- Creates lag features (lag_28) to avoid data leakage in the 28-day holdout
- Creates event proximity flags (`within_3_days_of_event`, `within_7_days_of_event`)
- Creates price features: average price, price missingness, and relative price
- Saves `outputs/category_features_ca1.parquet`, `outputs/product_features_ca1.parquet`, `outputs/feature_summary.csv`, and `outputs/feature_dictionary.csv`

### 4. Forecasting (`R/04_forecasting.R`)
- Splits data chronologically: training up to day T−28, testing on the final 28 days
- Trains Seasonal Naive, ETS, and XGBoost models
- Calculates MAE, RMSE, WAPE, and forecast bias per category and model
- Saves forecasts, metrics, rankings, residuals, XGBoost feature importance, and run metadata to `outputs/`

### 5. Inventory Analysis (`R/05_inventory_analysis.R`)
- Identifies products that may require inventory attention based on demand patterns

---

## Forecasting Models

Three models are trained and evaluated at the **category level** (Food, Hobbies, Household):

| Model | Description |
|-------|-------------|
| **Seasonal Naive** | Benchmark — repeats the value from the same weekday one week prior |
| **ETS** | Exponential smoothing with automatic error/trend/seasonality selection via `fable` |
| **XGBoost** | Global regression model trained across all categories using calendar, lag, event, SNAP, and price features via `tidymodels` |

**Evaluation design:** Fixed 28-day chronological holdout — no random splits. Models are ranked by **WAPE** (Weighted Absolute Percentage Error).

**Leakage prevention:** Short-range lag features (lag_1, lag_7, lag_14) and rolling averages are excluded from the XGBoost test predictions because they would incorporate holdout-period actuals. Only lag_28 and calendar/event features that are known before the forecast period are used.

---

## Shiny Dashboard

The interactive dashboard (`app/app.R`) presents the full analysis pipeline in six tabs:

| Tab | Content |
|-----|---------|
| **Overview** | KPI summary cards: total units sold, revenue, products analyzed, date coverage |
| **EDA** | Interactive charts for trends, seasonality, categories, weekdays, holidays, and SNAP |
| **Feature Engineering** | Feature summary table, feature dictionary, and engineered feature visualizations |
| **Forecasting** | Actual vs. predicted charts, model metrics, rankings, residual diagnostics, and XGBoost feature importance |
| **Inventory** | Demand-based inventory signals |
| **Methodology** | Project context, modelling decisions, and data notes |

**To launch the app:**

```r
shiny::runApp("app")
```

---

## Getting Started

### Prerequisites

Install the required R packages:

```r
install.packages(c(
  # Core
  "tidyverse", "lubridate", "here", "janitor", "arrow", "scales", "slider", "zoo",
  # Time series modelling
  "tsibble", "fable", "fabletools",
  # Machine learning
  "tidymodels", "xgboost",
  # Shiny
  "shiny", "bslib", "bsicons", "plotly", "DT", "shinycssloaders", "htmltools"
))
```

### Running the Pipeline

1. Clone the repository and open `retail-demand-forecasting.Rproj` in RStudio.
2. Download the M5 raw data files and place them in `data/raw/`.
3. Run each script in order:

```r
source("R/01_data_cleaning.R")
source("R/02_eda.R")
source("R/03_feature_engineering.R")
source("R/04_forecasting.R")
source("R/05_inventory_analysis.R")
```

4. Launch the Shiny app:

```r
shiny::runApp("app")
```

---

## Key Outputs

| Path | Description |
|------|-------------|
| `data/processed/sales_complete_ca1.parquet` | Cleaned long-format sales data for Store CA_1 |
| `data/processed/daily_category_sales_ca1.csv` | Daily category-level aggregated sales |
| `outputs/category_features_ca1.parquet` | Engineered features at the category level |
| `outputs/category_forecasts.csv` | 28-day holdout forecasts from all three models |
| `outputs/category_model_metrics.csv` | MAE, RMSE, WAPE, and bias by category and model |
| `outputs/category_model_rankings.csv` | Model rankings per category |
| `outputs/category_residuals.csv` | Residual-level diagnostics |
| `outputs/xgboost_feature_importance.csv` | XGBoost gain/cover/frequency per feature |
| `figures/` | EDA plots (PNG) |
