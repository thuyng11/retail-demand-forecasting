############################################################
# Retail Demand Intelligence
#
# Script: 04_forecasting.R
#
# Purpose:
# Train and evaluate category-level demand forecasting models
# using a chronological 28-day holdout period.
#
# Models:
# 1. Seasonal naive benchmark
# 2. ETS exponential smoothing
# 3. Global XGBoost regression model
#
# Input:
# outputs/category_features_ca1.parquet
#
# Outputs:
# outputs/category_forecasts.csv
# outputs/category_model_metrics.csv
# outputs/category_model_rankings.csv
# outputs/category_residuals.csv
# outputs/xgboost_feature_importance.csv
# outputs/forecasting_metadata.csv
#
# Evaluation design:
# The final 28 days are held out for testing.
# No random train/test split is used.
############################################################


# ==========================================================
# 1. Load packages
# ==========================================================

library(tidyverse)
library(lubridate)
library(here)
library(arrow)

library(tsibble)
library(fable)
library(fabletools)

library(tidymodels)
library(xgboost)


# ==========================================================
# 2. Configuration
# ==========================================================

# Number of days to forecast and evaluate
forecast_horizon <- 28L

# Reproducibility for XGBoost
random_seed <- 2026L

# Input file
feature_input_path <- here(
  "outputs",
  "category_features_ca1.parquet"
)

# Output files
forecast_output_path <- here(
  "outputs",
  "category_forecasts.csv"
)

metrics_output_path <- here(
  "outputs",
  "category_model_metrics.csv"
)

rankings_output_path <- here(
  "outputs",
  "category_model_rankings.csv"
)

residuals_output_path <- here(
  "outputs",
  "category_residuals.csv"
)

importance_output_path <- here(
  "outputs",
  "xgboost_feature_importance.csv"
)

metadata_output_path <- here(
  "outputs",
  "forecasting_metadata.csv"
)

# Ensure output directory exists
dir.create(
  here("outputs"),
  recursive = TRUE,
  showWarnings = FALSE
)


# ==========================================================
# 3. Validate input file
# ==========================================================

if (!file.exists(feature_input_path)) {
  stop(
    paste(
      "Feature dataset not found:",
      feature_input_path,
      "\nRun R/03_feature_engineering.R first."
    )
  )
}


# ==========================================================
# 4. Import engineered category features
# ==========================================================

category_features <- arrow::read_parquet(
  feature_input_path
) %>%
  as_tibble() %>%
  mutate(
    date = as.Date(date),
    cat_id = as.character(cat_id)
  ) %>%
  arrange(
    cat_id,
    date
  )

cat(
  "Imported",
  scales::comma(nrow(category_features)),
  "category-date records.\n"
)


# ==========================================================
# 5. Validate required columns
# ==========================================================

required_columns <- c(
  "date",
  "cat_id",
  "units_sold",
  "lag_28",
  "calendar_month",
  "calendar_week",
  "calendar_quarter",
  "day_of_month",
  "day_of_year",
  "weekday_number",
  "month_sin",
  "month_cos",
  "weekday_sin",
  "weekday_cos",
  "is_month_start",
  "is_month_end",
  "is_quarter_start",
  "is_quarter_end",
  "is_weekend",
  "is_event",
  "within_3_days_of_event",
  "within_7_days_of_event",
  "snap_ca"
)

missing_columns <- setdiff(
  required_columns,
  names(category_features)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "The feature dataset is missing these columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}


# ==========================================================
# 6. Check series completeness
# ==========================================================

series_validation <- category_features %>%
  group_by(cat_id) %>%
  summarise(
    rows = n(),
    unique_dates = n_distinct(date),
    minimum_date = min(date),
    maximum_date = max(date),
    missing_target = sum(is.na(units_sold)),
    .groups = "drop"
  )

print(series_validation)

if (any(series_validation$missing_target > 0)) {
  stop(
    "Missing target values were found in units_sold."
  )
}

if (
  n_distinct(series_validation$rows) != 1 ||
  n_distinct(series_validation$minimum_date) != 1 ||
  n_distinct(series_validation$maximum_date) != 1
) {
  stop(
    paste(
      "The category time series do not have matching",
      "date ranges or observation counts."
    )
  )
}


# ==========================================================
# 7. Create chronological train/test split
# ==========================================================

maximum_date <- max(category_features$date)

# The final 28 dates become the test period.
test_start_date <- maximum_date -
  days(forecast_horizon - 1L)

training_data <- category_features %>%
  filter(
    date < test_start_date
  )

testing_data <- category_features %>%
  filter(
    date >= test_start_date
  )

cat(
  "\nTraining period:",
  as.character(min(training_data$date)),
  "to",
  as.character(max(training_data$date)),
  "\n"
)

cat(
  "Testing period:",
  as.character(min(testing_data$date)),
  "to",
  as.character(max(testing_data$date)),
  "\n"
)

cat(
  "Forecast horizon:",
  forecast_horizon,
  "days per category\n\n"
)


# ==========================================================
# 8. Validate train/test split
# ==========================================================

test_counts <- testing_data %>%
  count(
    cat_id,
    name = "test_rows"
  )

print(test_counts)

if (any(test_counts$test_rows != forecast_horizon)) {
  stop(
    paste(
      "Each category must have exactly",
      forecast_horizon,
      "test observations."
    )
  )
}

if (max(training_data$date) >= min(testing_data$date)) {
  stop(
    "Training and testing periods overlap."
  )
}


# ==========================================================
# 9. Convert data to tsibbles for fable models
# ==========================================================

training_tsibble <- training_data %>%
  select(
    date,
    cat_id,
    units_sold
  ) %>%
  mutate(
    cat_id = factor(cat_id)
  ) %>%
  as_tsibble(
    key = cat_id,
    index = date
  )

testing_tsibble <- testing_data %>%
  select(
    date,
    cat_id,
    units_sold
  ) %>%
  mutate(
    cat_id = factor(cat_id)
  ) %>%
  as_tsibble(
    key = cat_id,
    index = date
  )


# ==========================================================
# 10. Fit seasonal-naive and ETS models
# ==========================================================

# Seasonal naive:
# Predicts demand using the corresponding weekday from the
# prior weekly seasonal cycle.
#
# ETS:
# Automatically selects an exponential-smoothing structure
# for each category.

time_series_models <- training_tsibble %>%
  model(
    seasonal_naive = SNAIVE(
      units_sold ~ lag("week")
    ),
    
    ets = ETS(
      units_sold
    )
  )

cat("Fitted seasonal-naive and ETS models.\n")


# ==========================================================
# 11. Forecast the 28-day test period
# ==========================================================

time_series_forecasts <- time_series_models %>%
  forecast(
    new_data = testing_tsibble
  ) %>%
  as_tibble() %>%
  transmute(
    date = as.Date(date),
    cat_id = as.character(cat_id),
    model = recode(
      .model,
      seasonal_naive = "Seasonal Naive",
      ets = "ETS"
    ),
    
    # Point forecast from the forecast distribution
    predicted = as.numeric(.mean)
  ) %>%
  mutate(
    # Negative unit forecasts have no operational meaning.
    predicted = pmax(
      predicted,
      0
    )
  )


# ==========================================================
# 12. Prepare leakage-safe XGBoost predictors
# ==========================================================

# IMPORTANT:
#
# We do not use lag_1, lag_7, lag_14, or rolling sales
# features in this fixed 28-day holdout evaluation.
#
# For later test dates, those fields were calculated using
# actual sales from earlier days inside the holdout period.
# Using them would leak holdout outcomes into the predictors.
#
# lag_28 is safe here because every test-period lag_28 value
# points to a date in the training period.
#
# Calendar, event, SNAP, and known price fields can also be
# used when they are available before the forecast period.

candidate_xgb_predictors <- c(
  "cat_id",
  "lag_28",
  "calendar_month",
  "calendar_week",
  "calendar_quarter",
  "day_of_month",
  "day_of_year",
  "weekday_number",
  "month_sin",
  "month_cos",
  "weekday_sin",
  "weekday_cos",
  "is_month_start",
  "is_month_end",
  "is_quarter_start",
  "is_quarter_end",
  "is_weekend",
  "is_event",
  "within_3_days_of_event",
  "within_7_days_of_event",
  "snap_ca",
  
  # Pricing variables are included only when present.
  "average_price",
  "price_missing",
  "price_relative_to_28_day_mean"
)

xgb_predictors <- intersect(
  candidate_xgb_predictors,
  names(category_features)
)

cat(
  "\nXGBoost predictors:\n",
  paste(xgb_predictors, collapse = ", "),
  "\n\n"
)

xgb_training_data <- training_data %>%
  select(
    units_sold,
    all_of(xgb_predictors)
  ) %>%
  mutate(
    cat_id = factor(cat_id)
  )

xgb_testing_data <- testing_data %>%
  select(
    date,
    cat_id,
    units_sold,
    all_of(
      setdiff(
        xgb_predictors,
        "cat_id"
      )
    )
  ) %>%
  mutate(
    cat_id = factor(
      cat_id,
      levels = levels(
        xgb_training_data$cat_id
      )
    )
  )


# ==========================================================
# 13. Create XGBoost preprocessing recipe
# ==========================================================

xgb_recipe <- recipe(
  units_sold ~ .,
  data = xgb_training_data
) %>%
  
  # Protect against missing factor levels.
  step_unknown(
    all_nominal_predictors(),
    new_level = "unknown"
  ) %>%
  
  # Convert categories into numeric dummy variables.
  step_dummy(
    all_nominal_predictors(),
    one_hot = TRUE
  ) %>%
  
  # Median imputation is used for price variables that may
  # be missing before products were actively sold.
  step_impute_median(
    all_numeric_predictors()
  ) %>%
  
  # Remove predictors with no variation in training.
  step_zv(
    all_predictors()
  )


# ==========================================================
# 14. Define the XGBoost model
# ==========================================================

available_cores <- parallel::detectCores(
  logical = TRUE
)

xgb_threads <- max(
  1L,
  available_cores - 1L
)

xgb_specification <- boost_tree(
  mode = "regression",
  trees = 800,
  tree_depth = 6,
  min_n = 10,
  learn_rate = 0.03,
  loss_reduction = 0,
  sample_size = 0.8
) %>%
  set_engine(
    "xgboost",
    objective = "reg:squarederror",
    nthread = xgb_threads,
    verbosity = 0
  )


# ==========================================================
# 15. Fit the global XGBoost workflow
# ==========================================================

xgb_workflow <- workflow() %>%
  add_recipe(
    xgb_recipe
  ) %>%
  add_model(
    xgb_specification
  )

set.seed(random_seed)

xgb_fit <- fit(
  xgb_workflow,
  data = xgb_training_data
)

cat("Fitted global XGBoost regression model.\n")


# ==========================================================
# 16. Generate XGBoost test predictions
# ==========================================================

xgb_forecasts <- predict(
  xgb_fit,
  new_data = xgb_testing_data
) %>%
  bind_cols(
    xgb_testing_data %>%
      select(
        date,
        cat_id,
        units_sold
      )
  ) %>%
  transmute(
    date,
    cat_id = as.character(cat_id),
    model = "XGBoost",
    predicted = pmax(
      as.numeric(.pred),
      0
    )
  )


# ==========================================================
# 17. Combine all forecasts with actual demand
# ==========================================================

all_forecasts <- bind_rows(
  time_series_forecasts,
  xgb_forecasts
) %>%
  left_join(
    testing_data %>%
      select(
        date,
        cat_id,
        actual = units_sold
      ),
    by = c(
      "date",
      "cat_id"
    )
  ) %>%
  mutate(
    residual = actual - predicted,
    absolute_error = abs(residual),
    squared_error = residual^2,
    absolute_percentage_error = case_when(
      actual == 0 ~ NA_real_,
      TRUE ~ absolute_error / actual * 100
    )
  ) %>%
  arrange(
    cat_id,
    model,
    date
  )


# ==========================================================
# 18. Validate forecast output
# ==========================================================

expected_forecast_rows <- (
  n_distinct(testing_data$cat_id) *
    forecast_horizon *
    3L
)

if (nrow(all_forecasts) != expected_forecast_rows) {
  stop(
    paste(
      "Unexpected forecast row count.",
      "Expected:",
      expected_forecast_rows,
      "Received:",
      nrow(all_forecasts)
    )
  )
}

if (any(is.na(all_forecasts$actual))) {
  stop(
    "Some forecasts could not be matched to actual demand."
  )
}

if (any(is.na(all_forecasts$predicted))) {
  stop(
    "Some models produced missing predictions."
  )
}


# ==========================================================
# 19. Define forecasting metrics
# ==========================================================

calculate_forecast_metrics <- function(data) {
  
  actual_total <- sum(
    data$actual,
    na.rm = TRUE
  )
  
  tibble(
    observations = nrow(data),
    
    mae = mean(
      abs(
        data$actual -
          data$predicted
      ),
      na.rm = TRUE
    ),
    
    rmse = sqrt(
      mean(
        (
          data$actual -
            data$predicted
        )^2,
        na.rm = TRUE
      )
    ),
    
    wape = if_else(
      actual_total == 0,
      NA_real_,
      sum(
        abs(
          data$actual -
            data$predicted
        ),
        na.rm = TRUE
      ) /
        actual_total *
        100
    ),
    
    forecast_bias = if_else(
      actual_total == 0,
      NA_real_,
      sum(
        data$predicted -
          data$actual,
        na.rm = TRUE
      ) /
        actual_total *
        100
    ),
    
    mean_actual = mean(
      data$actual,
      na.rm = TRUE
    ),
    
    mean_predicted = mean(
      data$predicted,
      na.rm = TRUE
    )
  )
}


# ==========================================================
# 20. Calculate metrics by category and model
# ==========================================================

category_model_metrics <- all_forecasts %>%
  group_by(
    cat_id,
    model
  ) %>%
  group_modify(
    ~ calculate_forecast_metrics(.x)
  ) %>%
  ungroup() %>%
  mutate(
    across(
      c(
        mae,
        rmse,
        wape,
        forecast_bias,
        mean_actual,
        mean_predicted
      ),
      ~ round(.x, 3)
    )
  ) %>%
  arrange(
    cat_id,
    wape
  )