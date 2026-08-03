############################################################
# Retail Demand Intelligence
#
# Script: 03_feature_engineering.R
#
# Purpose:
# Create forecasting-ready features from cleaned retail data.
#
# Inputs:
# data/processed/daily_category_sales_ca1.csv
# data/processed/selected_product_sales_ca1.parquet
#
# Outputs:
# outputs/category_features_ca1.parquet
# outputs/product_features_ca1.parquet
# outputs/feature_summary.csv
# outputs/feature_dictionary.csv
#
# Important:
# Rolling sales features use historical values only.
# Current-day sales are never included in predictors intended
# to forecast current-day demand.
############################################################


# ==========================================================
# 1. Load packages
# ==========================================================

library(tidyverse)
library(lubridate)
library(here)
library(arrow)
library(slider)


# ==========================================================
# 2. Configuration
# ==========================================================

# Store analyzed in this project
project_store <- "CA_1"

# Minimum history required for the largest lag feature
maximum_lag <- 28L

# Input paths
category_input_path <- here(
  "data",
  "processed",
  "daily_category_sales_ca1.csv"
)

product_input_path <- here(
  "data",
  "processed",
  "selected_product_sales_ca1.parquet"
)

# Output paths
category_output_path <- here(
  "outputs",
  "category_features_ca1.parquet"
)

product_output_path <- here(
  "outputs",
  "product_features_ca1.parquet"
)

feature_summary_path <- here(
  "outputs",
  "feature_summary.csv"
)

feature_dictionary_path <- here(
  "outputs",
  "feature_dictionary.csv"
)

# Ensure the output directory exists
dir.create(
  here("outputs"),
  recursive = TRUE,
  showWarnings = FALSE
)


# ==========================================================
# 3. Check that input files exist
# ==========================================================

if (!file.exists(category_input_path)) {
  stop(
    paste(
      "Category-level input file was not found:",
      category_input_path
    )
  )
}

if (!file.exists(product_input_path)) {
  stop(
    paste(
      "Product-level input file was not found:",
      product_input_path
    )
  )
}


# ==========================================================
# 4. Import processed datasets
# ==========================================================

category_sales <- readr::read_csv(
  category_input_path,
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(date)
  )

product_sales <- arrow::read_parquet(
  product_input_path
) %>%
  as_tibble() %>%
  mutate(
    date = as.Date(date)
  )


# ==========================================================
# 5. Standardize expected column names
# ==========================================================

# Earlier versions of the cleaning pipeline may have called
# this variable average_selling_price instead of average_price.

if (
  "average_selling_price" %in% names(category_sales) &&
  !"average_price" %in% names(category_sales)
) {
  category_sales <- category_sales %>%
    rename(
      average_price = average_selling_price
    )
}

# If no category price field exists, create one as missing.
# This keeps the pipeline from failing, but pricing features
# will not be usable until the cleaning script exports price.

if (!"average_price" %in% names(category_sales)) {
  warning(
    paste(
      "No average_price column was found.",
      "Category-level price features will be missing."
    )
  )
  
  category_sales <- category_sales %>%
    mutate(
      average_price = NA_real_
    )
}

# Confirm that important product fields are available
required_product_columns <- c(
  "date",
  "item_id",
  "dept_id",
  "cat_id",
  "sales",
  "sell_price",
  "is_event",
  "is_weekend",
  "snap_ca"
)

missing_product_columns <- setdiff(
  required_product_columns,
  names(product_sales)
)

if (length(missing_product_columns) > 0) {
  stop(
    paste(
      "The product dataset is missing required columns:",
      paste(missing_product_columns, collapse = ", ")
    )
  )
}


# ==========================================================
# 6. Validate input data
# ==========================================================

# Check for duplicate category-date records
category_duplicates <- category_sales %>%
  count(cat_id, date) %>%
  filter(n > 1)

if (nrow(category_duplicates) > 0) {
  stop(
    "Duplicate category-date records were found."
  )
}

# Check for duplicate item-date records
product_duplicates <- product_sales %>%
  count(item_id, date) %>%
  filter(n > 1)

if (nrow(product_duplicates) > 0) {
  stop(
    "Duplicate item-date records were found."
  )
}

# Check date ranges
cat(
  "Category date range:",
  as.character(min(category_sales$date)),
  "to",
  as.character(max(category_sales$date)),
  "\n"
)

cat(
  "Product date range:",
  as.character(min(product_sales$date)),
  "to",
  as.character(max(product_sales$date)),
  "\n"
)


# ==========================================================
# 7. Helper: create calendar features
# ==========================================================

add_calendar_features <- function(data) {
  
  data %>%
    mutate(
      # Standard calendar fields
      calendar_year = year(date),
      calendar_month = month(date),
      calendar_week = isoweek(date),
      calendar_quarter = quarter(date),
      day_of_month = day(date),
      day_of_year = yday(date),
      
      # ISO weekday:
      # Monday = 1 and Sunday = 7
      weekday_number = wday(
        date,
        week_start = 1
      ),
      
      # Beginning and end of calendar periods
      is_month_start = as.integer(
        day_of_month == 1
      ),
      
      is_month_end = as.integer(
        date == ceiling_date(date, "month") - days(1)
      ),
      
      is_quarter_start = as.integer(
        date == floor_date(date, "quarter")
      ),
      
      is_quarter_end = as.integer(
        date == ceiling_date(date, "quarter") - days(1)
      ),
      
      # Cyclical encodings allow machine-learning models to
      # recognize that December is close to January and that
      # Sunday is close to Monday.
      month_sin = sin(
        2 * pi * calendar_month / 12
      ),
      
      month_cos = cos(
        2 * pi * calendar_month / 12
      ),
      
      weekday_sin = sin(
        2 * pi * weekday_number / 7
      ),
      
      weekday_cos = cos(
        2 * pi * weekday_number / 7
      )
    )
}


# ==========================================================
# 8. Helper: calculate distance from event days
# ==========================================================

create_event_calendar <- function(data) {
  
  data %>%
    select(
      date,
      is_event
    ) %>%
    distinct() %>%
    arrange(date) %>%
    mutate(
      is_event = as.integer(is_event),
      
      previous_event_date = if_else(
        is_event == 1L,
        date,
        as.Date(NA)
      ),
      
      next_event_date = if_else(
        is_event == 1L,
        date,
        as.Date(NA)
      )
    ) %>%
    tidyr::fill(
      previous_event_date,
      .direction = "down"
    ) %>%
    tidyr::fill(
      next_event_date,
      .direction = "up"
    ) %>%
    mutate(
      days_since_event = as.integer(
        date - previous_event_date
      ),
      
      days_until_event = as.integer(
        next_event_date - date
      ),
      
      # Identify dates within three days before or after
      # an event. Missing distances are treated as far away.
      within_3_days_of_event = as.integer(
        coalesce(days_since_event, 999L) <= 3L |
          coalesce(days_until_event, 999L) <= 3L
      ),
      
      within_7_days_of_event = as.integer(
        coalesce(days_since_event, 999L) <= 7L |
          coalesce(days_until_event, 999L) <= 7L
      )
    ) %>%
    select(
      date,
      days_since_event,
      days_until_event,
      within_3_days_of_event,
      within_7_days_of_event
    )
}

event_calendar <- create_event_calendar(
  category_sales
)


# ==========================================================
# 9. Helper: create historical sales features
# ==========================================================

add_sales_history_features <- function(
    data,
    grouping_variables,
    target_variable
) {
  
  data %>%
    group_by(
      across(
        all_of(grouping_variables)
      )
    ) %>%
    arrange(
      date,
      .by_group = TRUE
    ) %>%
    mutate(
      # Time index within each series
      series_day_index = row_number(),
      
      # Lag features
      lag_1 = lag(
        .data[[target_variable]],
        n = 1
      ),
      
      lag_7 = lag(
        .data[[target_variable]],
        n = 7
      ),
      
      lag_14 = lag(
        .data[[target_variable]],
        n = 14
      ),
      
      lag_28 = lag(
        .data[[target_variable]],
        n = 28
      ),
      
      # Rolling features are calculated on lagged demand.
      # This prevents the current target from leaking into
      # the current row's predictors.
      rolling_mean_7 = slider::slide_dbl(
        lag(.data[[target_variable]], 1),
        ~ mean(.x, na.rm = TRUE),
        .before = 6,
        .complete = TRUE
      ),
      
      rolling_mean_14 = slider::slide_dbl(
        lag(.data[[target_variable]], 1),
        ~ mean(.x, na.rm = TRUE),
        .before = 13,
        .complete = TRUE
      ),
      
      rolling_mean_28 = slider::slide_dbl(
        lag(.data[[target_variable]], 1),
        ~ mean(.x, na.rm = TRUE),
        .before = 27,
        .complete = TRUE
      ),
      
      rolling_sd_7 = slider::slide_dbl(
        lag(.data[[target_variable]], 1),
        ~ sd(.x, na.rm = TRUE),
        .before = 6,
        .complete = TRUE
      ),
      
      rolling_sd_28 = slider::slide_dbl(
        lag(.data[[target_variable]], 1),
        ~ sd(.x, na.rm = TRUE),
        .before = 27,
        .complete = TRUE
      ),
      
      rolling_min_7 = slider::slide_dbl(
        lag(.data[[target_variable]], 1),
        ~ min(.x, na.rm = TRUE),
        .before = 6,
        .complete = TRUE
      ),
      
      rolling_max_7 = slider::slide_dbl(
        lag(.data[[target_variable]], 1),
        ~ max(.x, na.rm = TRUE),
        .before = 6,
        .complete = TRUE
      ),
      
      # Share of zero-sales days in the previous 28 days
      zero_sales_rate_28 = slider::slide_dbl(
        lag(.data[[target_variable]], 1),
        ~ mean(.x == 0, na.rm = TRUE),
        .before = 27,
        .complete = TRUE
      ),
      
      # Difference from recent historical demand
      difference_from_lag_7 =
        lag_1 - lag_7,
      
      difference_from_mean_28 =
        lag_1 - rolling_mean_28
    ) %>%
    ungroup()
}


# ==========================================================
# 10. Helper: create price features
# ==========================================================

add_price_features <- function(
    data,
    grouping_variables,
    price_variable
) {
  
  data %>%
    group_by(
      across(
        all_of(grouping_variables)
      )
    ) %>%
    arrange(
      date,
      .by_group = TRUE
    ) %>%
    mutate(
      price_missing = as.integer(
        is.na(.data[[price_variable]])
      ),
      
      previous_price = lag(
        .data[[price_variable]],
        n = 1
      ),
      
      price_change_1 =
        .data[[price_variable]] -
        previous_price,
      
      price_pct_change_1 = case_when(
        is.na(.data[[price_variable]]) ~ NA_real_,
        is.na(previous_price) ~ NA_real_,
        previous_price == 0 ~ NA_real_,
        TRUE ~ (
          .data[[price_variable]] -
            previous_price
        ) / previous_price
      ),
      
      is_price_decrease = as.integer(
        !is.na(price_change_1) &
          price_change_1 < 0
      ),
      
      is_price_increase = as.integer(
        !is.na(price_change_1) &
          price_change_1 > 0
      ),
      
      # Historical rolling average uses only prior prices.
      rolling_price_mean_28 = slider::slide_dbl(
        lag(.data[[price_variable]], 1),
        ~ {
          if (all(is.na(.x))) {
            NA_real_
          } else {
            mean(.x, na.rm = TRUE)
          }
        },
        .before = 27,
        .complete = TRUE
      ),
      
      price_relative_to_28_day_mean = case_when(
        is.na(.data[[price_variable]]) ~ NA_real_,
        is.na(rolling_price_mean_28) ~ NA_real_,
        rolling_price_mean_28 == 0 ~ NA_real_,
        TRUE ~
          .data[[price_variable]] /
          rolling_price_mean_28
      )
    ) %>%
    ungroup()
}


# ==========================================================
# 11. Category-level feature engineering
# ==========================================================

category_features <- category_sales %>%
  arrange(
    cat_id,
    date
  ) %>%
  add_calendar_features() %>%
  left_join(
    event_calendar,
    by = "date"
  ) %>%
  add_sales_history_features(
    grouping_variables = "cat_id",
    target_variable = "units_sold"
  ) %>%
  add_price_features(
    grouping_variables = "cat_id",
    price_variable = "average_price"
  ) %>%
  mutate(
    # Ensure event, weekend, and SNAP fields are numeric
    is_event = as.integer(is_event),
    is_weekend = as.integer(is_weekend),
    snap_ca = as.integer(snap_ca),
    
    # Category is retained as a categorical predictor
    cat_id = factor(cat_id),
    
    # Identify rows with enough historical information
    has_complete_sales_history = as.integer(
      !is.na(lag_28) &
        !is.na(rolling_mean_28)
    )
  )


# ==========================================================
# 12. Product-level feature engineering
# ==========================================================

product_features <- product_sales %>%
  arrange(
    item_id,
    date
  ) %>%
  add_calendar_features() %>%
  left_join(
    event_calendar,
    by = "date"
  ) %>%
  add_sales_history_features(
    grouping_variables = "item_id",
    target_variable = "sales"
  ) %>%
  add_price_features(
    grouping_variables = "item_id",
    price_variable = "sell_price"
  ) %>%
  mutate(
    is_event = as.integer(is_event),
    is_weekend = as.integer(is_weekend),
    snap_ca = as.integer(snap_ca),
    
    item_id = factor(item_id),
    dept_id = factor(dept_id),
    cat_id = factor(cat_id),
    
    has_complete_sales_history = as.integer(
      !is.na(lag_28) &
        !is.na(rolling_mean_28)
    )
  )


# ==========================================================
# 13. Remove initial rows without enough sales history
# ==========================================================

# The first 28 observations for each series cannot have a
# 28-day lag or rolling feature. These rows are excluded from
# the model-ready outputs rather than being imputed.

category_features_model <- category_features %>%
  filter(
    has_complete_sales_history == 1L
  )

product_features_model <- product_features %>%
  filter(
    has_complete_sales_history == 1L
  )


# ==========================================================
# 14. Validate engineered features
# ==========================================================

category_validation <- category_features_model %>%
  summarise(
    rows = n(),
    categories = n_distinct(cat_id),
    min_date = min(date),
    max_date = max(date),
    missing_target = sum(is.na(units_sold)),
    missing_lag_28 = sum(is.na(lag_28)),
    missing_rolling_mean_28 = sum(
      is.na(rolling_mean_28)
    )
  )

product_validation <- product_features_model %>%
  summarise(
    rows = n(),
    products = n_distinct(item_id),
    min_date = min(date),
    max_date = max(date),
    missing_target = sum(is.na(sales)),
    missing_lag_28 = sum(is.na(lag_28)),
    missing_rolling_mean_28 = sum(
      is.na(rolling_mean_28)
    ),
    missing_prices = sum(is.na(sell_price)),
    missing_price_pct = round(
      mean(is.na(sell_price)) * 100,
      2
    )
  )

cat("\nCategory feature validation:\n")
print(category_validation)

cat("\nProduct feature validation:\n")
print(product_validation)


# ==========================================================
# 15. Check for accidental target leakage
# ==========================================================

# Rolling features should not equal the current target simply
# because the target was included in their calculation.
# Correlation can still be high because demand is temporal,
# but the feature definitions only use prior observations.

category_leakage_check <- category_features_model %>%
  summarise(
    correlation_target_lag_1 = cor(
      units_sold,
      lag_1,
      use = "complete.obs"
    ),
    
    correlation_target_rolling_7 = cor(
      units_sold,
      rolling_mean_7,
      use = "complete.obs"
    )
  )

product_leakage_check <- product_features_model %>%
  summarise(
    correlation_target_lag_1 = cor(
      sales,
      lag_1,
      use = "complete.obs"
    ),
    
    correlation_target_rolling_7 = cor(
      sales,
      rolling_mean_7,
      use = "complete.obs"
    )
  )

cat("\nCategory historical-feature correlations:\n")
print(category_leakage_check)

cat("\nProduct historical-feature correlations:\n")
print(product_leakage_check)


# ==========================================================
# 16. Create feature summary for Shiny
# ==========================================================

feature_summary <- tibble(
  dataset = c(
    "Category-level",
    "Product-level"
  ),
  
  rows = c(
    nrow(category_features_model),
    nrow(product_features_model)
  ),
  
  series = c(
    n_distinct(category_features_model$cat_id),
    n_distinct(product_features_model$item_id)
  ),
  
  feature_count = c(
    ncol(category_features_model) - 1,
    ncol(product_features_model) - 1
  ),
  
  minimum_date = c(
    min(category_features_model$date),
    min(product_features_model$date)
  ),
  
  maximum_date = c(
    max(category_features_model$date),
    max(product_features_model$date)
  )
)

write_csv(
  feature_summary,
  feature_summary_path
)


# ==========================================================
# 17. Create a feature dictionary
# ==========================================================

feature_dictionary <- tribble(
  ~feature, ~group, ~description,
  
  "lag_1",
  "Historical demand",
  "Demand observed one day earlier",
  
  "lag_7",
  "Historical demand",
  "Demand observed seven days earlier",
  
  "lag_14",
  "Historical demand",
  "Demand observed fourteen days earlier",
  
  "lag_28",
  "Historical demand",
  "Demand observed twenty-eight days earlier",
  
  "rolling_mean_7",
  "Rolling demand",
  "Mean demand during the previous seven days",
  
  "rolling_mean_14",
  "Rolling demand",
  "Mean demand during the previous fourteen days",
  
  "rolling_mean_28",
  "Rolling demand",
  "Mean demand during the previous twenty-eight days",
  
  "rolling_sd_7",
  "Rolling demand",
  "Demand variability during the previous seven days",
  
  "rolling_sd_28",
  "Rolling demand",
  "Demand variability during the previous twenty-eight days",
  
  "zero_sales_rate_28",
  "Demand intermittency",
  "Share of the previous twenty-eight days with zero sales",
  
  "calendar_month",
  "Calendar",
  "Numeric calendar month",
  
  "calendar_week",
  "Calendar",
  "ISO calendar week",
  
  "calendar_quarter",
  "Calendar",
  "Calendar quarter",
  
  "weekday_number",
  "Calendar",
  "Day of the week, with Monday equal to one",
  
  "month_sin",
  "Calendar",
  "Cyclical sine encoding of month",
  
  "month_cos",
  "Calendar",
  "Cyclical cosine encoding of month",
  
  "weekday_sin",
  "Calendar",
  "Cyclical sine encoding of weekday",
  
  "weekday_cos",
  "Calendar",
  "Cyclical cosine encoding of weekday",
  
  "is_month_start",
  "Calendar",
  "Indicates the first day of a month",
  
  "is_month_end",
  "Calendar",
  "Indicates the final day of a month",
  
  "is_event",
  "Events",
  "Indicates a recorded holiday or special event",
  
  "days_since_event",
  "Events",
  "Number of days since the most recent event",
  
  "days_until_event",
  "Events",
  "Number of days until the next event",
  
  "within_3_days_of_event",
  "Events",
  "Indicates dates within three days of an event",
  
  "snap_ca",
  "External factor",
  "Indicates a California SNAP purchasing day",
  
  "price_change_1",
  "Pricing",
  "Difference from the previous observed price",
  
  "price_pct_change_1",
  "Pricing",
  "Percentage change from the previous observed price",
  
  "is_price_decrease",
  "Pricing",
  "Indicates that price decreased from the previous day",
  
  "rolling_price_mean_28",
  "Pricing",
  "Mean historical price during the previous twenty-eight days",
  
  "price_relative_to_28_day_mean",
  "Pricing",
  "Current price divided by its prior twenty-eight-day mean"
)

write_csv(
  feature_dictionary,
  feature_dictionary_path
)


# ==========================================================
# 18. Save model-ready feature datasets
# ==========================================================

arrow::write_parquet(
  category_features_model,
  category_output_path,
  compression = "snappy"
)

arrow::write_parquet(
  product_features_model,
  product_output_path,
  compression = "snappy"
)


# ==========================================================
# 19. Final output message
# ==========================================================

cat(
  "\nFeature engineering completed successfully.\n",
  "\nCategory features saved to:\n",
  category_output_path,
  "\n\nProduct features saved to:\n",
  product_output_path,
  "\n\nFeature summary saved to:\n",
  feature_summary_path,
  "\n\nFeature dictionary saved to:\n",
  feature_dictionary_path,
  "\n"
)