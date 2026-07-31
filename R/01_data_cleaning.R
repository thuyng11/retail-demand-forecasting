library(tidyverse)
library(lubridate)
library(here)
library(janitor)

# read data
calendar <- read_csv(
  here("data", "raw", "calendar.csv")
)

sales <- read_csv(
  here("data", "raw", "sales_train_evaluation.csv")
)

prices <- read_csv(
  here("data", "raw", "sell_prices.csv")
)

# inspect file

cat("Calendar:", dim(calendar), "\n")
cat("Sales:", dim(sales), "\n")
cat("Prices:", dim(prices), "\n")

# standardize col names
calendar <- clean_names(calendar)
sales <- clean_names(sales)
prices <- clean_names(prices)


# missing summary function
missing_summary <- function(df){
  tibble(
    variable = names(df),
    missing = colSums(is.na(df)),
    percent = round(
      colMeans(is.na(df))*100,
      2
    )
  )
}

# event_name_1 and 2 are NAs since most days aren't holidays
missing_summary(calendar)

# no missing
missing_summary(prices)

# no missing
missing_summary(sales)

# check duplicate
sum(duplicated(calendar)) # 0

sum(duplicated(prices)) # 0

sum(duplicated(sales)) # 0

# transform wide -> long format
# Limit the initial project scope before reshaping
sales_subset <- sales %>%
  filter(store_id == "CA_1")

# Convert daily columns from wide to long format
sales_complete <- sales_subset %>%
  pivot_longer(
    cols = starts_with("d_"),
    names_to = "d",
    values_to = "sales"
  ) %>%
  left_join(
    calendar,
    by = "d"
  ) %>%
  left_join(
    prices,
    by = c("store_id", "item_id", "wm_yr_wk")
  ) %>%
  mutate(
    date = as.Date(date),
    is_event = if_else(
      is.na(event_name_1) & is.na(event_name_2),
      0L,
      1L
    ),
    is_weekend = if_else(
      weekday %in% c("Saturday", "Sunday"),
      1L,
      0L
    ),
    revenue = sales * sell_price
  )

# validate
sales_complete %>%
  summarise(
    rows = n(),
    items = n_distinct(item_id),
    stores = n_distinct(store_id),
    min_date = min(date),
    max_date = max(date),
    missing_dates = sum(is.na(date)),
    missing_prices = sum(is.na(sell_price)),
    missing_price_pct = round(mean(is.na(sell_price)) * 100, 2)
  )

# investigate missing price
price_missing_summary <- sales_complete %>%
  group_by(price_missing = is.na(sell_price)) %>%
  summarise(
    rows = n(),
    zero_sales_rows = sum(sales == 0),
    positive_sales_rows = sum(sales > 0),
    total_units_sold = sum(sales, na.rm = TRUE),
    average_units_sold = mean(sales, na.rm = TRUE),
    .groups = "drop"
  )

price_missing_summary

sales_complete %>%
  mutate(year_month = floor_date(date, "month")) %>%
  group_by(year_month) %>%
  summarise(
    rows = n(),
    missing_prices = sum(is.na(sell_price)),
    missing_price_pct = round(
      mean(is.na(sell_price)) * 100,
      2
    ),
    .groups = "drop"
  ) %>%
  arrange(year_month)

# save processed datasets
library(arrow)

write_parquet(
  sales_complete,
  here(
    "data",
    "processed",
    "sales_complete_ca1.parquet"
  )
)

daily_category_sales <-
  sales_complete %>%
  group_by(
    date,
    store_id,
    state_id,
    cat_id,
    weekday,
    wday,
    month,
    year,
    event_name_1,
    event_type_1,
    event_name_2,
    event_type_2,
    snap_ca,
    is_event,
    is_weekend
  ) %>%
  summarise(
    units_sold = sum(sales),
    
    revenue = sum(revenue, na.rm = TRUE),
    
    average_price =
      weighted.mean(
        sell_price,
        w = pmax(sales, 1),
        na.rm = TRUE
      ),
    
    active_items =
      n_distinct(item_id[sales > 0]),
    
    .groups = "drop"
  )

write_csv(
  daily_category_sales,
  here(
    "data",
    "processed",
    "daily_category_sales_ca1.csv"
  )
)