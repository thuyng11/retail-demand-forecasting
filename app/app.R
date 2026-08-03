library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)
library(arrow)
library(scales)
library(zoo)
library(shinycssloaders)
library(htmltools)
library(here)

# Source application modules
source("modules/mod_overview.R", local = TRUE)
source("modules/mod_eda.R", local = TRUE)
source("modules/mod_features.R", local = TRUE)
source("modules/mod_forecasting.R", local = TRUE)
source("modules/mod_inventory.R", local = TRUE)
source("modules/mod_methodology.R", local = TRUE)

# Load data
daily_sales <- readr::read_csv(
  here(
    "data",
    "processed",
    "daily_category_sales_ca1.csv"
  ),
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(date))

product_summary <- readr::read_csv(
  here(
    "data",
    "processed",
    "product_summary_ca1.csv"
  ),
  show_col_types = FALSE
)

product_sales <- arrow::read_parquet(
  here(
    "data",
    "processed",
    "selected_product_sales_ca1.parquet"
  )
) %>%
  mutate(date = as.Date(date))

# =========================================================
# Feature-engineering outputs
# =========================================================

category_features <- arrow::read_parquet(
  here::here(
    "outputs",
    "category_features_ca1.parquet"
  )
) %>%
  as_tibble() %>%
  mutate(
    date = as.Date(date),
    cat_id = as.character(cat_id)
  )

feature_summary <- readr::read_csv(
  here::here(
    "outputs",
    "feature_summary.csv"
  ),
  show_col_types = FALSE
)

feature_dictionary <- readr::read_csv(
  here::here(
    "outputs",
    "feature_dictionary.csv"
  ),
  show_col_types = FALSE
)

# =========================================================
# Forecasting outputs
# =========================================================

category_forecasts <- readr::read_csv(
  here::here(
    "outputs",
    "category_forecasts.csv"
  ),
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(date),
    cat_id = as.character(cat_id),
    model = as.character(model)
  )

category_model_metrics <- readr::read_csv(
  here::here(
    "outputs",
    "category_model_metrics.csv"
  ),
  show_col_types = FALSE
) %>%
  mutate(
    cat_id = as.character(cat_id),
    model = as.character(model)
  )

category_model_rankings <- readr::read_csv(
  here::here(
    "outputs",
    "category_model_rankings.csv"
  ),
  show_col_types = FALSE
) %>%
  mutate(
    cat_id = as.character(cat_id),
    model = as.character(model)
  )

category_residuals <- readr::read_csv(
  here::here(
    "outputs",
    "category_residuals.csv"
  ),
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(date),
    cat_id = as.character(cat_id),
    model = as.character(model)
  )

xgboost_feature_importance <- readr::read_csv(
  here::here(
    "outputs",
    "xgboost_feature_importance.csv"
  ),
  show_col_types = FALSE
)

forecasting_metadata <- readr::read_csv(
  here::here(
    "outputs",
    "forecasting_metadata.csv"
  ),
  show_col_types = FALSE
)

app_theme <- bs_theme(
  version = 5,
  bg = "#F4F6F8",
  fg = "#182230",
  primary = "#2563EB",
  secondary = "#64748B",
  success = "#15803D",
  warning = "#D97706",
  danger = "#DC2626",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  "navbar-bg" = "#FFFFFF",
  "navbar-light-color" = "#475569",
  "navbar-light-hover-color" = "#2563EB",
  "card-border-color" = "#E2E8F0",
  "card-cap-bg" = "#FFFFFF",
  "border-radius" = "0.75rem"
)

ui <- page_navbar(
  title = div(
    class = "brand-title",
    span("Retail Demand"),
    span(class = "brand-accent", "Intelligence")
  ),
  
  id = "main_navigation",
  
  theme = app_theme,
  
  header = tagList(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )
  ),
  
  nav_panel(
    title = tagList(
      bsicons::bs_icon("speedometer2"),
      "Overview"
    ),
    value = "overview",
    overview_ui("overview")
  ),
  
  nav_panel(
    title = tagList(
      bsicons::bs_icon("bar-chart-line"),
      "EDA"
    ),
    value = "eda",
    eda_ui("eda")
  ),
  
  nav_panel(
    title = tagList(
      bsicons::bs_icon("sliders"),
      "Feature Engineering"
    ),
    value = "features",
    features_ui("features")
  ),
  
  nav_panel(
    title = tagList(
      bsicons::bs_icon("graph-up-arrow"),
      "Forecasting"
    ),
    value = "forecasting",
    forecasting_ui("forecasting")
  ),
  
  nav_panel(
    title = tagList(
      bsicons::bs_icon("boxes"),
      "Inventory"
    ),
    value = "inventory",
    inventory_ui("inventory")
  ),
  
  nav_spacer(),
  
  nav_panel(
    title = tagList(
      bsicons::bs_icon("info-circle"),
      "Methodology"
    ),
    value = "methodology",
    methodology_ui("methodology")
  ),
  
  nav_item(
    input_dark_mode(
      id = "dark_mode",
      mode = "light"
    )
  ),
  
  footer = div(
    class = "app-footer",
    "M5 Forecasting portfolio project · Store CA_1"
  )
)

server <- function(input, output, session) {
  
  overview_server(
    "overview",
    daily_sales = daily_sales,
    product_summary = product_summary
  )
  
  eda_server(
    "eda",
    daily_sales = daily_sales,
    product_summary = product_summary,
    product_sales = product_sales
  )
  
  features_server(
    "features",
    category_features = category_features,
    feature_summary = feature_summary,
    feature_dictionary = feature_dictionary
  )
  
  forecasting_server(
    "forecasting",
    category_forecasts = category_forecasts,
    category_model_metrics = category_model_metrics,
    category_model_rankings = category_model_rankings,
    category_residuals = category_residuals,
    xgboost_feature_importance = xgboost_feature_importance,
    forecasting_metadata = forecasting_metadata
  )
  
  inventory_server(
    "inventory"
  )
  
  methodology_server(
    "methodology"
  )
}

shinyApp(ui, server)