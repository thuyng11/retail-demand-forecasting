forecasting_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "dashboard-section-title h3",
      "Demand forecasting"
    ),

    div(
      class = "dashboard-section-subtitle",
      "Compare baseline and advanced forecasting models."
    ),

    layout_column_wrap(
      width = 1 / 4,

      value_box(
        "Forecast horizon",
        "28 days",
        showcase = bsicons::bs_icon("calendar4-week")
      ),

      value_box(
        "Baseline",
        "Seasonal naïve",
        showcase = bsicons::bs_icon("dash-lg")
      ),

      value_box(
        "Candidate model",
        "ETS / XGBoost",
        showcase = bsicons::bs_icon("cpu")
      ),

      value_box(
        "Primary metric",
        "WAPE",
        showcase = bsicons::bs_icon("bullseye")
      )
    ),

    card(
      card_header("Forecast workspace"),

      div(
        class = "placeholder-panel",

        bsicons::bs_icon(
          "graph-up-arrow",
          size = "3rem"
        ),

        h4("Forecasting results will appear here"),

        p(
          "The completed page will include actual versus predicted demand, ",
          "prediction intervals, model comparisons, and error metrics."
        )
      )
    )
  )
}

forecasting_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}