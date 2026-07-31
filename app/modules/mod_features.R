features_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "dashboard-section-title h3",
      "Feature engineering"
    ),
    
    div(
      class = "dashboard-section-subtitle",
      "Transform historical retail data into forecasting-ready predictors."
    ),
    
    layout_column_wrap(
      width = 1 / 4,
      
      value_box(
        "Calendar features",
        "Planned",
        showcase = bsicons::bs_icon("calendar-event")
      ),
      
      value_box(
        "Lag features",
        "Planned",
        showcase = bsicons::bs_icon("clock-history")
      ),
      
      value_box(
        "Rolling features",
        "Planned",
        showcase = bsicons::bs_icon("activity")
      ),
      
      value_box(
        "Price features",
        "Planned",
        showcase = bsicons::bs_icon("tag")
      )
    ),
    
    card(
      card_header("Planned feature pipeline"),
      
      div(
        class = "placeholder-panel",
        
        bsicons::bs_icon(
          "diagram-3",
          size = "3rem"
        ),
        
        h4("Feature pipeline coming next"),
        
        p(
          "This page will document lag variables, rolling statistics, ",
          "price changes, events, SNAP indicators, and leakage prevention."
        )
      )
    )
  )
}

features_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}