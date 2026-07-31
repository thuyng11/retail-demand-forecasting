inventory_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "dashboard-section-title h3",
      "Inventory recommendations"
    ),
    
    div(
      class = "dashboard-section-subtitle",
      "Translate forecasts into simulated replenishment decisions."
    ),
    
    card(
      card_header("Inventory planning workspace"),
      
      div(
        class = "placeholder-panel",
        
        bsicons::bs_icon(
          "boxes",
          size = "3rem"
        ),
        
        h4("Inventory simulation will be added after forecasting"),
        
        p(
          "This page will show stockout risk, safety stock, reorder points, ",
          "and suggested reorder quantities."
        )
      )
    )
  )
}

inventory_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}