methodology_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    div(
      class = "dashboard-section-title h3",
      "Methodology"
    ),
    
    div(
      class = "dashboard-section-subtitle",
      "Project workflow, data processing pipeline, and modeling approach."
    ),
    
    layout_columns(
      
      col_widths = c(8,4),
      
      card(
        
        card_header("Project Workflow"),
        
        tags$ol(
          
          tags$li(
            strong("Data Collection"),
            br(),
            "Import M5 Forecasting competition datasets."
          ),
          
          tags$li(
            strong("Data Cleaning"),
            br(),
            "Check missing values, duplicates, merge calendar and pricing information."
          ),
          
          tags$li(
            strong("Exploratory Data Analysis"),
            br(),
            "Analyze demand trends, seasonality, product behavior, holidays, pricing, and SNAP."
          ),
          
          tags$li(
            strong("Feature Engineering"),
            br(),
            "Generate lag variables, rolling statistics, calendar features, and pricing indicators."
          ),
          
          tags$li(
            strong("Forecast Modeling"),
            br(),
            "Train forecasting models and evaluate predictive performance."
          ),
          
          tags$li(
            strong("Business Recommendations"),
            br(),
            "Translate forecasts into inventory planning insights."
          )
          
        )
        
      ),
      
      card(
        
        card_header("Project Information"),
        
        tags$b("Dataset"),
        tags$p("M5 Forecasting Accuracy Competition"),
        
        tags$b("Store"),
        tags$p("CA_1"),
        
        tags$b("Products"),
        tags$p("3,049"),
        
        tags$b("Time Period"),
        tags$p("2011 - 2016"),
        
        tags$b("Language"),
        tags$p("R")
        
      )
      
    ),
    
    br(),
    
    card(
      
      card_header("Analytics Pipeline"),
      
      tags$pre("
Raw Data
     │
     ▼
01_data_cleaning.R
     │
     ▼
02_eda.R
     │
     ▼
03_feature_engineering.R
     │
     ▼
04_forecasting.R
     │
     ▼
05_inventory_analysis.R
     │
     ▼
Interactive Shiny Dashboard
")
      
    ),
    
    br(),
    
    layout_columns(
      
      col_widths = c(6,6),
      
      card(
        
        card_header("Current Status"),
        
        tags$table(
          
          class = "table",
          
          tags$thead(
            
            tags$tr(
              tags$th("Module"),
              tags$th("Status")
            )
            
          ),
          
          tags$tbody(
            
            tags$tr(
              tags$td("Data Cleaning"),
              tags$td("Completed")
            ),
            
            tags$tr(
              tags$td("EDA"),
              tags$td("Completed")
            ),
            
            tags$tr(
              tags$td("Feature Engineering"),
              tags$td("Coming Soon")
            ),
            
            tags$tr(
              tags$td("Forecasting"),
              tags$td("Coming Soon")
            ),
            
            tags$tr(
              tags$td("Inventory Analysis"),
              tags$td("Coming Soon")
            )
            
          )
          
        )
        
      ),
      
      card(
        
        card_header("Tools Used"),
        
        tags$ul(
          
          tags$li("tidyverse"),
          
          tags$li("ggplot2"),
          
          tags$li("Plotly"),
          
          tags$li("Shiny"),
          
          tags$li("Arrow"),
          
          tags$li("zoo"),
          
          tags$li("forecast (planned)"),
          
          tags$li("fable (planned)")
          
        )
        
      )
      
    )
    
  )
  
}

methodology_server <- function(id){
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
    }
    
  )
  
}