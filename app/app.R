library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)
library(arrow)
library(here)
library(scales)
library(zoo)

# =========================================================
# Load processed data
# =========================================================

daily_sales <- read_csv(
  here(
    "data",
    "processed",
    "daily_category_sales_ca1.csv"
  ),
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(date)
  )

product_summary <- read_csv(
  here(
    "data",
    "processed",
    "product_summary_ca1.csv"
  ),
  show_col_types = FALSE
)

product_sales <- read_parquet(
  here(
    "data",
    "processed",
    "selected_product_sales_ca1.parquet"
  )
) %>%
  mutate(
    date = as.Date(date)
  )

# =========================================================
# Precomputed app summaries
# =========================================================

daily_total <- daily_sales %>%
  group_by(date) %>%
  summarise(
    units = sum(units_sold, na.rm = TRUE),
    revenue = sum(revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(date) %>%
  mutate(
    ma30 = zoo::rollmean(
      units,
      k = 30,
      fill = NA,
      align = "right"
    )
  )

category_summary <- daily_sales %>%
  group_by(cat_id) %>%
  summarise(
    total_units = sum(units_sold, na.rm = TRUE),
    total_revenue = sum(revenue, na.rm = TRUE),
    average_daily_units = mean(units_sold, na.rm = TRUE),
    .groups = "drop"
  )

event_summary <- daily_sales %>%
  group_by(is_event) %>%
  summarise(
    average_units = mean(units_sold, na.rm = TRUE),
    .groups = "drop"
  )

snap_summary <- daily_sales %>%
  group_by(snap_ca) %>%
  summarise(
    average_units = mean(units_sold, na.rm = TRUE),
    .groups = "drop"
  )

# =========================================================
# UI
# =========================================================

ui <- page_navbar(
  title = "Retail Demand Intelligence",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  # -------------------------------------------------------
  # Overview page
  # -------------------------------------------------------
  
  nav_panel(
    "Overview",
    
    layout_columns(
      value_box(
        title = "Total units sold",
        value = comma(sum(daily_total$units, na.rm = TRUE))
      ),
      
      value_box(
        title = "Known revenue",
        value = dollar(
          sum(daily_total$revenue, na.rm = TRUE)
        )
      ),
      
      value_box(
        title = "Products analyzed",
        value = comma(nrow(product_summary))
      ),
      
      value_box(
        title = "Date range",
        value = paste(
          format(min(daily_total$date), "%b %Y"),
          "to",
          format(max(daily_total$date), "%b %Y")
        )
      )
    ),
    
    card(
      card_header("Daily demand trend"),
      plotlyOutput(
        "daily_trend",
        height = "450px"
      )
    ),
    
    card(
      card_header("Executive summary"),
      uiOutput("overview_analysis")
    )
  ),
  
  # -------------------------------------------------------
  # Category page
  # -------------------------------------------------------
  
  nav_panel(
    "Category Analysis",
    
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "category",
          "Category",
          choices = c(
            "All",
            sort(unique(daily_sales$cat_id))
          ),
          selected = "All"
        ),
        
        dateRangeInput(
          "category_dates",
          "Date range",
          start = min(daily_sales$date),
          end = max(daily_sales$date),
          min = min(daily_sales$date),
          max = max(daily_sales$date)
        )
      ),
      
      card(
        card_header("Category demand over time"),
        plotlyOutput(
          "category_trend",
          height = "420px"
        )
      ),
      
      layout_columns(
        card(
          card_header("Total sales by category"),
          plotlyOutput(
            "category_bar",
            height = "350px"
          )
        ),
        
        card(
          card_header("Average demand by weekday"),
          plotlyOutput(
            "weekday_plot",
            height = "350px"
          )
        )
      ),
      
      card(
        card_header("Category interpretation"),
        uiOutput("category_analysis")
      )
    )
  ),
  
  # -------------------------------------------------------
  # Events and SNAP page
  # -------------------------------------------------------
  
  nav_panel(
    "Events and SNAP",
    
    layout_columns(
      card(
        card_header("Event-day demand"),
        plotlyOutput(
          "event_plot",
          height = "350px"
        )
      ),
      
      card(
        card_header("SNAP-day demand"),
        plotlyOutput(
          "snap_plot",
          height = "350px"
        )
      )
    ),
    
    card(
      card_header("Interpretation"),
      uiOutput("event_analysis")
    )
  ),
  
  # -------------------------------------------------------
  # Product page
  # -------------------------------------------------------
  
  nav_panel(
    "Product Analysis",
    
    layout_sidebar(
      sidebar = sidebar(
        selectizeInput(
          "product",
          "Select a product",
          choices = product_summary$item_id,
          selected = product_summary$item_id[[1]],
          options = list(
            maxOptions = 100,
            placeholder = "Search for a product"
          )
        )
      ),
      
      card(
        card_header("Product sales over time"),
        plotlyOutput(
          "product_trend",
          height = "400px"
        )
      ),
      
      layout_columns(
        card(
          card_header("Top-selling products"),
          plotlyOutput(
            "top_products_plot",
            height = "450px"
          )
        ),
        
        card(
          card_header("Selected product metrics"),
          DTOutput("product_table")
        )
      ),
      
      card(
        card_header("Product interpretation"),
        uiOutput("product_analysis")
      )
    )
  ),
  
  # -------------------------------------------------------
  # About page
  # -------------------------------------------------------
  
  nav_panel(
    "About",
    
    card(
      card_header("Project objective"),
      p(
        "This application explores historical retail demand at ",
        "Store CA_1 using the M5 Forecasting dataset. The goal is ",
        "to identify demand patterns that can support forecasting ",
        "and inventory-planning decisions."
      )
    ),
    
    card(
      card_header("Data scope"),
      tags$ul(
        tags$li("Store: CA_1"),
        tags$li("Products: 3,049"),
        tags$li("Categories: FOODS, HOBBIES, HOUSEHOLD"),
        tags$li("Date range: January 2011 to May 2016"),
        tags$li(
          "Price information may be unavailable before certain ",
          "products entered the assortment."
        )
      )
    ),
    
    card(
      card_header("Methods"),
      tags$ul(
        tags$li("Data cleaning and reshaping with tidyverse"),
        tags$li("Calendar and price-data integration"),
        tags$li("Time-series trend and seasonality analysis"),
        tags$li("Product demand and intermittency analysis"),
        tags$li("Interactive reporting with Shiny and Plotly")
      )
    )
  )
)

# =========================================================
# Server
# =========================================================

server <- function(input, output, session) {
  
  # -------------------------------------------------------
  # Overview
  # -------------------------------------------------------
  
  output$daily_trend <- renderPlotly({
    
    plot_data <- daily_total %>%
      mutate(
        tooltip = paste0(
          "Date: ", format(date, "%b %d, %Y"),
          "<br>Units: ", comma(units),
          "<br>30-day average: ",
          comma(round(ma30))
        )
      )
    
    p <- ggplot(
      plot_data,
      aes(x = date)
    ) +
      geom_line(
        aes(
          y = units,
          text = tooltip
        ),
        alpha = 0.3
      ) +
      geom_line(
        aes(
          y = ma30
        ),
        linewidth = 1
      ) +
      scale_y_continuous(
        labels = comma
      ) +
      labs(
        x = NULL,
        y = "Units sold",
        subtitle = "Daily sales and 30-day moving average"
      ) +
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  output$overview_analysis <- renderUI({
    
    highest_day <- daily_total %>%
      slice_max(
        units,
        n = 1,
        with_ties = FALSE
      )
    
    lowest_day <- daily_total %>%
      slice_min(
        units,
        n = 1,
        with_ties = FALSE
      )
    
    tags$div(
      tags$p(
        paste0(
          "The highest observed daily demand was ",
          comma(highest_day$units),
          " units on ",
          format(highest_day$date, "%B %d, %Y"),
          "."
        )
      ),
      
      tags$p(
        paste0(
          "The lowest observed daily demand was ",
          comma(lowest_day$units),
          " units on ",
          format(lowest_day$date, "%B %d, %Y"),
          "."
        )
      ),
      
      tags$p(
        "The 30-day moving average reveals longer-term demand ",
        "patterns while reducing short-term daily noise."
      )
    )
  })
  
  # -------------------------------------------------------
  # Category filters
  # -------------------------------------------------------
  
  filtered_category <- reactive({
    
    req(input$category_dates)
    
    data <- daily_sales %>%
      filter(
        date >= input$category_dates[1],
        date <= input$category_dates[2]
      )
    
    if (input$category != "All") {
      data <- data %>%
        filter(cat_id == input$category)
    }
    
    data
  })
  
  # -------------------------------------------------------
  # Category trend
  # -------------------------------------------------------
  
  output$category_trend <- renderPlotly({
    
    data <- filtered_category() %>%
      group_by(date, cat_id) %>%
      summarise(
        units = sum(units_sold, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(
      data,
      aes(
        x = date,
        y = units,
        color = cat_id
      )
    ) +
      geom_line() +
      scale_y_continuous(
        labels = comma
      ) +
      labs(
        x = NULL,
        y = "Units sold",
        color = "Category"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # -------------------------------------------------------
  # Category bar chart
  # -------------------------------------------------------
  
  output$category_bar <- renderPlotly({
    
    data <- filtered_category() %>%
      group_by(cat_id) %>%
      summarise(
        total_units = sum(units_sold, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(
      data,
      aes(
        x = reorder(cat_id, total_units),
        y = total_units,
        fill = cat_id
      )
    ) +
      geom_col() +
      coord_flip() +
      guides(fill = "none") +
      scale_y_continuous(
        labels = comma
      ) +
      labs(
        x = NULL,
        y = "Total units sold"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # -------------------------------------------------------
  # Weekday chart
  # -------------------------------------------------------
  
  output$weekday_plot <- renderPlotly({
    
    weekday_data <- filtered_category() %>%
      group_by(weekday, wday) %>%
      summarise(
        average_units = mean(
          units_sold,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      arrange(wday) %>%
      mutate(
        weekday = factor(
          weekday,
          levels = weekday
        )
      )
    
    p <- ggplot(
      weekday_data,
      aes(
        x = weekday,
        y = average_units,
        fill = average_units
      )
    ) +
      geom_col() +
      guides(fill = "none") +
      scale_y_continuous(
        labels = comma
      ) +
      labs(
        x = NULL,
        y = "Average units sold"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 35,
          hjust = 1
        )
      )
    
    ggplotly(p)
  })
  
  output$category_analysis <- renderUI({
    
    data <- filtered_category() %>%
      group_by(cat_id) %>%
      summarise(
        total_units = sum(
          units_sold,
          na.rm = TRUE
        ),
        average_units = mean(
          units_sold,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    top_category <- data %>%
      slice_max(
        total_units,
        n = 1,
        with_ties = FALSE
      )
    
    tags$p(
      paste0(
        top_category$cat_id,
        " had the highest sales volume in the selected period, ",
        "with ",
        comma(top_category$total_units),
        " total units sold."
      )
    )
  })
  
  # -------------------------------------------------------
  # Event analysis
  # -------------------------------------------------------
  
  output$event_plot <- renderPlotly({
    
    data <- event_summary %>%
      mutate(
        status = if_else(
          is_event == 1,
          "Event day",
          "Non-event day"
        )
      )
    
    p <- ggplot(
      data,
      aes(
        x = status,
        y = average_units,
        fill = status
      )
    ) +
      geom_col() +
      guides(fill = "none") +
      scale_y_continuous(
        labels = comma
      ) +
      labs(
        x = NULL,
        y = "Average units sold"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$snap_plot <- renderPlotly({
    
    data <- snap_summary %>%
      mutate(
        status = if_else(
          snap_ca == 1,
          "SNAP day",
          "Non-SNAP day"
        )
      )
    
    p <- ggplot(
      data,
      aes(
        x = status,
        y = average_units,
        fill = status
      )
    ) +
      geom_col() +
      guides(fill = "none") +
      scale_y_continuous(
        labels = comma
      ) +
      labs(
        x = NULL,
        y = "Average units sold"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$event_analysis <- renderUI({
    
    event_mean <- event_summary %>%
      filter(is_event == 1) %>%
      pull(average_units)
    
    non_event_mean <- event_summary %>%
      filter(is_event == 0) %>%
      pull(average_units)
    
    event_difference <- (
      event_mean - non_event_mean
    ) / non_event_mean * 100
    
    snap_mean <- snap_summary %>%
      filter(snap_ca == 1) %>%
      pull(average_units)
    
    non_snap_mean <- snap_summary %>%
      filter(snap_ca == 0) %>%
      pull(average_units)
    
    snap_difference <- (
      snap_mean - non_snap_mean
    ) / non_snap_mean * 100
    
    tags$div(
      tags$p(
        paste0(
          "Average category-level demand on event days was ",
          round(abs(event_difference), 1),
          "% ",
          ifelse(
            event_difference >= 0,
            "higher",
            "lower"
          ),
          " than on non-event days."
        )
      ),
      
      tags$p(
        paste0(
          "Average category-level demand on SNAP days was ",
          round(abs(snap_difference), 1),
          "% ",
          ifelse(
            snap_difference >= 0,
            "higher",
            "lower"
          ),
          " than on non-SNAP days."
        )
      ),
      
      tags$p(
        tags$strong("Interpretation note: "),
        "These are descriptive associations and do not establish causality."
      )
    )
  })
  
  # -------------------------------------------------------
  # Product analysis
  # -------------------------------------------------------
  
  selected_product_data <- reactive({
    
    req(input$product)
    
    product_sales %>%
      filter(
        item_id == input$product
      )
  })
  
  output$product_trend <- renderPlotly({
    
    data <- selected_product_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No detailed time-series data is available for this product."
      )
    )
    
    p <- ggplot(
      data,
      aes(
        x = date,
        y = sales
      )
    ) +
      geom_line() +
      scale_y_continuous(
        labels = comma
      ) +
      labs(
        x = NULL,
        y = "Units sold",
        subtitle = input$product
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$top_products_plot <- renderPlotly({
    
    data <- product_summary %>%
      slice_max(
        total_sales,
        n = 10
      )
    
    p <- ggplot(
      data,
      aes(
        x = reorder(item_id, total_sales),
        y = total_sales,
        fill = total_sales
      )
    ) +
      geom_col() +
      coord_flip() +
      guides(fill = "none") +
      scale_y_continuous(
        labels = comma
      ) +
      labs(
        x = NULL,
        y = "Total units sold"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$product_table <- renderDT({
    
    product_summary %>%
      filter(
        item_id == input$product
      ) %>%
      mutate(
        total_sales = comma(total_sales),
        average_daily_sales = round(
          average_daily_sales,
          2
        ),
        zero_sales_pct = percent(
          zero_sales_pct / 100,
          accuracy = 0.1
        ),
        average_price = dollar(
          average_price
        )
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(
          dom = "t",
          pageLength = 1
        )
      )
  })
  
  output$product_analysis <- renderUI({
    
    product <- product_summary %>%
      filter(
        item_id == input$product
      )
    
    req(nrow(product) == 1)
    
    tags$p(
      paste0(
        input$product,
        " recorded ",
        comma(product$total_sales),
        " total units sold. It had zero sales on ",
        round(product$zero_sales_pct, 1),
        "% of observed days."
      )
    )
  })
}

shinyApp(
  ui = ui,
  server = server
)