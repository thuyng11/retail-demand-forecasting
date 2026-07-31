eda_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "dashboard-section-title h3",
      "Exploratory data analysis"
    ),
    
    div(
      class = "dashboard-section-subtitle",
      "Explore demand behavior across time, products, categories, and events."
    ),
    
    layout_sidebar(
      sidebar = sidebar(
        open = "desktop",
        width = 300,
        
        selectInput(
          ns("category"),
          "Category",
          choices = c("All"),
          selected = "All"
        ),
        
        dateRangeInput(
          ns("dates"),
          "Date range",
          start = NULL,
          end = NULL
        ),
        
        selectInput(
          ns("metric"),
          "Metric",
          choices = c(
            "Units sold" = "units_sold",
            "Revenue" = "revenue"
          )
        ),
        
        tags$hr(),
        
        p(
          class = "text-muted small",
          "Filters apply to the trend, seasonality, and category views."
        )
      ),
      
      navset_card_tab(
        nav_panel(
          "Demand trends",
          plotlyOutput(ns("trend_plot"), height = "470px"),
          uiOutput(ns("trend_insight"))
        ),
        
        nav_panel(
          "Categories",
          layout_columns(
            card(
              card_header("Category totals"),
              plotlyOutput(ns("category_plot"), height = "380px")
            ),
            card(
              card_header("Monthly category trends"),
              plotlyOutput(ns("monthly_plot"), height = "380px")
            )
          )
        ),
        
        nav_panel(
          "Seasonality",
          layout_columns(
            card(
              card_header("Weekday pattern"),
              plotlyOutput(ns("weekday_plot"), height = "360px")
            ),
            card(
              card_header("Monthly pattern"),
              plotlyOutput(ns("month_plot"), height = "360px")
            )
          )
        ),
        
        nav_panel(
          "Events and SNAP",
          layout_columns(
            card(
              card_header("Event comparison"),
              plotlyOutput(ns("event_plot"), height = "360px")
            ),
            card(
              card_header("SNAP comparison"),
              plotlyOutput(ns("snap_plot"), height = "360px")
            )
          ),
          uiOutput(ns("event_insight"))
        ),
        
        nav_panel(
          "Products",
          layout_columns(
            card(
              card_header("Top products"),
              plotlyOutput(ns("top_products"), height = "440px")
            ),
            card(
              card_header("Demand intermittency"),
              plotlyOutput(ns("intermittency"), height = "440px")
            )
          )
        )
      )
    )
  )
}

eda_server <- function(
    id,
    daily_sales,
    product_summary,
    product_sales
) {
  moduleServer(id, function(input, output, session) {
    
    # Populate filter choices after data loads
    updateSelectInput(
      session,
      "category",
      choices = c(
        "All",
        sort(unique(daily_sales$cat_id))
      ),
      selected = "All"
    )
    
    updateDateRangeInput(
      session,
      "dates",
      start = min(daily_sales$date),
      end = max(daily_sales$date),
      min = min(daily_sales$date),
      max = max(daily_sales$date)
    )
    
    filtered_data <- reactive({
      req(input$dates)
      
      data <- daily_sales %>%
        filter(
          date >= input$dates[1],
          date <= input$dates[2]
        )
      
      if (!is.null(input$category) &&
          input$category != "All") {
        data <- data %>%
          filter(cat_id == input$category)
      }
      
      data
    })
    
    output$trend_plot <- renderPlotly({
      metric_name <- req(input$metric)
      
      data <- filtered_data() %>%
        group_by(date, cat_id) %>%
        summarise(
          metric_value = sum(
            .data[[metric_name]],
            na.rm = TRUE
          ),
          .groups = "drop"
        )
      
      y_label <- if (
        metric_name == "units_sold"
      ) {
        "Units sold"
      } else {
        "Revenue"
      }
      
      p <- ggplot(
        data,
        aes(
          x = date,
          y = metric_value,
          color = cat_id
        )
      ) +
        geom_line() +
        scale_y_continuous(
          labels = if (
            metric_name == "revenue"
          ) {
            scales::label_dollar(
              scale_cut = scales::cut_short_scale()
            )
          } else {
            scales::label_number(
              scale_cut = scales::cut_short_scale()
            )
          }
        ) +
        labs(
          x = NULL,
          y = y_label,
          color = "Category"
        ) +
        theme_minimal(base_size = 12)
      
      ggplotly(p) %>%
        config(displaylogo = FALSE)
    })
    
    output$trend_insight <- renderUI({
      data <- filtered_data() %>%
        group_by(cat_id) %>%
        summarise(
          total_units = sum(
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
      
      div(
        class = "insight-box mt-3",
        paste0(
          top_category$cat_id,
          " recorded the highest demand in the selected period, with ",
          scales::comma(top_category$total_units),
          " units sold."
        )
      )
    })
    
    output$category_plot <- renderPlotly({
      data <- filtered_data() %>%
        group_by(cat_id) %>%
        summarise(
          total_units = sum(
            units_sold,
            na.rm = TRUE
          ),
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
          labels = scales::label_number(
            scale_cut = scales::cut_short_scale()
          )
        ) +
        labs(
          x = NULL,
          y = "Total units sold"
        ) +
        theme_minimal(base_size = 12)
      
      ggplotly(p) %>%
        config(displaylogo = FALSE)
    })
    
    output$monthly_plot <- renderPlotly({
      data <- filtered_data() %>%
        mutate(
          year_month = lubridate::floor_date(
            date,
            unit = "month"
          )
        ) %>%
        group_by(year_month, cat_id) %>%
        summarise(
          total_units = sum(
            units_sold,
            na.rm = TRUE
          ),
          .groups = "drop"
        )
      
      p <- ggplot(
        data,
        aes(
          x = year_month,
          y = total_units,
          color = cat_id
        )
      ) +
        geom_line() +
        scale_y_continuous(
          labels = scales::label_number(
            scale_cut = scales::cut_short_scale()
          )
        ) +
        labs(
          x = NULL,
          y = "Monthly units sold",
          color = "Category"
        ) +
        theme_minimal(base_size = 12)
      
      ggplotly(p) %>%
        config(displaylogo = FALSE)
    })
    
    output$weekday_plot <- renderPlotly({
      data <- filtered_data() %>%
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
        data,
        aes(
          x = weekday,
          y = average_units,
          fill = average_units
        )
      ) +
        geom_col() +
        guides(fill = "none") +
        scale_y_continuous(
          labels = scales::comma
        ) +
        labs(
          x = NULL,
          y = "Average units sold"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          axis.text.x = element_text(
            angle = 30,
            hjust = 1
          )
        )
      
      ggplotly(p) %>%
        config(displaylogo = FALSE)
    })
    
    output$month_plot <- renderPlotly({
      data <- filtered_data() %>%
        group_by(month) %>%
        summarise(
          average_units = mean(
            units_sold,
            na.rm = TRUE
          ),
          .groups = "drop"
        )
      
      p <- ggplot(
        data,
        aes(
          x = factor(
            month,
            levels = 1:12,
            labels = month.abb
          ),
          y = average_units,
          fill = average_units
        )
      ) +
        geom_col() +
        guides(fill = "none") +
        labs(
          x = NULL,
          y = "Average units sold"
        ) +
        theme_minimal(base_size = 12)
      
      ggplotly(p) %>%
        config(displaylogo = FALSE)
    })
    
    output$event_plot <- renderPlotly({
      data <- filtered_data() %>%
        group_by(is_event) %>%
        summarise(
          average_units = mean(
            units_sold,
            na.rm = TRUE
          ),
          .groups = "drop"
        ) %>%
        mutate(
          event_status = if_else(
            is_event == 1,
            "Event day",
            "Non-event day"
          )
        )
      
      p <- ggplot(
        data,
        aes(
          x = event_status,
          y = average_units,
          fill = event_status
        )
      ) +
        geom_col() +
        guides(fill = "none") +
        labs(
          x = NULL,
          y = "Average units sold"
        ) +
        theme_minimal(base_size = 12)
      
      ggplotly(p) %>%
        config(displaylogo = FALSE)
    })
    
    output$snap_plot <- renderPlotly({
      data <- filtered_data() %>%
        group_by(snap_ca) %>%
        summarise(
          average_units = mean(
            units_sold,
            na.rm = TRUE
          ),
          .groups = "drop"
        ) %>%
        mutate(
          snap_status = if_else(
            snap_ca == 1,
            "SNAP day",
            "Non-SNAP day"
          )
        )
      
      p <- ggplot(
        data,
        aes(
          x = snap_status,
          y = average_units,
          fill = snap_status
        )
      ) +
        geom_col() +
        guides(fill = "none") +
        labs(
          x = NULL,
          y = "Average units sold"
        ) +
        theme_minimal(base_size = 12)
      
      ggplotly(p) %>%
        config(displaylogo = FALSE)
    })
    
    output$event_insight <- renderUI({
      event_data <- filtered_data() %>%
        group_by(is_event) %>%
        summarise(
          average_units = mean(
            units_sold,
            na.rm = TRUE
          ),
          .groups = "drop"
        )
      
      event_mean <- event_data %>%
        filter(is_event == 1) %>%
        pull(average_units)
      
      non_event_mean <- event_data %>%
        filter(is_event == 0) %>%
        pull(average_units)
      
      if (
        length(event_mean) == 1 &&
        length(non_event_mean) == 1 &&
        non_event_mean != 0
      ) {
        difference_pct <- (
          event_mean - non_event_mean
        ) / non_event_mean * 100
        
        div(
          class = "insight-box mt-3",
          paste0(
            "Average demand on event days was ",
            round(abs(difference_pct), 1),
            "% ",
            ifelse(
              difference_pct >= 0,
              "higher",
              "lower"
            ),
            " than on non-event days. This is a descriptive association, not a causal estimate."
          )
        )
      }
    })
    
    output$top_products <- renderPlotly({
      data <- product_summary %>%
        slice_max(
          total_sales,
          n = 10,
          with_ties = FALSE
        )
      
      p <- ggplot(
        data,
        aes(
          x = reorder(
            item_id,
            total_sales
          ),
          y = total_sales,
          fill = total_sales
        )
      ) +
        geom_col() +
        coord_flip() +
        guides(fill = "none") +
        scale_y_continuous(
          labels = scales::label_number(
            scale_cut = scales::cut_short_scale()
          )
        ) +
        labs(
          x = NULL,
          y = "Total units sold"
        ) +
        theme_minimal(base_size = 12)
      
      ggplotly(p) %>%
        config(displaylogo = FALSE)
    })
    
    output$intermittency <- renderPlotly({
      p <- ggplot(
        product_summary,
        aes(x = zero_sales_pct)
      ) +
        geom_histogram(
          bins = 40
        ) +
        labs(
          x = "Zero-sales days (%)",
          y = "Number of products"
        ) +
        theme_minimal(base_size = 12)
      
      ggplotly(p) %>%
        config(displaylogo = FALSE)
    })
  })
}