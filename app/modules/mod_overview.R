overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "dashboard-section-title h3",
      "Executive overview"
    ),
    
    div(
      class = "dashboard-section-subtitle",
      "A high-level summary of demand, revenue, and product performance."
    ),
    
    layout_column_wrap(
      width = 1 / 4,
      
      value_box(
        title = "Total units sold",
        value = textOutput(ns("total_units")),
        showcase = bsicons::bs_icon("cart-check"),
        theme = "primary"
      ),
      
      value_box(
        title = "Known revenue",
        value = textOutput(ns("total_revenue")),
        showcase = bsicons::bs_icon("currency-dollar"),
        theme = "success"
      ),
      
      value_box(
        title = "Products analyzed",
        value = textOutput(ns("product_count")),
        showcase = bsicons::bs_icon("box-seam"),
        theme = "secondary"
      ),
      
      value_box(
        title = "Date coverage",
        value = textOutput(ns("date_range")),
        showcase = bsicons::bs_icon("calendar-range"),
        theme = "warning"
      )
    ),
    
    layout_columns(
      col_widths = c(8, 4),
      
      card(
        full_screen = TRUE,
        card_header("Demand trend"),
        plotlyOutput(ns("demand_trend"), height = "420px") %>%
          withSpinner(type = 6)
      ),
      
      card(
        card_header("Key findings"),
        uiOutput(ns("key_findings"))
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        full_screen = TRUE,
        card_header("Category contribution"),
        plotlyOutput(ns("category_mix"), height = "340px")
      ),
      
      card(
        full_screen = TRUE,
        card_header("Average demand by weekday"),
        plotlyOutput(ns("weekday_demand"), height = "340px")
      )
    )
  )
}

overview_server <- function(id, daily_sales, product_summary) {
  moduleServer(id, function(input, output, session) {
    
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
    
    output$total_units <- renderText({
      scales::comma(sum(daily_total$units, na.rm = TRUE))
    })
    
    output$total_revenue <- renderText({
      scales::dollar(
        sum(daily_total$revenue, na.rm = TRUE),
        scale_cut = scales::cut_short_scale()
      )
    })
    
    output$product_count <- renderText({
      scales::comma(nrow(product_summary))
    })
    
    output$date_range <- renderText({
      paste(
        format(min(daily_total$date), "%b %Y"),
        "–",
        format(max(daily_total$date), "%b %Y")
      )
    })
    
    output$demand_trend <- renderPlotly({
      p <- ggplot(
        daily_total,
        aes(
          x = date,
          y = units
        )
      ) +
        geom_line(alpha = 0.25) +
        geom_line(
          aes(y = ma30),
          linewidth = 1
        ) +
        scale_y_continuous(
          labels = scales::label_number(
            scale_cut = scales::cut_short_scale()
          )
        ) +
        labs(
          x = NULL,
          y = "Units sold",
          subtitle = "Daily demand with 30-day moving average"
        ) +
        theme_minimal(base_size = 12)
      
      ggplotly(
        p,
        tooltip = c("x", "y")
      ) %>%
        config(displaylogo = FALSE)
    })
    
    output$category_mix <- renderPlotly({
      data <- daily_sales %>%
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
          fill = cat_id,
          text = paste(
            "Category:", cat_id,
            "<br>Total units:", scales::comma(total_units)
          )
        )
      ) +
        geom_col() +
        coord_flip() +
        guides(fill = "none") +
        scale_y_continuous(
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        ) +
        labs(x = NULL, y = "Total units sold") +
        theme_minimal(base_size = 12)
      
      ggplotly(p, tooltip = "text") %>%
        config(displaylogo = FALSE)
    })
    
    output$weekday_demand <- renderPlotly({
      data <- daily_sales %>%
        group_by(weekday, wday) %>%
        summarise(
          average_units = mean(units_sold, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(wday) %>%
        mutate(
          weekday = factor(weekday, levels = weekday)
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
        scale_y_continuous(labels = scales::comma) +
        labs(x = NULL, y = "Average units") +
        theme_minimal(base_size = 12) +
        theme(
          axis.text.x = element_text(angle = 30, hjust = 1)
        )
      
      ggplotly(p) %>%
        config(displaylogo = FALSE)
    })
    
    output$key_findings <- renderUI({
      top_category <- daily_sales %>%
        group_by(cat_id) %>%
        summarise(
          total_units = sum(units_sold, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        slice_max(total_units, n = 1, with_ties = FALSE)
      
      peak_day <- daily_total %>%
        slice_max(units, n = 1, with_ties = FALSE)
      
      tagList(
        div(
          class = "insight-box",
          tags$p(
            tags$strong(top_category$cat_id),
            " generated the highest total unit demand."
          ),
          tags$p(
            "Peak demand occurred on ",
            tags$strong(format(peak_day$date, "%B %d, %Y")),
            " with ",
            tags$strong(scales::comma(peak_day$units)),
            " units sold."
          ),
          tags$p(
            "The moving average indicates recurring variation that should be ",
            "captured through calendar, event, and lag-based forecasting features."
          )
        )
      )
    })
  })
}