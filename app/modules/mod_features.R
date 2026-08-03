############################################################
# Feature Engineering Dashboard Module
#
# File:
# app/modules/mod_features.R
#
# Purpose:
# Present the engineered forecasting features, explain their
# business purpose, and validate that historical predictors
# were created without target leakage.
############################################################


# ==========================================================
# Feature Engineering UI
# ==========================================================

features_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # ------------------------------------------------------
    # Page heading
    # ------------------------------------------------------
    
    div(
      class = "dashboard-section-title h3",
      "Feature engineering"
    ),
    
    div(
      class = "dashboard-section-subtitle",
      paste(
        "Explore the historical demand, calendar, event,",
        "pricing, and seasonality features used for forecasting."
      )
    ),
    
    # ------------------------------------------------------
    # KPI cards
    # ------------------------------------------------------
    
    layout_column_wrap(
      width = 1 / 4,
      
      value_box(
        title = "Category rows",
        value = textOutput(
          ns("category_rows")
        ),
        showcase = bsicons::bs_icon(
          "table"
        ),
        theme = "primary"
      ),
      
      value_box(
        title = "Demand series",
        value = textOutput(
          ns("series_count")
        ),
        showcase = bsicons::bs_icon(
          "diagram-3"
        ),
        theme = "secondary"
      ),
      
      value_box(
        title = "Engineered features",
        value = textOutput(
          ns("feature_count")
        ),
        showcase = bsicons::bs_icon(
          "sliders"
        ),
        theme = "success"
      ),
      
      value_box(
        title = "Maximum lag",
        value = "28 days",
        showcase = bsicons::bs_icon(
          "clock-history"
        ),
        theme = "warning"
      )
    ),
    
    # ------------------------------------------------------
    # Main analytical area
    # ------------------------------------------------------
    
    layout_sidebar(
      
      sidebar = sidebar(
        open = "desktop",
        width = 290,
        
        selectInput(
          ns("category"),
          "Product category",
          choices = NULL
        ),
        
        dateRangeInput(
          ns("dates"),
          "Date range",
          start = NULL,
          end = NULL
        ),
        
        selectInput(
          ns("history_feature"),
          "Historical feature",
          choices = c(
            "1-day lag" = "lag_1",
            "7-day lag" = "lag_7",
            "14-day lag" = "lag_14",
            "28-day lag" = "lag_28",
            "7-day rolling mean" =
              "rolling_mean_7",
            "14-day rolling mean" =
              "rolling_mean_14",
            "28-day rolling mean" =
              "rolling_mean_28"
          ),
          selected = "rolling_mean_7"
        ),
        
        tags$hr(),
        
        p(
          class = "text-muted small",
          paste(
            "All lag and rolling demand variables use only",
            "values from earlier dates. Current-day demand",
            "is not included in its own predictors."
          )
        )
      ),
      
      navset_card_tab(
        
        # --------------------------------------------------
        # Pipeline overview tab
        # --------------------------------------------------
        
        nav_panel(
          "Pipeline overview",
          
          layout_columns(
            col_widths = c(7, 5),
            
            card(
              full_screen = TRUE,
              card_header(
                "Features by group"
              ),
              plotlyOutput(
                ns("feature_group_plot"),
                height = "390px"
              )
            ),
            
            card(
              card_header(
                "Feature pipeline"
              ),
              
              div(
                class = "feature-pipeline",
                
                div(
                  class = "pipeline-step",
                  bsicons::bs_icon("database"),
                  tags$strong("Clean data"),
                  tags$span(
                    "Daily category demand"
                  )
                ),
                
                div(
                  class = "pipeline-arrow",
                  "↓"
                ),
                
                div(
                  class = "pipeline-step",
                  bsicons::bs_icon("calendar-event"),
                  tags$strong("Calendar signals"),
                  tags$span(
                    "Month, weekday, quarter, events"
                  )
                ),
                
                div(
                  class = "pipeline-arrow",
                  "↓"
                ),
                
                div(
                  class = "pipeline-step",
                  bsicons::bs_icon("clock-history"),
                  tags$strong("Demand history"),
                  tags$span(
                    "Lags and rolling statistics"
                  )
                ),
                
                div(
                  class = "pipeline-arrow",
                  "↓"
                ),
                
                div(
                  class = "pipeline-step",
                  bsicons::bs_icon("tag"),
                  tags$strong("Pricing signals"),
                  tags$span(
                    "Price changes and relative price"
                  )
                ),
                
                div(
                  class = "pipeline-arrow",
                  "↓"
                ),
                
                div(
                  class = "pipeline-step",
                  bsicons::bs_icon("cpu"),
                  tags$strong("Model-ready table"),
                  tags$span(
                    "Historical predictors and target"
                  )
                )
              )
            )
          ),
          
          card(
            card_header(
              "Feature dictionary"
            ),
            DTOutput(
              ns("feature_dictionary_table")
            )
          )
        ),
        
        # --------------------------------------------------
        # Historical feature explorer
        # --------------------------------------------------
        
        nav_panel(
          "Historical features",
          
          card(
            full_screen = TRUE,
            card_header(
              "Actual demand versus historical feature"
            ),
            plotlyOutput(
              ns("history_plot"),
              height = "470px"
            )
          ),
          
          uiOutput(
            ns("history_insight")
          )
        ),
        
        # --------------------------------------------------
        # Calendar and event features
        # --------------------------------------------------
        
        nav_panel(
          "Calendar and events",
          
          layout_columns(
            
            card(
              full_screen = TRUE,
              card_header(
                "Average demand by weekday"
              ),
              plotlyOutput(
                ns("weekday_plot"),
                height = "360px"
              )
            ),
            
            card(
              full_screen = TRUE,
              card_header(
                "Event proximity and demand"
              ),
              plotlyOutput(
                ns("event_proximity_plot"),
                height = "360px"
              )
            )
          ),
          
          card(
            card_header(
              "Calendar feature interpretation"
            ),
            uiOutput(
              ns("calendar_insight")
            )
          )
        ),
        
        # --------------------------------------------------
        # Data-quality and leakage checks
        # --------------------------------------------------
        
        nav_panel(
          "Validation",
          
          layout_columns(
            col_widths = c(6, 6),
            
            card(
              full_screen = TRUE,
              card_header(
                "Feature missingness"
              ),
              plotlyOutput(
                ns("missingness_plot"),
                height = "390px"
              )
            ),
            
            card(
              full_screen = TRUE,
              card_header(
                "Actual versus historical estimate"
              ),
              plotlyOutput(
                ns("actual_vs_feature_plot"),
                height = "390px"
              )
            )
          ),
          
          card(
            card_header(
              "Leakage prevention"
            ),
            
            div(
              class = "insight-box",
              
              tags$p(
                tags$strong(
                  "Why this matters: "
                ),
                paste(
                  "A forecasting model must not receive information",
                  "that would be unavailable at prediction time."
                )
              ),
              
              tags$p(
                paste(
                  "The lag variables use earlier dates, while the",
                  "rolling statistics are calculated from demand",
                  "shifted by one day. Therefore, current-day sales",
                  "are not included in their own feature values."
                )
              ),
              
              tags$p(
                paste(
                  "The first 28 observations in each category series",
                  "were removed because they did not contain enough",
                  "history for the longest lag and rolling window."
                )
              )
            )
          )
        ),
        
        # --------------------------------------------------
        # Sample table tab
        # --------------------------------------------------
        
        nav_panel(
          "Feature table",
          
          card(
            card_header(
              "Model-ready category features"
            ),
            DTOutput(
              ns("feature_table")
            )
          )
        )
      )
    )
  )
}


# ==========================================================
# Feature Engineering Server
# ==========================================================

features_server <- function(
    id,
    category_features,
    feature_summary,
    feature_dictionary
) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ----------------------------------------------------
      # Initialize filters
      # ----------------------------------------------------
      
      observe({
        
        categories <- sort(
          unique(category_features$cat_id)
        )
        
        updateSelectInput(
          session,
          "category",
          choices = categories,
          selected = categories[[1]]
        )
        
        updateDateRangeInput(
          session,
          "dates",
          start = min(category_features$date),
          end = max(category_features$date),
          min = min(category_features$date),
          max = max(category_features$date)
        )
      })
      
      
      # ----------------------------------------------------
      # KPI outputs
      # ----------------------------------------------------
      
      output$category_rows <- renderText({
        
        category_row <- feature_summary %>%
          filter(
            dataset == "Category-level"
          )
        
        scales::comma(
          category_row$rows[[1]]
        )
      })
      
      output$series_count <- renderText({
        
        category_row <- feature_summary %>%
          filter(
            dataset == "Category-level"
          )
        
        scales::comma(
          category_row$series[[1]]
        )
      })
      
      output$feature_count <- renderText({
        
        scales::comma(
          nrow(feature_dictionary)
        )
      })
      
      
      # ----------------------------------------------------
      # Filtered feature dataset
      # ----------------------------------------------------
      
      filtered_features <- reactive({
        
        req(
          input$category,
          input$dates
        )
        
        category_features %>%
          filter(
            cat_id == input$category,
            date >= input$dates[1],
            date <= input$dates[2]
          ) %>%
          arrange(date)
      })
      
      
      # ----------------------------------------------------
      # Feature group chart
      # ----------------------------------------------------
      
      output$feature_group_plot <- renderPlotly({
        
        data <- feature_dictionary %>%
          count(
            group,
            name = "feature_count"
          ) %>%
          arrange(feature_count)
        
        p <- ggplot(
          data,
          aes(
            x = reorder(
              group,
              feature_count
            ),
            y = feature_count,
            fill = feature_count
          )
        ) +
          geom_col() +
          coord_flip() +
          guides(fill = "none") +
          scale_y_continuous(
            breaks = scales::breaks_width(1)
          ) +
          labs(
            x = NULL,
            y = "Number of features"
          ) +
          theme_minimal(
            base_size = 12
          ) +
          theme(
            panel.grid.major.y =
              element_blank()
          )
        
        ggplotly(p) %>%
          config(
            displaylogo = FALSE
          )
      })
      
      
      # ----------------------------------------------------
      # Feature dictionary table
      # ----------------------------------------------------
      
      output$feature_dictionary_table <- renderDT({
        
        feature_dictionary %>%
          arrange(
            group,
            feature
          ) %>%
          datatable(
            rownames = FALSE,
            filter = "top",
            options = list(
              pageLength = 10,
              autoWidth = TRUE,
              scrollX = TRUE
            ),
            colnames = c(
              "Feature",
              "Feature group",
              "Description"
            )
          )
      })
      
      
      # ----------------------------------------------------
      # Historical feature plot
      # ----------------------------------------------------
      
      output$history_plot <- renderPlotly({
        
        data <- filtered_features()
        
        selected_feature <- req(
          input$history_feature
        )
        
        validate(
          need(
            selected_feature %in%
              names(data),
            "The selected feature is unavailable."
          )
        )
        
        plot_data <- data %>%
          select(
            date,
            units_sold,
            feature_value =
              all_of(selected_feature)
          ) %>%
          pivot_longer(
            cols = c(
              units_sold,
              feature_value
            ),
            names_to = "series",
            values_to = "value"
          ) %>%
          mutate(
            series = recode(
              series,
              units_sold = "Actual demand",
              feature_value =
                "Historical feature"
            )
          )
        
        p <- ggplot(
          plot_data,
          aes(
            x = date,
            y = value,
            color = series
          )
        ) +
          geom_line(
            linewidth = 0.7
          ) +
          scale_y_continuous(
            labels = scales::comma
          ) +
          labs(
            x = NULL,
            y = "Units sold",
            color = NULL,
            subtitle = paste(
              input$category,
              "—",
              names(
                which(
                  c(
                    "1-day lag" = "lag_1",
                    "7-day lag" = "lag_7",
                    "14-day lag" = "lag_14",
                    "28-day lag" = "lag_28",
                    "7-day rolling mean" =
                      "rolling_mean_7",
                    "14-day rolling mean" =
                      "rolling_mean_14",
                    "28-day rolling mean" =
                      "rolling_mean_28"
                  ) == selected_feature
                )
              )
            )
          ) +
          theme_minimal(
            base_size = 12
          ) +
          theme(
            legend.position = "top"
          )
        
        ggplotly(p) %>%
          config(
            displaylogo = FALSE
          )
      })
      
      
      # ----------------------------------------------------
      # Historical feature insight
      # ----------------------------------------------------
      
      output$history_insight <- renderUI({
        
        data <- filtered_features()
        selected_feature <- req(
          input$history_feature
        )
        
        correlation <- cor(
          data$units_sold,
          data[[selected_feature]],
          use = "complete.obs"
        )
        
        div(
          class = "insight-box mt-3",
          
          tags$p(
            paste0(
              "For ",
              input$category,
              ", the selected historical feature has a correlation of ",
              round(correlation, 3),
              " with actual daily demand during the selected period."
            )
          ),
          
          tags$p(
            paste(
              "This correlation measures association, not final",
              "forecasting performance. The feature will be evaluated",
              "alongside other predictors using time-based validation."
            )
          )
        )
      })
      
      
      # ----------------------------------------------------
      # Weekday plot
      # ----------------------------------------------------
      
      output$weekday_plot <- renderPlotly({
        
        data <- filtered_features() %>%
          group_by(
            weekday_number
          ) %>%
          summarise(
            average_units = mean(
              units_sold,
              na.rm = TRUE
            ),
            .groups = "drop"
          ) %>%
          mutate(
            weekday = factor(
              weekday_number,
              levels = 1:7,
              labels = c(
                "Monday",
                "Tuesday",
                "Wednesday",
                "Thursday",
                "Friday",
                "Saturday",
                "Sunday"
              )
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
          theme_minimal(
            base_size = 12
          ) +
          theme(
            axis.text.x = element_text(
              angle = 30,
              hjust = 1
            )
          )
        
        ggplotly(p) %>%
          config(
            displaylogo = FALSE
          )
      })
      
      
      # ----------------------------------------------------
      # Event proximity plot
      # ----------------------------------------------------
      
      output$event_proximity_plot <- renderPlotly({
        
        data <- filtered_features() %>%
          mutate(
            event_proximity = case_when(
              is_event == 1 ~
                "Event day",
              
              within_3_days_of_event == 1 ~
                "Within 3 days",
              
              within_7_days_of_event == 1 ~
                "Within 4–7 days",
              
              TRUE ~
                "More than 7 days away"
            ),
            
            event_proximity = factor(
              event_proximity,
              levels = c(
                "Event day",
                "Within 3 days",
                "Within 4–7 days",
                "More than 7 days away"
              )
            )
          ) %>%
          group_by(
            event_proximity
          ) %>%
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
            x = event_proximity,
            y = average_units,
            fill = event_proximity
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
          theme_minimal(
            base_size = 12
          ) +
          theme(
            axis.text.x = element_text(
              angle = 25,
              hjust = 1
            )
          )
        
        ggplotly(p) %>%
          config(
            displaylogo = FALSE
          )
      })
      
      
      # ----------------------------------------------------
      # Calendar insight
      # ----------------------------------------------------
      
      output$calendar_insight <- renderUI({
        
        weekday_data <- filtered_features() %>%
          group_by(
            weekday_number
          ) %>%
          summarise(
            average_units = mean(
              units_sold,
              na.rm = TRUE
            ),
            .groups = "drop"
          ) %>%
          slice_max(
            average_units,
            n = 1,
            with_ties = FALSE
          )
        
        weekday_names <- c(
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday",
          "Sunday"
        )
        
        top_weekday <- weekday_names[
          weekday_data$weekday_number
        ]
        
        div(
          class = "insight-box",
          
          tags$p(
            paste0(
              top_weekday,
              " had the highest average demand for ",
              input$category,
              " during the selected period."
            )
          ),
          
          tags$p(
            paste(
              "Weekday, event proximity, month, and cyclical",
              "calendar encodings allow forecasting models to",
              "capture recurring demand patterns."
            )
          )
        )
      })
      
      
      # ----------------------------------------------------
      # Missingness plot
      # ----------------------------------------------------
      
      output$missingness_plot <- renderPlotly({
        
        important_features <- c(
          "lag_1",
          "lag_7",
          "lag_14",
          "lag_28",
          "rolling_mean_7",
          "rolling_mean_14",
          "rolling_mean_28",
          "rolling_sd_7",
          "rolling_sd_28",
          "average_price",
          "price_change_1",
          "rolling_price_mean_28"
        )
        
        available_features <- intersect(
          important_features,
          names(category_features)
        )
        
        data <- category_features %>%
          summarise(
            across(
              all_of(
                available_features
              ),
              ~ mean(is.na(.x)) * 100
            )
          ) %>%
          pivot_longer(
            everything(),
            names_to = "feature",
            values_to = "missing_pct"
          ) %>%
          arrange(missing_pct)
        
        p <- ggplot(
          data,
          aes(
            x = reorder(
              feature,
              missing_pct
            ),
            y = missing_pct,
            fill = missing_pct
          )
        ) +
          geom_col() +
          coord_flip() +
          guides(fill = "none") +
          scale_y_continuous(
            labels = function(x) {
              paste0(x, "%")
            }
          ) +
          labs(
            x = NULL,
            y = "Missing values"
          ) +
          theme_minimal(
            base_size = 12
          )
        
        ggplotly(p) %>%
          config(
            displaylogo = FALSE
          )
      })
      
      
      # ----------------------------------------------------
      # Actual versus feature scatterplot
      # ----------------------------------------------------
      
      output$actual_vs_feature_plot <- renderPlotly({
        
        data <- filtered_features()
        
        selected_feature <- req(
          input$history_feature
        )
        
        plot_data <- data %>%
          filter(
            !is.na(
              .data[[selected_feature]]
            )
          )
        
        p <- ggplot(
          plot_data,
          aes(
            x = .data[[selected_feature]],
            y = units_sold
          )
        ) +
          geom_point(
            alpha = 0.25
          ) +
          geom_smooth(
            method = "lm",
            se = FALSE
          ) +
          scale_x_continuous(
            labels = scales::comma
          ) +
          scale_y_continuous(
            labels = scales::comma
          ) +
          labs(
            x = "Historical feature value",
            y = "Actual units sold"
          ) +
          theme_minimal(
            base_size = 12
          )
        
        ggplotly(p) %>%
          config(
            displaylogo = FALSE
          )
      })
      
      
      # ----------------------------------------------------
      # Feature table
      # ----------------------------------------------------
      
      output$feature_table <- renderDT({
        
        selected_columns <- c(
          "date",
          "cat_id",
          "units_sold",
          "lag_1",
          "lag_7",
          "lag_28",
          "rolling_mean_7",
          "rolling_mean_28",
          "rolling_sd_28",
          "calendar_month",
          "weekday_number",
          "is_event",
          "snap_ca",
          "average_price",
          "price_change_1"
        )
        
        selected_columns <- intersect(
          selected_columns,
          names(filtered_features())
        )
        
        filtered_features() %>%
          select(
            all_of(
              selected_columns
            )
          ) %>%
          datatable(
            rownames = FALSE,
            filter = "top",
            options = list(
              pageLength = 15,
              scrollX = TRUE,
              autoWidth = TRUE
            )
          ) %>%
          formatRound(
            columns = intersect(
              c(
                "rolling_mean_7",
                "rolling_mean_28",
                "rolling_sd_28",
                "average_price",
                "price_change_1"
              ),
              selected_columns
            ),
            digits = 2
          )
      })
    }
  )
}