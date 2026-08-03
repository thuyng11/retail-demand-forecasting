############################################################
# Forecasting Dashboard Module
#
# File:
# app/modules/mod_forecasting.R
#
# Purpose:
# Present category-level forecast results, compare model
# performance, examine residuals, and communicate business
# recommendations.
############################################################


# ==========================================================
# Forecasting UI
# ==========================================================

forecasting_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # ------------------------------------------------------
    # Page heading
    # ------------------------------------------------------
    
    div(
      class = "dashboard-section-title h3",
      "Demand forecasting"
    ),
    
    div(
      class = "dashboard-section-subtitle",
      paste(
        "Compare Seasonal Naive, ETS, and XGBoost forecasts",
        "using a fixed 28-day chronological holdout period."
      )
    ),
    
    # ------------------------------------------------------
    # KPI cards
    # ------------------------------------------------------
    
    layout_column_wrap(
      width = 1 / 4,
      
      value_box(
        title = "Best overall model",
        value = textOutput(
          ns("best_overall_model")
        ),
        showcase = bsicons::bs_icon(
          "trophy"
        ),
        theme = "primary"
      ),
      
      value_box(
        title = "Best overall WAPE",
        value = textOutput(
          ns("best_overall_wape")
        ),
        showcase = bsicons::bs_icon(
          "bullseye"
        ),
        theme = "success"
      ),
      
      value_box(
        title = "Forecast horizon",
        value = textOutput(
          ns("forecast_horizon")
        ),
        showcase = bsicons::bs_icon(
          "calendar4-week"
        ),
        theme = "secondary"
      ),
      
      value_box(
        title = "Categories modeled",
        value = textOutput(
          ns("category_count")
        ),
        showcase = bsicons::bs_icon(
          "collection"
        ),
        theme = "warning"
      )
    ),
    
    # ------------------------------------------------------
    # Main workspace
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
        
        checkboxGroupInput(
          ns("models"),
          "Models displayed",
          choices = NULL,
          selected = NULL
        ),
        
        selectInput(
          ns("metric"),
          "Comparison metric",
          choices = c(
            "WAPE" = "wape",
            "MAE" = "mae",
            "RMSE" = "rmse",
            "Forecast bias" = "forecast_bias"
          ),
          selected = "wape"
        ),
        
        tags$hr(),
        
        p(
          class = "text-muted small",
          paste(
            "Models were trained on observations before",
            "April 25, 2016 and evaluated on the final",
            "28 days of historical demand."
          )
        )
      ),
      
      navset_card_tab(
        
        # --------------------------------------------------
        # Forecast performance tab
        # --------------------------------------------------
        
        nav_panel(
          "Forecast performance",
          
          card(
            full_screen = TRUE,
            card_header(
              "Actual versus predicted demand"
            ),
            plotlyOutput(
              ns("forecast_plot"),
              height = "470px"
            ) %>%
              shinycssloaders::withSpinner(
                type = 6
              )
          ),
          
          uiOutput(
            ns("forecast_insight")
          )
        ),
        
        # --------------------------------------------------
        # Model comparison tab
        # --------------------------------------------------
        
        nav_panel(
          "Model comparison",
          
          layout_columns(
            col_widths = c(7, 5),
            
            card(
              full_screen = TRUE,
              card_header(
                "Performance by model"
              ),
              plotlyOutput(
                ns("metric_plot"),
                height = "400px"
              )
            ),
            
            card(
              card_header(
                "Recommended model"
              ),
              uiOutput(
                ns("model_recommendation")
              )
            )
          ),
          
          card(
            card_header(
              "Model metrics"
            ),
            DTOutput(
              ns("metrics_table")
            )
          )
        ),
        
        # --------------------------------------------------
        # Residual diagnostics tab
        # --------------------------------------------------
        
        nav_panel(
          "Residual diagnostics",
          
          layout_columns(
            
            card(
              full_screen = TRUE,
              card_header(
                "Residuals over time"
              ),
              plotlyOutput(
                ns("residual_time_plot"),
                height = "370px"
              )
            ),
            
            card(
              full_screen = TRUE,
              card_header(
                "Residual distribution"
              ),
              plotlyOutput(
                ns("residual_boxplot"),
                height = "370px"
              )
            )
          ),
          
          card(
            card_header(
              "Residual interpretation"
            ),
            uiOutput(
              ns("residual_insight")
            )
          )
        ),
        
        # --------------------------------------------------
        # Feature importance tab
        # --------------------------------------------------
        
        nav_panel(
          "Feature importance",
          
          layout_columns(
            col_widths = c(8, 4),
            
            card(
              full_screen = TRUE,
              card_header(
                "Top XGBoost predictors"
              ),
              plotlyOutput(
                ns("importance_plot"),
                height = "470px"
              )
            ),
            
            card(
              card_header(
                "How to interpret importance"
              ),
              
              div(
                class = "insight-box",
                
                tags$p(
                  tags$strong("Gain"),
                  paste(
                    "measures how much each predictor improved",
                    "the XGBoost model when it was used in tree",
                    "splits."
                  )
                ),
                
                tags$p(
                  paste(
                    "A high importance score does not prove",
                    "causality. It indicates that the predictor",
                    "was useful for reducing forecast error."
                  )
                ),
                
                tags$p(
                  paste(
                    "The feature-importance chart applies only",
                    "to XGBoost, not ETS or Seasonal Naive."
                  )
                )
              )
            )
          )
        ),
        
        # --------------------------------------------------
        # Forecast table tab
        # --------------------------------------------------
        
        nav_panel(
          "Forecast table",
          
          card(
            card_header(
              "Daily holdout forecasts"
            ),
            DTOutput(
              ns("forecast_table")
            )
          )
        ),
        
        # --------------------------------------------------
        # Methodology tab
        # --------------------------------------------------
        
        nav_panel(
          "Evaluation design",
          
          layout_columns(
            col_widths = c(7, 5),
            
            card(
              card_header(
                "Forecast evaluation workflow"
              ),
              
              div(
                class = "forecast-pipeline",
                
                div(
                  class = "pipeline-step",
                  bsicons::bs_icon("database"),
                  tags$strong("Historical data"),
                  tags$span(
                    "Daily category-level demand"
                  )
                ),
                
                div(
                  class = "pipeline-arrow",
                  "↓"
                ),
                
                div(
                  class = "pipeline-step",
                  bsicons::bs_icon("scissors"),
                  tags$strong("Time-based split"),
                  tags$span(
                    "Final 28 days reserved for testing"
                  )
                ),
                
                div(
                  class = "pipeline-arrow",
                  "↓"
                ),
                
                div(
                  class = "pipeline-step",
                  bsicons::bs_icon("cpu"),
                  tags$strong("Three models"),
                  tags$span(
                    "Seasonal Naive, ETS, and XGBoost"
                  )
                ),
                
                div(
                  class = "pipeline-arrow",
                  "↓"
                ),
                
                div(
                  class = "pipeline-step",
                  bsicons::bs_icon("bar-chart"),
                  tags$strong("Evaluation"),
                  tags$span(
                    "MAE, RMSE, WAPE, and forecast bias"
                  )
                ),
                
                div(
                  class = "pipeline-arrow",
                  "↓"
                ),
                
                div(
                  class = "pipeline-step",
                  bsicons::bs_icon("check-circle"),
                  tags$strong("Champion selection"),
                  tags$span(
                    "Best model selected separately by category"
                  )
                )
              )
            ),
            
            card(
              card_header(
                "Model definitions"
              ),
              
              tags$dl(
                
                tags$dt(
                  "Seasonal Naive"
                ),
                tags$dd(
                  paste(
                    "Predicts demand using the corresponding",
                    "day from the previous weekly cycle."
                  )
                ),
                
                tags$dt(
                  "ETS"
                ),
                tags$dd(
                  paste(
                    "Models demand level, trend, seasonality,",
                    "and forecast error through exponential",
                    "smoothing."
                  )
                ),
                
                tags$dt(
                  "XGBoost"
                ),
                tags$dd(
                  paste(
                    "Uses boosted decision trees and engineered",
                    "calendar, event, price, and historical",
                    "demand features."
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}


# ==========================================================
# Forecasting Server
# ==========================================================

forecasting_server <- function(
    id,
    category_forecasts,
    category_model_metrics,
    category_model_rankings,
    category_residuals,
    xgboost_feature_importance,
    forecasting_metadata
) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ----------------------------------------------------
      # Initialize filter inputs
      # ----------------------------------------------------
      
      observe({
        
        categories <- sort(
          unique(
            category_forecasts$cat_id
          )
        )
        
        models <- sort(
          unique(
            category_forecasts$model
          )
        )
        
        updateSelectInput(
          session,
          "category",
          choices = categories,
          selected = categories[[1]]
        )
        
        updateCheckboxGroupInput(
          session,
          "models",
          choices = models,
          selected = models
        )
      })
      
      
      # ----------------------------------------------------
      # KPI cards
      # ----------------------------------------------------
      
      overall_metrics <- reactive({
        
        category_model_metrics %>%
          filter(
            cat_id == "ALL"
          ) %>%
          arrange(
            wape,
            rmse,
            mae
          )
      })
      
      output$best_overall_model <- renderText({
        
        data <- overall_metrics()
        
        validate(
          need(
            nrow(data) > 0,
            "No overall metrics are available."
          )
        )
        
        data$model[[1]]
      })
      
      output$best_overall_wape <- renderText({
        
        data <- overall_metrics()
        
        validate(
          need(
            nrow(data) > 0,
            "No overall metrics are available."
          )
        )
        
        paste0(
          round(
            data$wape[[1]],
            2
          ),
          "%"
        )
      })
      
      output$forecast_horizon <- renderText({
        
        if (
          "forecast_horizon_days" %in%
          names(forecasting_metadata)
        ) {
          paste0(
            forecasting_metadata$
              forecast_horizon_days[[1]],
            " days"
          )
        } else {
          "28 days"
        }
      })
      
      output$category_count <- renderText({
        
        scales::comma(
          n_distinct(
            category_forecasts$cat_id
          )
        )
      })
      
      
      # ----------------------------------------------------
      # Filtered forecasts
      # ----------------------------------------------------
      
      filtered_forecasts <- reactive({
        
        req(
          input$category,
          input$models
        )
        
        category_forecasts %>%
          filter(
            cat_id == input$category,
            model %in% input$models
          ) %>%
          arrange(
            date,
            model
          )
      })
      
      
      # ----------------------------------------------------
      # Filtered metrics
      # ----------------------------------------------------
      
      filtered_metrics <- reactive({
        
        req(input$category)
        
        category_model_metrics %>%
          filter(
            cat_id == input$category
          ) %>%
          arrange(
            wape,
            rmse,
            mae
          )
      })
      
      
      # ----------------------------------------------------
      # Actual versus forecast plot
      # ----------------------------------------------------
      
      output$forecast_plot <- renderPlotly({
        
        data <- filtered_forecasts()
        
        validate(
          need(
            nrow(data) > 0,
            "Select at least one model."
          )
        )
        
        actual_data <- data %>%
          distinct(
            date,
            actual
          )
        
        p <- ggplot() +
          
          geom_line(
            data = actual_data,
            aes(
              x = date,
              y = actual
            ),
            linewidth = 1.1,
            color = "#111827"
          ) +
          
          geom_line(
            data = data,
            aes(
              x = date,
              y = predicted,
              color = model
            ),
            linewidth = 0.9
          ) +
          
          geom_point(
            data = data,
            aes(
              x = date,
              y = predicted,
              color = model
            ),
            size = 1.4,
            alpha = 0.7
          ) +
          
          scale_y_continuous(
            labels = scales::comma
          ) +
          
          labs(
            x = NULL,
            y = "Units sold",
            color = "Model",
            subtitle = paste(
              input$category,
              "— 28-day holdout period"
            ),
            caption = paste(
              "Black line represents actual demand."
            )
          ) +
          
          theme_minimal(
            base_size = 12
          ) +
          
          theme(
            legend.position = "top"
          )
        
        ggplotly(
          p,
          tooltip = c(
            "x",
            "y",
            "colour"
          )
        ) %>%
          config(
            displaylogo = FALSE
          )
      })
      
      
      # ----------------------------------------------------
      # Forecast insight
      # ----------------------------------------------------
      
      output$forecast_insight <- renderUI({
        
        metrics <- filtered_metrics()
        
        validate(
          need(
            nrow(metrics) > 0,
            "No model metrics are available."
          )
        )
        
        best <- metrics %>%
          slice_head(n = 1)
        
        baseline <- metrics %>%
          filter(
            model == "Seasonal Naive"
          )
        
        improvement_text <- NULL
        
        if (
          nrow(baseline) == 1 &&
          baseline$wape[[1]] != 0
        ) {
          improvement <- (
            baseline$wape[[1]] -
              best$wape[[1]]
          ) /
            baseline$wape[[1]] *
            100
          
          improvement_text <- tags$p(
            paste0(
              "Compared with Seasonal Naive, the selected model reduced WAPE by ",
              round(improvement, 1),
              "%."
            )
          )
        }
        
        div(
          class = "insight-box mt-3",
          
          tags$p(
            tags$strong(
              best$model[[1]]
            ),
            paste0(
              " produced the lowest WAPE for ",
              input$category,
              " at ",
              round(
                best$wape[[1]],
                2
              ),
              "%."
            )
          ),
          
          tags$p(
            paste0(
              "Its average absolute error was ",
              scales::comma(
                round(
                  best$mae[[1]],
                  1
                )
              ),
              " units per day, while its forecast bias was ",
              round(
                best$forecast_bias[[1]],
                2
              ),
              "%."
            )
          ),
          
          improvement_text
        )
      })
      
      
      # ----------------------------------------------------
      # Metric comparison plot
      # ----------------------------------------------------
      
      output$metric_plot <- renderPlotly({
        
        data <- filtered_metrics()
        
        selected_metric <- req(
          input$metric
        )
        
        validate(
          need(
            selected_metric %in%
              names(data),
            "Selected metric is unavailable."
          )
        )
        
        metric_label <- switch(
          selected_metric,
          wape = "WAPE (%)",
          mae = "MAE",
          rmse = "RMSE",
          forecast_bias = "Forecast bias (%)"
        )
        
        plot_data <- data %>%
          mutate(
            metric_value =
              .data[[selected_metric]]
          )
        
        p <- ggplot(
          plot_data,
          aes(
            x = reorder(
              model,
              metric_value
            ),
            y = metric_value,
            fill = model
          )
        ) +
          geom_col() +
          guides(fill = "none") +
          coord_flip() +
          labs(
            x = NULL,
            y = metric_label
          ) +
          theme_minimal(
            base_size = 12
          ) +
          theme(
            panel.grid.major.y =
              element_blank()
          )
        
        if (
          selected_metric ==
          "forecast_bias"
        ) {
          p <- p +
            geom_hline(
              yintercept = 0,
              linetype = "dashed"
            )
        }
        
        ggplotly(p) %>%
          config(
            displaylogo = FALSE
          )
      })
      
      
      # ----------------------------------------------------
      # Model recommendation
      # ----------------------------------------------------
      
      output$model_recommendation <- renderUI({
        
        ranking <- category_model_rankings %>%
          filter(
            cat_id == input$category
          ) %>%
          arrange(
            model_rank
          )
        
        validate(
          need(
            nrow(ranking) > 0,
            "No ranking is available."
          )
        )
        
        best <- ranking %>%
          slice_head(n = 1)
        
        bias_direction <- case_when(
          best$forecast_bias[[1]] >
            0 ~ "overforecast",
          best$forecast_bias[[1]] <
            0 ~ "underforecast",
          TRUE ~ "remain unbiased"
        )
        
        tagList(
          
          div(
            class = "forecast-model-badge",
            tags$span(
              "Recommended"
            ),
            tags$strong(
              best$model[[1]]
            )
          ),
          
          tags$p(
            paste0(
              best$model[[1]],
              " is recommended for ",
              input$category,
              " because it ranked first by WAPE."
            )
          ),
          
          tags$p(
            paste0(
              "WAPE: ",
              round(
                best$wape[[1]],
                2
              ),
              "%"
            )
          ),
          
          tags$p(
            paste0(
              "RMSE: ",
              scales::comma(
                round(
                  best$rmse[[1]],
                  1
                )
              )
            )
          ),
          
          tags$p(
            paste0(
              "The model tended to ",
              bias_direction,
              " demand, with a bias of ",
              round(
                best$forecast_bias[[1]],
                2
              ),
              "%."
            )
          )
        )
      })
      
      
      # ----------------------------------------------------
      # Metrics table
      # ----------------------------------------------------
      
      output$metrics_table <- renderDT({
        
        filtered_metrics() %>%
          select(
            model,
            observations,
            mae,
            rmse,
            wape,
            forecast_bias,
            mean_actual,
            mean_predicted
          ) %>%
          mutate(
            wape = paste0(
              round(wape, 2),
              "%"
            ),
            
            forecast_bias = paste0(
              round(
                forecast_bias,
                2
              ),
              "%"
            ),
            
            across(
              c(
                mae,
                rmse,
                mean_actual,
                mean_predicted
              ),
              ~ round(.x, 1)
            )
          ) %>%
          datatable(
            rownames = FALSE,
            options = list(
              dom = "t",
              pageLength = 5,
              autoWidth = TRUE,
              scrollX = TRUE
            ),
            colnames = c(
              "Model",
              "Observations",
              "MAE",
              "RMSE",
              "WAPE",
              "Forecast bias",
              "Mean actual",
              "Mean predicted"
            )
          )
      })
      
      
      # ----------------------------------------------------
      # Residual time-series plot
      # ----------------------------------------------------
      
      output$residual_time_plot <- renderPlotly({
        
        data <- category_residuals %>%
          filter(
            cat_id == input$category,
            model %in% input$models
          )
        
        validate(
          need(
            nrow(data) > 0,
            "Select at least one model."
          )
        )
        
        p <- ggplot(
          data,
          aes(
            x = date,
            y = residual,
            color = model
          )
        ) +
          geom_hline(
            yintercept = 0,
            linetype = "dashed"
          ) +
          geom_line(
            linewidth = 0.8
          ) +
          scale_y_continuous(
            labels = scales::comma
          ) +
          labs(
            x = NULL,
            y = "Residual: actual − predicted",
            color = "Model"
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
      # Residual boxplot
      # ----------------------------------------------------
      
      output$residual_boxplot <- renderPlotly({
        
        data <- category_residuals %>%
          filter(
            cat_id == input$category,
            model %in% input$models
          )
        
        validate(
          need(
            nrow(data) > 0,
            "Select at least one model."
          )
        )
        
        p <- ggplot(
          data,
          aes(
            x = model,
            y = residual,
            fill = model
          )
        ) +
          geom_hline(
            yintercept = 0,
            linetype = "dashed"
          ) +
          geom_boxplot(
            alpha = 0.8
          ) +
          guides(fill = "none") +
          labs(
            x = NULL,
            y = "Residual: actual − predicted"
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
      # Residual insight
      # ----------------------------------------------------
      
      output$residual_insight <- renderUI({
        
        data <- category_residuals %>%
          filter(
            cat_id == input$category
          ) %>%
          group_by(model) %>%
          summarise(
            mean_residual = mean(
              residual,
              na.rm = TRUE
            ),
            
            largest_absolute_error = max(
              absolute_error,
              na.rm = TRUE
            ),
            
            .groups = "drop"
          ) %>%
          arrange(
            abs(mean_residual)
          )
        
        best_centered <- data %>%
          slice_head(n = 1)
        
        div(
          class = "insight-box",
          
          tags$p(
            paste0(
              best_centered$model[[1]],
              " had the residual mean closest to zero for ",
              input$category,
              ", at ",
              round(
                best_centered$mean_residual[[1]],
                1
              ),
              " units."
            )
          ),
          
          tags$p(
            paste(
              "Positive residuals indicate underforecasting;",
              "negative residuals indicate overforecasting."
            )
          ),
          
          tags$p(
            paste(
              "A strong model should have residuals centered",
              "near zero without a persistent pattern over time."
            )
          )
        )
      })
      
      
      # ----------------------------------------------------
      # XGBoost feature importance
      # ----------------------------------------------------
      
      output$importance_plot <- renderPlotly({
        
        validate(
          need(
            nrow(
              xgboost_feature_importance
            ) > 0,
            "No XGBoost importance data is available."
          )
        )
        
        data <- xgboost_feature_importance %>%
          slice_max(
            gain,
            n = 15,
            with_ties = FALSE
          ) %>%
          arrange(gain)
        
        p <- ggplot(
          data,
          aes(
            x = reorder(
              feature,
              gain
            ),
            y = gain,
            fill = gain
          )
        ) +
          geom_col() +
          coord_flip() +
          guides(fill = "none") +
          scale_y_continuous(
            labels = scales::percent
          ) +
          labs(
            x = NULL,
            y = "Relative gain"
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
      # Forecast table
      # ----------------------------------------------------
      
      output$forecast_table <- renderDT({
        
        filtered_forecasts() %>%
          select(
            date,
            cat_id,
            model,
            actual,
            predicted,
            residual,
            absolute_error,
            absolute_percentage_error,
            model_rank,
            is_best_model
          ) %>%
          mutate(
            predicted = round(
              predicted,
              1
            ),
            
            residual = round(
              residual,
              1
            ),
            
            absolute_error = round(
              absolute_error,
              1
            ),
            
            absolute_percentage_error =
              round(
                absolute_percentage_error,
                2
              )
          ) %>%
          datatable(
            rownames = FALSE,
            filter = "top",
            options = list(
              pageLength = 15,
              scrollX = TRUE,
              autoWidth = TRUE
            ),
            colnames = c(
              "Date",
              "Category",
              "Model",
              "Actual",
              "Predicted",
              "Residual",
              "Absolute error",
              "Absolute percentage error",
              "Model rank",
              "Best model"
            )
          )
      })
    }
  )
}