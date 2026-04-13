# ============================================================================
# IMPORT LIBRARIES AND LOAD DATA
# ============================================================================

library(shiny)
library(dplyr)
library(randomForest)

# adjust as needed
setwd("~/OneDrive - Nanyang Technological University/Y2S2/Analytics II/Project/dataset2/src")

# load model and data with error handling
tryCatch({
  rf_best <- readRDS("../data/rf_best.rds")
  df.train <- readRDS("../data/df_train.rds")
}, error = function(e) {
  stop("Failed to load model or training data. Please ensure rf_best.rds and df_train.rds exist in ../data/")
})

# compute defaults from training data
price_median <- round(median(df.train$total_price, na.rm = TRUE))
sales_median <- round(median(df.train$units_sold))

# get unique store and sku ids
store_ids <- levels(df.train$store_id)
sku_ids   <- levels(df.train$sku_id)

# compute next week date range at startup
today <- Sys.Date()
days_until_monday <- (8 - as.integer(format(today, "%u"))) %% 7
if (days_until_monday == 0) days_until_monday <- 7
week_start <- today + days_until_monday
week_end <- week_start + 6
week_disp <- paste0(format(week_start, "%d %b"), " to ", format(week_end, "%d %b %Y"))
week_month <- as.numeric(format(week_start, "%m"))
week_day <- as.numeric(format(week_start, "%d"))

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f8f9fa; font-family: 'Segoe UI', sans-serif; }
      .title-panel { background-color: #C41E3A; color: white; padding: 20px 30px; margin: 0px -15px 20px -15px; }
      .title-panel h2 { margin: 0; font-size: 22px; font-weight: 600; }
      .title-panel p { margin: 4px 0 0; font-size: 13px; opacity: 0.7; }
      .sidebar-panel { background: white; border-radius: 8px; padding: 20px; box-shadow: 0 1px 4px rgba(0,0,0,0.08); }
      .main-panel { padding-left: 20px; }
      .result-card { background: white; border-radius: 8px; padding: 20px; text-align: center; box-shadow: 0 1px 4px rgba(0,0,0,0.08); height: 140px; display: flex; flex-direction: column; justify-content: center; }
      .result-label { font-size: 12px; text-transform: uppercase; letter-spacing: 1px; color: #888; margin-bottom: 8px; }
      .result-value { font-size: 32px; font-weight: 700; margin: 0; }
      .result-sub { font-size: 11px; color: #aaa; margin-top: 4px; }
      .section-title { font-size: 11px; text-transform: uppercase; letter-spacing: 1px; color: #888; margin: 0 0 8px; font-weight: 600; }
      .interp-box { background: white; border-radius: 8px; padding: 16px 20px; box-shadow: 0 1px 4px rgba(0,0,0,0.08); font-size: 14px; line-height: 1.6; color: #444; min-height: 50px; }
      .badge-box { border-radius: 6px; padding: 10px 16px; font-size: 14px; font-weight: 600; display: inline-block; }
      .date-banner { background: white; border-radius: 8px; padding: 12px 20px; box-shadow: 0 1px 4px rgba(0,0,0,0.08); margin-bottom: 16px; display: flex; align-items: center; gap: 10px; }
      .date-label { font-size: 12px; text-transform: uppercase; letter-spacing: 1px; color: #888; font-weight: 600; }
      .date-value { font-size: 14px; font-weight: 700; color: #2c3e50; }
      .date-sub { font-size: 12px; color: #aaa; }
      hr { border: none; border-top: 1px solid #eee; margin: 16px 0; }
      .btn-primary { background-color: #C41E3A !important; border-color: #C41E3A !important; color: white !important; border-radius: 6px !important; font-weight: 600 !important; padding: 10px !important; }
      .btn-primary:hover { background-color: #DE3163 !important; border-color: #DE3163 !important; }
      label { font-size: 13px; color: #555; font-weight: 500; }
      .form-control, .selectize-input { border-radius: 6px !important; border: 1px solid #ddd !important; font-size: 13px !important; }
    "))
  ),
  
  div(class = "title-panel",
      h2("H&M Weekly Promotion Impact Predictor"),
      p("Forecast next week's demand for any store-SKU combination to support inventory and promotion decisions.")
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      width = 3,
      
      p(class = "section-title", "Store & Product"),
      selectInput("store_id", "Store ID", choices = store_ids),
      selectInput("sku_id", "SKU ID", choices = sku_ids),
      
      hr(),
      
      p(class = "section-title", "Promotion Plan"),
      selectInput("is_featured", "Is Featured SKU",
                  choices = c("No" = "0", "Yes" = "1")),
      selectInput("is_display",  "Is Display SKU",
                  choices = c("No" = "0", "Yes" = "1")),
      
      hr(),
      
      p(class = "section-title", "Pricing"),
      numericInput("total_price", "Total Price ($)", value = price_median),
      numericInput("base_price", "Base Price ($)", value = price_median),
      
      hr(),
      
      p(class = "section-title", "Recent Sales History"),
      numericInput("lag_1", "This Week's Completed Sales", value = sales_median, min = 0),
      numericInput("lag_2", "Last Week's Sales", value = sales_median, min = 0),
      numericInput("lag_3", "2 Weeks Ago Sales", value = sales_median, min = 0),
      numericInput("lag_4", "3 Weeks Ago Sales", value = sales_median, min = 0),
      
      hr(),
      
      actionButton("predict", "Predict Demand", class = "btn-primary", width = "100%")
    ),
    
    mainPanel(
      class = "main-panel",
      width = 9,
      
      # date banner
      div(class = "date-banner",
          span(class = "date-label", "Predicting for week of"),
          span(class = "date-value", week_disp),
          span(class = "date-sub",  "(next week)")
      ),
      
      p(class = "section-title", "Prediction Results"),
      
      fluidRow(
        column(4,
               div(class = "result-card",
                   div(class = "result-label", "With Promotion"),
                   div(class = "result-value", style = "color: #27ae60;",
                       textOutput("pred_with")),
                   div(class = "result-sub", "units predicted")
               )
        ),
        column(4,
               div(class = "result-card",
                   div(class = "result-label", "Without Promotion"),
                   div(class = "result-value", style = "color: #e74c3c;",
                       textOutput("pred_without")),
                   div(class = "result-sub", "units predicted")
               )
        ),
        column(4,
               div(class = "result-card",
                   div(class = "result-label", "Sales Lift"),
                   div(class = "result-value", style = "color: #2980b9;",
                       textOutput("lift")),
                   div(class = "result-sub", "vs no promotion")
               )
        )
      ),
      
      br(),
      
      p(class = "section-title", "Recommendation"),
      uiOutput("recommendation"),
      
      br(),
      
      p(class = "section-title", "Interpretation"),
      div(class = "interp-box", textOutput("interpretation"))
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output) {
  
  # initialize outputs on load
  output$pred_with <- renderText({ "\u2014" })
  output$pred_without <- renderText({ "\u2014" })
  output$lift <- renderText({ "\u2014" })
  output$recommendation <- renderUI({
    div(class = "badge-box",
        style = "background-color: #f0f0f0; color: #888;",
        "Awaiting prediction")
  })
  output$interpretation <- renderText({
    "Select your inputs and click Predict Demand to see results."
  })
  
  # helper to build input dataframe
  build_input <- function(is_featured, is_display) {
    
    # all 4 lag values for proper rolling window calculation
    lag_vals <- c(input$lag_1, input$lag_2, input$lag_3, input$lag_4)
    
    # store-level aggregates from most recent 4 weeks
    store_recent <- df.train %>%
      filter(store_id == input$store_id) %>%
      group_by(date) %>%
      summarise(total_units = sum(units_sold), .groups = "drop") %>%
      arrange(desc(date)) %>%
      slice(1:5)
    
    store_agg <- store_recent %>%
      summarise(
        store_total_lag_1 = total_units[1],
        store_total_roll_mean_4 = mean(total_units[2:5]),
        store_total_roll_sd_4 = ifelse(
          is.na(sd(total_units[2:5])), 0, sd(total_units[2:5])
        )
      )
    
    # sku-level aggregates from most recent 4 weeks
    sku_recent <- df.train %>%
      filter(sku_id == input$sku_id) %>%
      group_by(date) %>%
      summarise(total_units = sum(units_sold), .groups = "drop") %>%
      arrange(desc(date)) %>%
      slice(1:5)
    
    sku_agg <- sku_recent %>%
      summarise(
        sku_total_lag_1 = total_units[1],
        sku_total_roll_mean_4 = mean(total_units[2:5]),
        sku_total_roll_sd_4 = ifelse(
          is.na(sd(total_units[2:5])), 0, sd(total_units[2:5])
        )
      )
    
    data.frame(
      total_price = input$total_price,
      base_price = input$base_price,
      is_featured_sku = factor(is_featured, levels = c("0", "1")),
      is_display_sku = factor(is_display,  levels = c("0", "1")),
      lag_1 = input$lag_1,
      lag_2 = input$lag_2,
      lag_4 = input$lag_4,
      roll_mean_4 = mean(lag_vals),
      roll_sd_4 = ifelse(is.na(sd(lag_vals)), 0, sd(lag_vals)),
      store_total_lag_1 = store_agg$store_total_lag_1,
      store_total_roll_mean_4 = store_agg$store_total_roll_mean_4,
      store_total_roll_sd_4 = store_agg$store_total_roll_sd_4,
      sku_total_lag_1 = sku_agg$sku_total_lag_1,
      sku_total_roll_mean_4 = sku_agg$sku_total_roll_mean_4,
      sku_total_roll_sd_4 = sku_agg$sku_total_roll_sd_4,
      month = week_month,
      day = week_day
    )
  }
  
  observeEvent(input$predict, {
    
    # input validation
    if (is.na(input$total_price) || input$total_price <= 0) {
      showNotification("Please enter a valid total price.", type = "error")
      return()
    }
    if (is.na(input$base_price) || input$base_price <= 0) {
      showNotification("Please enter a valid base price.", type = "error")
      return()
    }
    if (is.na(input$lag_1) || input$lag_1 < 0) {
      showNotification("Please enter a valid value for This Week's Completed Sales.", type = "error")
      return()
    }
    if (is.na(input$lag_2) || input$lag_2 < 0) {
      showNotification("Please enter a valid value for Last Week's Sales.", type = "error")
      return()
    }
    if (is.na(input$lag_3) || input$lag_3 < 0) {
      showNotification("Please enter a valid value for 2 Weeks Ago Sales.", type = "error")
      return()
    }
    if (is.na(input$lag_4) || input$lag_4 < 0) {
      showNotification("Please enter a valid value for 3 Weeks Ago Sales.", type = "error")
      return()
    }
    
    # predict with error handling
    tryCatch({
      
      # predict with selected promotion
      input_with <- build_input(input$is_featured, input$is_display)
      pred_with <- round(as.numeric(predict(rf_best, newdata = input_with)))
      
      # predict without any promotion
      input_without <- build_input("0", "0")
      pred_without <- round(as.numeric(predict(rf_best, newdata = input_without)))
      
      # sales lift
      lift <- pred_with - pred_without
      lift_pct <- ifelse(pred_without == 0, 0,
                         round((lift / pred_without) * 100, 1))
      
      # no promotion selected
      no_promo <- input$is_featured == "0" & input$is_display == "0"
      
      output$pred_with <- renderText({ pred_with })
      output$pred_without <- renderText({ pred_without })
      output$lift <- renderText({
        if (no_promo) "\u2014"
        else if (lift >= 0) paste0("+", lift, " (", lift_pct, "%)")
        else paste0(lift, " (", lift_pct, "%)")
      })
      
      # recommendation badge
      output$recommendation <- renderUI({
        if (no_promo) {
          div(class = "badge-box",
              style = "background-color: #f0f0f0; color: #555;",
              "No promotion selected")
        } else if (lift <= 0) {
          div(class = "badge-box",
              style = "background-color: #fdecea; color: #c0392b;",
              "Not recommended, negative lift")
        } else if (lift_pct < 50) {
          div(class = "badge-box",
              style = "background-color: #fef9e7; color: #d68910;",
              paste0("Weak lift (", lift_pct, "%), reconsider"))
        } else if (lift_pct < 150) {
          div(class = "badge-box",
              style = "background-color: #eaf4fb; color: #1a6fa5;",
              paste0("Moderate lift (", lift_pct, "%), proceed"))
        } else {
          div(class = "badge-box",
              style = "background-color: #eafaf1; color: #1e8449;",
              paste0("Strong lift (", lift_pct, "%), highly recommended"))
        }
      })
      
      # interpretation text
      output$interpretation <- renderText({
        if (no_promo) {
          paste0(
            "No promotion selected. Predicted demand for SKU ", input$sku_id,
            " at Store ", input$store_id, " is ", pred_with, " units ",
            "for the week of ", week_disp, "."
          )
        } else if (lift <= 0) {
          paste0(
            "The selected promotion is predicted to decrease sales by ",
            abs(lift), " units for SKU ", input$sku_id,
            " at Store ", input$store_id,
            " for the week of ", week_disp,
            ". Consider a different promotion strategy."
          )
        } else if (lift_pct < 50) {
          paste0(
            "The promotion is predicted to increase sales by only ", lift,
            " units (", lift_pct, "%) for SKU ", input$sku_id,
            " at Store ", input$store_id,
            " for the week of ", week_disp,
            ". This is below the typical lift range, consider whether the promotion cost is justified."
          )
        } else if (lift_pct < 150) {
          paste0(
            "The promotion is predicted to increase sales by ", lift,
            " units (", lift_pct, "%) for SKU ", input$sku_id,
            " at Store ", input$store_id,
            " for the week of ", week_disp,
            ". This is within the typical lift range, promotion is likely worthwhile."
          )
        } else {
          paste0(
            "The promotion is predicted to increase sales by ", lift,
            " units (", lift_pct, "%) for SKU ", input$sku_id,
            " at Store ", input$store_id,
            " for the week of ", week_disp,
            ". This is an exceptionally strong lift, prioritise this promotion."
          )
        }
      })
      
    }, error = function(e) {
      showNotification(
        paste("Prediction error:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)