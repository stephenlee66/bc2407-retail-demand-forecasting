# ============================================================================
# IMPORT LIBRARIES AND LOAD DATA
# ============================================================================

library(dplyr)
library(randomForest)
library(ggplot2)

set.seed(42)

setwd("~/OneDrive - Nanyang Technological University/Y2S2/Analytics II/Project/dataset2/src")
df.train <- readRDS("../data/df_train.rds")
df.test <- readRDS("../data/df_test.rds")

# ============================================================================
# REUSABLE REGRESSION METRICS FUNCTION
# ============================================================================

get_reg_metrics <- function(model, train_data, test_data, target = "units_sold") {
  
  pred_train <- as.numeric(predict(model, newdata = train_data))
  pred_test <- as.numeric(predict(model, newdata = test_data))
  
  actual_train <- train_data[[target]]
  actual_test <- test_data[[target]]
  
  rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
  mae <- function(actual, pred) mean(abs(actual - pred), na.rm = TRUE)
  r2 <- function(actual, pred) {
    1 - sum((actual - pred)^2, na.rm = TRUE) /
      sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  }
  
  data.frame(
    Dataset = c("Train", "Test"),
    RMSE = c(rmse(actual_train, pred_train), rmse(actual_test, pred_test)),
    MAE = c(mae(actual_train, pred_train), mae(actual_test, pred_test)),
    R2 = c(r2(actual_train, pred_train), r2(actual_test, pred_test))
  )
}

# ============================================================================
# PREPARE MODELLING DATA
# ============================================================================

drop_cols <- c("date", "record_ID", "store_id", "sku_id")

df.train.model <- df.train %>% select(-all_of(drop_cols))
df.test.model <- df.test  %>% select(-all_of(drop_cols))

# ============================================================================
# RANDOM FOREST
# ============================================================================

rf_grid <- expand.grid(
  ntree = c(500),
  mtry = c(6)
)

rf_results <- data.frame()
rf_models <- list()

for (i in 1:nrow(rf_grid)) {
  
  model_name <- paste0("nt", rf_grid$ntree[i], "_mt", rf_grid$mtry[i])
  
  rf_model <- randomForest(
    units_sold ~ .,
    data = df.train.model,
    ntree = rf_grid$ntree[i],
    mtry = rf_grid$mtry[i],
    nodesize = 20,
    maxnodes = 700,
    importance = TRUE
  )
  
  rf_models[[model_name]] <- rf_model
  
  metrics <- get_reg_metrics(rf_model, df.train.model, df.test.model)
  metrics$model <- model_name
  rf_results <- rbind(rf_results, metrics)
}

rownames(rf_results) <- NULL
rf_results <- rf_results[, c("model", "Dataset", "RMSE", "MAE", "R2")]

rf_results_test <- subset(rf_results, Dataset == "Test")
rf_results_test <- rf_results_test[order(rf_results_test$RMSE), ]
rownames(rf_results_test) <- NULL
rf_results_test

# ============================================================================
# BEST RF PLOTS
# ============================================================================

best_rf_name <- rf_results_test$model[1]
rf_best <- rf_models[[best_rf_name]]

pred_rf <- as.numeric(predict(rf_best, newdata = df.test.model))

plot_df_rf <- data.frame(
  actual = df.test.model$units_sold,
  predicted = pred_rf,
  residual  = df.test.model$units_sold - pred_rf
)

# 1. Actual vs Predicted
plot(
  plot_df_rf$actual, plot_df_rf$predicted,
  xlab = "Actual Units Sold",
  ylab = "Predicted Units Sold",
  main = paste0("RF (", best_rf_name, "): Actual vs Predicted"),
  pch  = 16, cex = 0.4, col = rgb(0, 0, 0, 0.2)
)
abline(0, 1, col = "red", lwd = 2)

# 2. Residuals vs Predicted
plot(
  plot_df_rf$predicted, plot_df_rf$residual,
  xlab = "Predicted Units Sold",
  ylab = "Residuals",
  main = paste0("RF (", best_rf_name, "): Residuals vs Predicted"),
  pch  = 16, cex = 0.4, col = rgb(0, 0, 0, 0.2)
)
abline(h = 0, col = "red", lwd = 2)

# 3. Residual Histogram
hist(
  plot_df_rf$residual,
  main = paste0("RF (", best_rf_name, "): Residual Histogram"),
  xlab = "Residuals",
  breaks = 30
)

# 4. Variable Importance
varImpPlot(
  rf_best,
  main = paste0("RF (", best_rf_name, "): Variable Importance"),
  type = 1
)

# ============================================================================
# BEST RF FORECAST PLOT
# ============================================================================

train_trend_rf <- data.frame(
  date = df.train$date,
  actual = df.train$units_sold,
  period = "Train"
) %>%
  group_by(date, period) %>%
  summarise(actual = mean(actual), .groups = "drop")

test_trend_rf <- data.frame(
  date = df.test$date,
  actual = df.test$units_sold,
  predicted = pred_rf,
  period = "Test"
) %>%
  group_by(date, period) %>%
  summarise(
    actual = mean(actual),
    predicted = mean(predicted),
    .groups = "drop"
  )

ggplot() +
  geom_vline(xintercept = as.Date(max(train_trend_rf$date)) + 
               as.numeric(difftime(min(test_trend_rf$date), max(train_trend_rf$date), units = "days")) / 2,
             color = "black", linetype = "longdash", lwd = 0.8) +
  geom_line(data = train_trend_rf, aes(x = date, y = actual, color = "Train Actual"), lwd = 0.8) +
  geom_line(data = test_trend_rf, aes(x = date, y = actual, color = "Test Actual"), lwd = 0.8) +
  geom_line(data = test_trend_rf, aes(x = date, y = predicted, color = "Predicted"), lwd = 0.8) +
  scale_color_manual(values = c("Train Actual" = "grey50", "Test Actual" = "steelblue", "Predicted" = "red")) +
  labs(
    title = paste0("RF (", best_rf_name, "): Train/Test Actual vs Predicted"),
    x = "Date",
    y = "Average Units Sold",
    color = ""
  )

# ============================================================================
# SAVE BEST RF MODEL
# ============================================================================

# to be loaded in rshiny app
saveRDS(rf_best, "../data/rf_best.rds")

# ============================================================================
# RANGER ** run for faster testing of RF only
# ============================================================================

library(ranger)

ranger_grid <- expand.grid(
  ntree = c(500, 600),
  mtry = c(5, 6, 7)
)

ranger_results <- data.frame()
ranger_models <- list()

for (i in 1:nrow(ranger_grid)) {
  
  model_name <- paste0("nt", ranger_grid$ntree[i], "_mt", ranger_grid$mtry[i])
  
  ranger_model <- ranger(
    units_sold ~ .,
    data = df.train.model,
    num.trees = ranger_grid$ntree[i],
    mtry = ranger_grid$mtry[i],
    importance = "permutation",
    min.node.size = 20,
    max.depth = 10,
    seed = 42
  )
  
  ranger_models[[model_name]] <- ranger_model
  
  pred_train <- predict(ranger_model, data = df.train.model)$predictions
  pred_test <- predict(ranger_model, data = df.test.model)$predictions
  
  actual_train <- df.train.model$units_sold
  actual_test <- df.test.model$units_sold
  
  rmse <- function(a, p) sqrt(mean((a - p)^2))
  mae <- function(a, p) mean(abs(a - p))
  r2 <- function(a, p) 1 - sum((a - p)^2) / sum((a - mean(a))^2)
  
  metrics <- data.frame(
    model = model_name,
    Dataset = c("Train", "Test"),
    RMSE = c(rmse(actual_train, pred_train), rmse(actual_test, pred_test)),
    MAE = c(mae(actual_train, pred_train), mae(actual_test, pred_test)),
    R2 = c(r2(actual_train, pred_train), r2(actual_test, pred_test))
  )
  
  ranger_results <- rbind(ranger_results, metrics)
}

rownames(ranger_results) <- NULL

ranger_results_test <- subset(ranger_results, Dataset == "Test")
ranger_results_test <- ranger_results_test[order(ranger_results_test$RMSE), ]
rownames(ranger_results_test) <- NULL
ranger_results_test
