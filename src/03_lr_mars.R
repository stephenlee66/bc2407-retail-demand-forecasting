# ============================================================================
# IMPORT LIBRARIES AND LOAD DATA
# ============================================================================

library(dplyr)
library(car)
library(earth)
library(ggplot2)

set.seed(42)

setwd("~/OneDrive - Nanyang Technological University/Y2S2/Analytics II/Project/dataset2/src")
df.train <- readRDS("../data/df_train.rds")
df.test  <- readRDS("../data/df_test.rds")

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
df.test.model <- df.test %>% select(-all_of(drop_cols))

# ============================================================================
# LINEAR REGRESSION
# ============================================================================

# VIF exploration to identify multicollinear variables
lr.full <- lm(units_sold ~ ., data = df.train.model)
summary(lr.full)
vif(lr.full)

# model specifications based on VIF analysis
lr_specs <- list(
  LR1 = c(),
  LR2 = c("total_price"),
  LR3 = c("total_price", "roll_mean_4"),
  LR4 = c("total_price", "roll_mean_4", "store_total_roll_mean_4")
)

lr_results <- list()
lr_models <- list()

for (model_name in names(lr_specs)) {
  
  drop_vars <- lr_specs[[model_name]]
  
  if (length(drop_vars) > 0) {
    train_data <- df.train.model %>% select(-all_of(drop_vars))
    test_data <- df.test.model  %>% select(-all_of(drop_vars))
  } else {
    train_data <- df.train.model
    test_data <- df.test.model
  }
  
  lr_model <- lm(units_sold ~ ., data = train_data)
  lr_models[[model_name]] <- lr_model
  
  metrics <- get_reg_metrics(lr_model, train_data, test_data)
  metrics$model <- model_name
  lr_results[[model_name]] <- metrics
}

lr_results <- do.call(rbind, lr_results)
lr_results <- lr_results[, c("model", "Dataset", "RMSE", "MAE", "R2")]
rownames(lr_results) <- NULL

lr_results_test <- subset(lr_results, Dataset == "Test")
lr_results_test <- lr_results_test[order(lr_results_test$RMSE), ]
rownames(lr_results_test) <- NULL
lr_results_test

# ============================================================================
# MARS
# ============================================================================

mars_grid <- expand.grid(
  degree = c(1, 2, 3),
  nprune = c(10, 15, 20, 25, 30)
)

mars_results <- list()
mars_models <- list()

for (i in 1:nrow(mars_grid)) {
  
  model_name <- paste0("deg", mars_grid$degree[i], "_np", mars_grid$nprune[i])
  
  mars_model <- earth(
    units_sold ~ .,
    data = df.train.model,
    degree = mars_grid$degree[i],
    nprune = mars_grid$nprune[i]
  )
  
  mars_models[[model_name]] <- mars_model
  
  metrics <- get_reg_metrics(mars_model, df.train.model, df.test.model)
  metrics$model <- model_name
  mars_results[[model_name]] <- metrics
}

mars_results <- do.call(rbind, mars_results)
mars_results <- mars_results[, c("model", "Dataset", "RMSE", "MAE", "R2")]
rownames(mars_results) <- NULL

mars_results_test <- subset(mars_results, Dataset == "Test")
mars_results_test <- mars_results_test[order(mars_results_test$RMSE), ]
rownames(mars_results_test) <- NULL
mars_results_test

# ============================================================================
# BEST LINEAR REGRESSION PLOTS
# ============================================================================

best_lr_name <- lr_results_test$model[1]
lr_best <- lr_models[[best_lr_name]]

par(mfrow = c(2, 2))
plot(lr_best, main = paste0(best_lr_name))
par(mfrow = c(1, 1))

lr_best_results <- get_reg_metrics(lr_best, df.train.model, df.test.model)
lr_best_results

# ============================================================================
# BEST MARS PLOTS
# ============================================================================

best_mars_name <- mars_results_test$model[1]
mars_best <- mars_models[[best_mars_name]]

pred_mars <- as.numeric(predict(mars_best, newdata = df.test.model))

plot_df_mars <- data.frame(
  actual = df.test.model$units_sold,
  predicted = pred_mars,
  residual = df.test.model$units_sold - pred_mars
)

# 1. Actual vs Predicted
plot(
  plot_df_mars$actual, plot_df_mars$predicted,
  xlab = "Actual Units Sold",
  ylab = "Predicted Units Sold",
  main = paste0("MARS (", best_mars_name, "): Actual vs Predicted"),
  pch = 16, cex = 0.4, col = rgb(0, 0, 0, 0.2)
)
abline(0, 1, col = "red", lwd = 2)

# 2. Residuals vs Predicted
plot(
  plot_df_mars$predicted, plot_df_mars$residual,
  xlab = "Predicted Units Sold",
  ylab = "Residuals",
  main = paste0("MARS (", best_mars_name, "): Residuals vs Predicted"),
  pch = 16, cex = 0.4, col = rgb(0, 0, 0, 0.2)
)
abline(h = 0, col = "red", lwd = 2)

# 3. Residual Histogram
hist(
  plot_df_mars$residual,
  main = paste0("MARS (", best_mars_name, "): Residual Histogram"),
  xlab = "Residuals",
  breaks = 30
)

# 4. Variable Importance
mars_imp <- evimp(mars_best)
mars_imp
plot(mars_imp)

# ============================================================================
# BEST LR FORECAST PLOT
# ============================================================================

pred_lr <- as.numeric(predict(lr_best, newdata = df.test.model %>% 
                                select(-all_of(lr_specs[[best_lr_name]]))))

train_trend_lr <- data.frame(
  date = df.train$date,
  actual = df.train$units_sold,
  period = "Train"
) %>%
  group_by(date, period) %>%
  summarise(actual = mean(actual), .groups = "drop")

test_trend_lr <- data.frame(
  date = df.test$date,
  actual = df.test$units_sold,
  predicted = pred_lr,
  period = "Test"
) %>%
  group_by(date, period) %>%
  summarise(
    actual = mean(actual),
    predicted = mean(predicted),
    .groups = "drop"
  )

ggplot() +
  geom_vline(xintercept = as.Date(max(train_trend_lr$date)) + 
               as.numeric(difftime(min(test_trend_lr$date), max(train_trend_lr$date), units = "days")) / 2,
             color = "black", linetype = "longdash", lwd = 0.8) +
  geom_line(data = train_trend_lr, aes(x = date, y = actual, color = "Train Actual"), lwd = 0.8) +
  geom_line(data = test_trend_lr, aes(x = date, y = actual, color = "Test Actual"), lwd = 0.8) +
  geom_line(data = test_trend_lr, aes(x = date, y = predicted, color = "Predicted"), lwd = 0.8) +
  scale_color_manual(values = c("Train Actual" = "grey50", "Test Actual" = "steelblue", "Predicted" = "red")) +
  labs(
    title = paste0("LR (", best_lr_name, "): Train/Test Actual vs Predicted"),
    x = "Date",
    y = "Average Units Sold",
    color = ""
  )

# ============================================================================
# BEST MARS FORECAST PLOT
# ============================================================================

train_trend_mars <- data.frame(
  date = df.train$date,
  actual = df.train$units_sold,
  period = "Train"
) %>%
  group_by(date, period) %>%
  summarise(actual = mean(actual), .groups = "drop")

test_trend_mars <- data.frame(
  date = df.test$date,
  actual = df.test$units_sold,
  predicted = pred_mars,
  period = "Test"
) %>%
  group_by(date, period) %>%
  summarise(
    actual = mean(actual),
    predicted = mean(predicted),
    .groups = "drop"
  )

ggplot() +
  geom_vline(xintercept = as.Date(max(train_trend_mars$date)) + 
               as.numeric(difftime(min(test_trend_mars$date), max(train_trend_mars$date), units = "days")) / 2,
             color = "black", linetype = "longdash", lwd = 0.8) +
  geom_line(data = train_trend_mars, aes(x = date, y = actual, color = "Train Actual"), lwd = 0.8) +
  geom_line(data = test_trend_mars, aes(x = date, y = actual, color = "Test Actual"), lwd = 0.8) +
  geom_line(data = test_trend_mars, aes(x = date, y = predicted, color = "Predicted"), lwd = 0.8) +
  scale_color_manual(values = c("Train Actual" = "grey50", "Test Actual" = "steelblue", "Predicted" = "red")) +
  labs(
    title = paste0("MARS (", best_mars_name, "): Train/Test Actual vs Predicted"),
    x = "Date",
    y = "Average Units Sold",
    color = ""
  )
