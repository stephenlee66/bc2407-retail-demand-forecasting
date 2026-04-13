# 🛒 BC2407 Retail Demand Forecasting

Group project for BC2407 Analytics II: Advanced Predictive Techniques, focused on forecasting weekly retail demand and quantifying the impact of promotions on sales.

## 📌 Project Overview

This project develops a ML pipeline to forecast weekly units sold across **76 stores** and **28 SKUs** spanning January 2022 to July 2024. The pipeline engineers time-series features at 3 levels (store-SKU, store, SKU), compares Linear Regression, MARS, and Random Forest models, and deploys the best model in an interactive Shiny application that predicts promotion impact.

The final Random Forest model achieved **R² = 0.77** and **RMSE = 21.51** on the test set, with `is_featured_sku` as the dominant predictor.

## 📂 Repository Structure

```
├── data/
│   ├── retail_data.csv          # Raw dataset (Kaggle)
│   ├── df_clean.rds             # Cleaned data (for EDA)
│   ├── df_feature.rds           # Feature-engineered data (for EDA)
│   ├── df_train.rds             # Training set
│   ├── df_test.rds              # Test set
│   └── rf_best.rds              # Saved Random Forest model
│
├── src/
│   ├── 01_data_prep.R           # Data cleaning, feature engineering, outlier filtering, train-test split
│   ├── 02_eda.R                 # Exploratory data analysis and visualisations
│   ├── 03_lr_mars.R             # Linear Regression and MARS modelling with grid search
│   ├── 04_rf.R                  # Random Forest modelling with Ranger for hyperparameter testing
│   └── 05_app.R                 # Shiny application — Weekly Promotion Impact Predictor
│
└── README.md
```

## 📊 Dataset

| Variable | Type | Description |
|---|---|---|
| `record_ID` | Integer | Unique row identifier |
| `week` | Date | Date of the transaction |
| `store_id` | Factor | Store identifier (76 unique) |
| `sku_id` | Factor | Product identifier (28 unique) |
| `total_price` | Numeric | Selling price |
| `base_price` | Numeric | Original price before discounts |
| `is_featured_sku` | Binary | 1 = Product was featured in promotion |
| `is_display_sku` | Binary | 1 = Product was on display promotion |
| `units_sold` | Integer | **Target variable**: weekly units sold |

## 🔍 Key EDA Findings

| # | Finding | Insight |
|---|---|---|
| 1 | `units_sold` heavily right-skewed (median 35, max 2,876) | Extreme tail filtered at 99th percentile (283 units) |
| 2 | Featured SKUs sell **~2.5×** more than non-featured | Promotions are the dominant demand driver |
| 3 | Combined promotions (featured + display) yield **highest** sales | Effects are additive, not redundant |
| 4 | February, May, December are peak months | Seasonal and promotional activity concentrated |
| 5 | Store 8023 and SKU 219009 dominate volume | High concentration justifies store/SKU aggregate features |
| 6 | Weak negative price-sales correlation (-0.24) | Demand shaped by more than price alone |

## ⚙️ Methodology

### Pipeline Overview

```
Raw Dataset (150,150 rows)
        │
        ▼
  Data Cleaning
  (date parsing, factor conversion, 1 NA imputed after split using train set only)
        │
        ▼
  Feature Engineering
  (lags, rolling stats at 3 levels)
        │
        ▼
  Outlier Filtering (99th percentile)
  (~1,498 rows removed, 1% of data)
        │
        ▼
  Time-Based Train/Test Split (70/30)
        │
        ├──► df.train — historical data for model training
        │
        └──► df.test — future data for evaluation
```

### Feature Engineering

**Store-SKU Level:**

| Feature | Description |
|---|---|
| `lag_1` | Units sold 1 week prior |
| `lag_2` | Units sold 2 weeks prior |
| `lag_4` | Units sold 4 weeks prior |
| `roll_mean_4` | 4-week rolling mean of lagged sales |
| `roll_sd_4` | 4-week rolling standard deviation of lagged sales |

**Store Level (aggregated across all SKUs per store):**

| Feature | Description |
|---|---|
| `store_total_lag_1` | Total store sales 1 week prior |
| `store_total_roll_mean_4` | 4-week rolling mean of total store sales |
| `store_total_roll_sd_4` | 4-week rolling standard deviation of total store sales |

**SKU Level (aggregated across all stores per SKU):**

| Feature | Description |
|---|---|
| `sku_total_lag_1` | Total SKU sales 1 week prior |
| `sku_total_roll_mean_4` | 4-week rolling mean of total SKU sales |
| `sku_total_roll_sd_4` | 4-week rolling standard deviation of total SKU sales |

> All features use lagged values only so no future data leakage. Rows with insufficient history (~first 4 weeks per store-SKU) are dropped. Outlier filtering applied after feature engineering to preserve lag integrity.

### Variables Dropped before Modelling

| Variable | Reason |
|---|---|
| `record_ID` | Identifier only, no predictive value |
| `date` | Replaced by `month` and `day` features |
| `store_id` | Replaced by store-level aggregate features |
| `sku_id` | Replaced by SKU-level aggregate features |

### Outlier Treatment

| Stage | Max | Median |
|---|---|---|---|
| Before filtering | 2,876 | 35 |
| After filtering (99th pctl) | 286 | 35 |

~1,498 rows removed (1%). Extreme values were predominantly SKU 219009 during Feb 2022 with both promotions active.

## 🤖 Models

### Linear Regression

| Model | Variables Dropped | Test RMSE | Test R² |
|---|---|---|---|
| LR1 | None (full model) | 25.98 | 0.660 |
| LR2 | `total_price` | 26.14 | 0.656 |
| LR3 | `total_price`, `roll_mean_4` | 26.30 | 0.651 |
| LR4 | `total_price`, `roll_mean_4`, `store_total_roll_mean_4` | 26.39 | 0.649 |

### MARS

| Model | Configuration | Test RMSE | Test R² |
|---|---|---|---|
| deg1_np30 | degree=1, nprune=30 | 23.92 | 0.711 |
| deg1_np25 | degree=1, nprune=25 | 23.94 | 0.711 |
| deg1_np20 | degree=1, nprune=20 | 23.96 | 0.711 |

### Random Forest

| Model | Configuration | Test RMSE | Test R² |
|---|---|---|---|
| nt500_mt6 | ntree=500, mtry=6 | 21.51 | **0.767 |

> `nt500_mt6` was selected as the final model through fast testing of different parameter combinations using `ranger` package.

## 📈 Final Results

| Model | Test RMSE | Test MAE | Test R² |
|---|---|---|---|
| Linear Regression (LR1) | 25.98 | 16.66 | 0.660 |
| MARS (deg1_np30) | 23.92 | 15.60 | 0.711 |
| **Random Forest (nt500_mt6)** | **21.51** | **13.73** | **0.767** |

### ✅ Recommended Model — Random Forest (nt500_mt6)

Random Forest outperforms LR and MARS across all metrics, capturing nonlinear relationships between promotions, pricing, and demand that linear models cannot.

### Variable Importance (RF — %IncMSE)

| Rank | Variable | %IncMSE | Insight |
|---|---|---|---|
| 1 | `is_featured_sku` | ~110% | Dominant demand driver — validates app purpose |
| 2 | `month` | ~85% | Strong seasonal effects |
| 3 | `day` | ~80% | Week-start timing affects demand |
| 4 | `is_display_sku` | ~72% | Secondary promotion effect |
| 5 | `sku_total_roll_sd_4` | ~65% | Product demand volatility signals |

## 🖥️ Shiny Application

The app loads the saved RF model (`rf_best.rds`) and provides a **Weekly Promotion Impact Predictor**:

1. User selects store, SKU, promotion plan, pricing, and recent sales history
2. `build_input()` assembles inputs into the model's expected column structure
3. Model predicts twice with selected promotion and without any promotion
4. Sales lift = difference between the two predictions
5. Recommendation displayed as weak / moderate / strong lift

**To run:** Ensure `rf_best.rds` and `df_train.rds` is in the `../data/` directory and run `05_app.R`. Update the `setwd()` path to match your file structure.

## 🔍 Limitations

1. **Test set used for model selection:** All model configurations were evaluated directly on the test set. A validation set or cross-validation would provide less optimistic performance estimates.
2. **Stale store/SKU aggregates in app:** The Shiny app pulls store and SKU aggregate features from the training data. In production, these would need to be refreshed with live data weekly.
3. **No lag_3 in model, but used in app:** `lag_3` is collected in the app solely to compute `roll_mean_4` and `roll_sd_4` accurately, it is not passed to the model as a feature.
4. **Static Kaggle dataset:** The dataset is a fixed historical snapshot. The app simulates real-time deployment but cannot reflect actual current demand patterns.

## 🛠️ Libraries Used

| Package | Purpose |
|---|---|
| `dplyr` | Data manipulation |
| `lubridate` | Date handling |
| `zoo` | Rolling window calculations |
| `ggplot2` | Visualisation |
| `car` | VIF multicollinearity analysis |
| `earth` | MARS modelling |
| `randomForest` | Random Forest modelling |
| `ranger` | Fast RF hyperparameter testing |
| `reshape2` | Correlation heatmap preparation |
| `shiny` | Interactive web application |
