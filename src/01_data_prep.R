# ============================================================================
# IMPORT LIBRARIES AND LOAD DATA
# ============================================================================

library(dplyr)
library(lubridate)
library(zoo)

# load data
setwd("~/OneDrive - Nanyang Technological University/Y2S2/Analytics II/Project/dataset2/src")

if (!dir.exists("../data")) {
  dir.create("../data")
}

df.raw <- read.csv("../data/retail_data.csv", header = TRUE)

# ============================================================================
# INITIAL INSPECTION
# ============================================================================

summary(df.raw)
dim(df.raw)
str(df.raw)

# ============================================================================
# DATA CLEANING
# ============================================================================

df.clean <- df.raw %>%
  mutate(
    week = as.Date(week, format = "%d/%m/%y"),
    month = month(week),
    day = day(week),
    store_id = factor(store_id),
    sku_id = factor(sku_id),
    is_featured_sku = factor(is_featured_sku),
    is_display_sku = factor(is_display_sku)
  ) %>%
  arrange(store_id, sku_id, week) %>%
  rename(date = week)

# ============================================================================
# FEATURE ENGINEERING
# ============================================================================

# lag features
df.feature <- df.clean %>%
  arrange(store_id, sku_id, date) %>%
  group_by(store_id, sku_id) %>%
  mutate(
    lag_1 = lag(units_sold, 1),
    lag_2 = lag(units_sold, 2),
    lag_4 = lag(units_sold, 4),
    roll_mean_4 = zoo::rollmean(lag(units_sold, 1), k = 4, fill = NA, align = "right"),
    roll_sd_4 = zoo::rollapply(lag(units_sold, 1), width = 4, FUN = sd, fill = NA, align = "right")
  ) %>%
  ungroup() %>%
  filter(!is.na(lag_1), !is.na(lag_2), !is.na(lag_4), !is.na(roll_mean_4), !is.na(roll_sd_4))

# store-level time-series aggregates
store_daily <- df.clean %>%
  group_by(store_id, date) %>%
  summarise(
    store_total_units = sum(units_sold),
    .groups = "drop"
  ) %>%
  group_by(store_id) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    store_total_lag_1 = lag(store_total_units, 1),
    store_total_roll_mean_4 = zoo::rollmean(lag(store_total_units, 1), k = 4, fill = NA, align = "right"),
    store_total_roll_sd_4 = zoo::rollapply(lag(store_total_units, 1), width = 4, FUN = sd, fill = NA, align = "right")
  ) %>%
  ungroup()

# sku-level time-series aggregates
sku_daily <- df.clean %>%
  group_by(sku_id, date) %>%
  summarise(
    sku_total_units = sum(units_sold),
    .groups = "drop"
  ) %>%
  group_by(sku_id) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    sku_total_lag_1 = lag(sku_total_units, 1),
    sku_total_roll_mean_4 = zoo::rollmean(lag(sku_total_units, 1), k = 4, fill = NA, align = "right"),
    sku_total_roll_sd_4 = zoo::rollapply(lag(sku_total_units, 1), width = 4, FUN = sd, fill = NA, align = "right")
  ) %>%
  ungroup()

# join back
df.feature <- df.feature %>%
  left_join(
    store_daily %>%
      select(date, store_id, store_total_lag_1, store_total_roll_mean_4, store_total_roll_sd_4),
    by = c("date", "store_id")
  ) %>%
  left_join(
    sku_daily %>%
      select(date, sku_id, sku_total_lag_1, sku_total_roll_mean_4, sku_total_roll_sd_4),
    by = c("date", "sku_id")
  ) %>%
  filter(
    !is.na(store_total_lag_1),
    !is.na(store_total_roll_mean_4),
    !is.na(store_total_roll_sd_4),
    !is.na(sku_total_lag_1),
    !is.na(sku_total_roll_mean_4),
    !is.na(sku_total_roll_sd_4)
  )

# filter off extreme values
print(nrow(df.feature))
upper_limit <- quantile(df.feature$units_sold, 0.99)
df.feature <- df.feature %>% filter(units_sold <= upper_limit)
print(nrow(df.feature))

# ============================================================================
# TRAIN TEST SPLIT BY DATE (70/30)
# ============================================================================

all_dates <- sort(unique(df.feature$date))
cutoff <- all_dates[floor(0.7 * length(all_dates))]

df.train <- df.feature %>% filter(date <= cutoff)
df.test <- df.feature %>% filter(date > cutoff)

range(df.train$date)
range(df.test$date)

nrow(df.train)
nrow(df.test)

# check total NA
sum(is.na(df.train))
sum(is.na(df.test))

# replace with train median
median_total_price <- median(df.train$total_price, na.rm = TRUE)
df.test$total_price[is.na(df.test$total_price)] <- median_total_price

# check NA again
sum(is.na(df.test))

# ============================================================================
# SAVE DATA IN DIRECTORY
# ============================================================================

# save for EDA visualization script
saveRDS(df.clean, "../data/df_clean.rds")
saveRDS(df.feature, "../data/df_feature.rds")

# save for modelling script(s)
saveRDS(df.train, "../data/df_train.rds")
saveRDS(df.test, "../data/df_test.rds")
