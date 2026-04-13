# ============================================================================
# IMPORT LIBRARIES AND LOAD DATA
# ============================================================================

library(dplyr)
library(ggplot2)
library(reshape2)

# load clean data
setwd("~/OneDrive - Nanyang Technological University/Y2S2/Analytics II/Project/dataset2/src")
df.clean <- readRDS("../data/df_clean.rds")
df.feature <- readRDS("../data/df_feature.rds")

# ============================================================================
# BASIC OVERVIEW
# ============================================================================

dim(df.clean)
str(df.clean)
summary(df.clean)

cat("Unique stores:", n_distinct(df.clean$store_id), "\n")
cat("Unique SKUs:", n_distinct(df.clean$sku_id), "\n")
cat("Date range:", format(range(df.clean$date)), "\n")

sum(duplicated(df.clean))
sum(duplicated(df.raw))

# ============================================================================
# DISTRIBUTION OF UNITS SOLD
# ============================================================================

# # units sold distribution before removing outliers
ggplot(df.clean, aes(x = units_sold)) +
  geom_histogram(bins = 50, fill = "pink", color = "white") +
  labs(
    title = "Distribution of Units Sold (Before)",
    x = "Units Sold",
    y = "Count"
  )

# units sold distribution after removing outliers
ggplot(df.feature, aes(x = units_sold)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Units Sold (After)",
    x = "Units Sold",
    y = "Count"
  )

# ============================================================================
# DISTRIBUTION OF PRICES
# ============================================================================

ggplot(df.clean, aes(x = total_price)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "lightgreen", color = "white") +
  geom_density(color = "red", lwd = 1, adjust = 1.5) +
  labs(
    title = "Distribution of Total Price",
    x = "Total Price",
    y = "Density"
  )

ggplot(df.clean, aes(x = base_price)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "coral", color = "white") +
  geom_density(color = "red", lwd = 1, adjust = 1.5) +
  labs(
    title = "Distribution of Base Price",
    x = "Base Price",
    y = "Density"
  )

# ============================================================================
# SALES TREND OVER TIME
# ============================================================================

sales_trend <- df.clean %>%
  group_by(date) %>%
  summarise(avg_units_sold = mean(units_sold), .groups = "drop")

ggplot(sales_trend, aes(x = date, y = avg_units_sold)) +
  geom_line(color = "steelblue", lwd = 0.8) +
  geom_smooth(method = "loess", color = "red", lwd = 1, se = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Average Units Sold Over Time",
    x = "Date",
    y = "Average Units Sold"
  )

# price trend over time
price_trend <- df.clean %>%
  group_by(date) %>%
  summarise(avg_total_price = mean(total_price, na.rm = TRUE), .groups = "drop")

ggplot(price_trend, aes(x = date, y = avg_total_price)) +
  geom_line(color = "coral") +
  labs(
    title = "Average Total Price Over Time",
    x = "Time",
    y = "Average Total Price"
  )

# ============================================================================
# AVERAGE UNITS SOLD BY MONTH
# ============================================================================

monthly_sales <- df.clean %>%
  group_by(month) %>%
  summarise(avg_units_sold = mean(units_sold), .groups = "drop")

ggplot(monthly_sales, aes(x = factor(month), y = avg_units_sold)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(avg_units_sold, 1)), vjust = -0.5, size = 3) +
  scale_x_discrete(labels = month.abb) +
  labs(
    title = "Average Units Sold by Month",
    x = "Month",
    y = "Average Units Sold"
  )

# ============================================================================
# STORE LEVEL ANALYSIS
# ============================================================================

top_stores <- df.clean %>%
  group_by(store_id) %>%
  summarise(total_units = sum(units_sold), .groups = "drop") %>%
  arrange(desc(total_units)) %>%
  slice(1:10)

ggplot(top_stores, aes(x = reorder(store_id, total_units), y = total_units)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top 10 Stores by Total Units Sold",
    x = "Store ID",
    y = "Total Units Sold"
  )

# ============================================================================
# SKU LEVEL ANALYSIS
# ============================================================================

top_skus <- df.clean %>%
  group_by(sku_id) %>%
  summarise(total_units = sum(units_sold), .groups = "drop") %>%
  arrange(desc(total_units)) %>%
  slice(1:10)

ggplot(top_skus, aes(x = reorder(sku_id, total_units), y = total_units)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top 10 SKUs by Total Units Sold",
    x = "SKU ID",
    y = "Total Units Sold"
  )

# ============================================================================
# PROMOTION ANALYSIS
# ============================================================================

# featured sku
featured_summary <- df.clean %>%
  group_by(is_featured_sku) %>%
  summarise(avg_units_sold = mean(units_sold), .groups = "drop")

ggplot(featured_summary, aes(x = is_featured_sku, y = avg_units_sold, fill = is_featured_sku)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Units Sold by Featured SKU",
    x = "Is Featured SKU",
    y = "Average Units Sold"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# display sku
display_summary <- df.clean %>%
  group_by(is_display_sku) %>%
  summarise(avg_units_sold = mean(units_sold), .groups = "drop")

ggplot(display_summary, aes(x = is_display_sku, y = avg_units_sold, fill = is_display_sku)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Units Sold by Display SKU",
    x = "Is Display SKU",
    y = "Average Units Sold"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# both promotions combined
promo_summary <- df.clean %>%
  group_by(is_featured_sku, is_display_sku) %>%
  summarise(avg_units_sold = mean(units_sold), .groups = "drop") %>%
  mutate(promo_group = paste0("Featured=", is_featured_sku, ", Display=", is_display_sku))

ggplot(promo_summary, aes(x = reorder(promo_group, avg_units_sold), y = avg_units_sold, fill = promo_group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Average Units Sold by Promotion Combination",
    x = "Promotion Group",
    y = "Average Units Sold"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ============================================================================
# CORRELATION HEATMAP
# ============================================================================

num_vars <- df.clean %>%
  select(total_price, base_price, units_sold, month, day)

cor_matrix <- cor(num_vars, use = "complete.obs")
cor_melted <- melt(cor_matrix)

ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1, 1), name = "Correlation") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  labs(
    title = "Correlation Heatmap",
    x = "",
    y = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

