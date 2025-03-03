# Install necessary packages
#install.packages(c("readabs", "readrba", "data.table", "dynlm", "sandwich", "lmtest", "ggplot2", "zoo"))
library(readabs)
library(readrba)
library(data.table)
library(dynlm)
library(sandwich)
library(lmtest)
library(ggplot2)
library(zoo)

rm(list=ls())

start_date <- as.Date("1990-01-01")

# === 1. Download Trade Balance Data from ABS ===
trade_data <- read_abs("5368.0")
setDT(trade_data)

# Filter for Exports and Imports, using Seasonally Adjusted Data from Table 536801
trade_data <- trade_data[
  series %in% c("Credits, Total goods ;", "Debits, Total goods ;") &
    table_no == "536801" & series_type == "Seasonally Adjusted"
]

# Convert to wide format
trade_data <- dcast(trade_data, date ~ series, value.var = "value")

# Rename columns
setnames(trade_data, old = c("Credits, Total goods ;", "Debits, Total goods ;"), 
         new = c("exports", "imports"))

# Compute Trade Balance (since imports are negative, use addition)
trade_data[, trade_balance := exports + imports]

# === 2. Retrieve Exchange Rate Data from RBA ===
# Use readrba to fetch the Trade-Weighted Index (TWI)
fx_data <- read_rba(series_id = "FXRTWI") # TWI Index from RBA
setDT(fx_data)

# Keep only necessary columns (date and TWI)
fx_data <- fx_data[, .(date, value)]
fx_data[, `:=`(year = year(date), month = month(date))]

# Calculate monthly average TWI
monthly_twi <- fx_data[, .(twi = mean(value, na.rm = TRUE)), by = .(year, month)]
monthly_twi[, date := as.Date(paste(year, month, "01", sep = "-"))]

# Merge Trade Balance with Exchange Rate Data
trade_data <- merge(trade_data, monthly_twi, by = "date", all.x = TRUE)

# Deflate by total trade flow
trade_data[, total_trade := exports - imports]
trade_data[, balance_ratio := trade_balance / total_trade]

# === 3. Compute Lagged and Differenced Variables ===
setorder(trade_data, date)  # Ensure chronological order

# Compute First Differences
trade_data[, d_fx := twi - shift(twi, type = "lag")]
trade_data[, d_trade_balance := trade_balance - shift(trade_balance, type = "lag")]
trade_data[, d_balance_ratio := balance_ratio - shift(balance_ratio, type = "lag")]

# Drop missing values after differencing
trade_data <- na.omit(trade_data)

# Set dates of interest
trade_data <- trade_data[date >= start_date]

# Convert to time-series (zoo) object
trade_data_ts <- zoo(trade_data[, .(d_trade_balance, d_balance_ratio, d_fx)], order.by = trade_data$date)

# === 4. Estimate ARDL Model for Trade Balance ===
j_curve_ardl <- dynlm(d_trade_balance ~ L(d_trade_balance, 1) + L(d_fx, 0:12), data = trade_data_ts)

# Compute Newey-West standard errors
nw_se_ardl <- coeftest(j_curve_ardl, vcov = NeweyWest(j_curve_ardl, lag = 12))

# Extract coefficients and standard errors
coef_df_ardl <- data.table(
  lag = 0:12,
  coef = coef(j_curve_ardl)[3:15],  # Extract coefficients for d_fx lags
  se = nw_se_ardl[3:15, 2]  # Extract Newey-West standard errors
)

# Compute Confidence Intervals
coef_df_ardl[, `:=`(upper = coef + 1.96 * se, lower = coef - 1.96 * se)]

# Plot ARDL J-Curve Response for Trade Balance
ggplot(coef_df_ardl, aes(x = lag, y = coef)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  labs(title = "ARDL J-Curve Effect: Impact of Exchange Rate on Trade Balance",
       x = "Lag (Months)", y = "Estimated Effect on Trade Balance") +
  theme_minimal() +
  geom_hline(yintercept = 0)

# === 5. Estimate ARDL Model for Trade Balance Ratio ===
j_curve_ardl_ratio <- dynlm(d_balance_ratio ~ L(d_balance_ratio, 1) + L(d_fx, 0:12), data = trade_data_ts)

# Compute Newey-West standard errors
nw_se_ardl_ratio <- coeftest(j_curve_ardl_ratio, vcov = NeweyWest(j_curve_ardl_ratio, lag = 12))

# Extract coefficients and standard errors
coef_df_ardl_ratio <- data.table(
  lag = 0:12,
  coef = coef(j_curve_ardl_ratio)[3:15],  # Extract coefficients for d_fx lags
  se = nw_se_ardl_ratio[3:15, 2]  # Extract Newey-West standard errors
)

# Compute Confidence Intervals
coef_df_ardl_ratio[, `:=`(upper = coef + 1.96 * se, lower = coef - 1.96 * se)]

# Plot ARDL J-Curve Response for Trade Balance Ratio
ggplot(coef_df_ardl_ratio, aes(x = lag, y = coef)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  labs(title = "ARDL J-Curve Effect: Impact of Exchange Rate on Trade Balance Ratio",
       x = "Lag (Months)", y = "Estimated Effect on Trade Balance Ratio") +
  theme_minimal() +
  geom_hline(yintercept = 0)
