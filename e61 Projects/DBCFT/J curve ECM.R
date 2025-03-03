# Install and load required packages
#install.packages(c("readabs", "readrba", "data.table", "tseries", "dynlm", "sandwich", "lmtest", "ggplot2", "zoo"))
library(readabs)
library(readrba)
library(data.table)
library(tseries)
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

# === 3. Check for Cointegration ===
# Regress trade balance on exchange rate
long_run_model <- lm(trade_balance ~ twi, data = trade_data)

# Extract residuals (error term from long-run relationship)
trade_data[, ecm_residuals := residuals(long_run_model)]

# Test for stationarity of residuals using the ADF test
adf_test <- adf.test(trade_data$ecm_residuals)
print(adf_test)

# If p-value < 0.05, residuals are stationary, meaning a cointegration relationship exists.

# === 4. Create ECM Variables ===
setorder(trade_data, date)  # Ensure chronological order

# Compute First Differences
trade_data[, d_trade_balance := trade_balance - shift(trade_balance, type = "lag")]
trade_data[, d_twi := twi - shift(twi, type = "lag")]

# Lagged residual (EC term)
trade_data[, ecm_residuals_lag := shift(ecm_residuals, type = "lag")]

# Drop missing values after differencing
trade_data <- na.omit(trade_data)

# Convert to time-series (zoo) object
trade_data_ts <- zoo(trade_data[, .(d_trade_balance, d_twi, ecm_residuals_lag)], order.by = trade_data$date)

# === 5. Estimate ECM ===
ecm_model <- dynlm(d_trade_balance ~ L(ecm_residuals_lag, 1) + L(d_twi, 0:12), data = trade_data_ts)

# Compute Newey-West standard errors
nw_se_ecm <- coeftest(ecm_model, vcov = NeweyWest(ecm_model, lag = 12))

# Extract coefficients and standard errors
coef_df_ecm <- data.table(
  lag = 0:12,
  coef = coef(ecm_model)[3:15],  # Extract coefficients for d_twi lags
  se = nw_se_ecm[3:15, 2]  # Extract Newey-West standard errors
)

# Compute Confidence Intervals
coef_df_ecm[, `:=`(upper = coef + 1.96 * se, lower = coef - 1.96 * se)]

# Plot ECM J-Curve Response
ggplot(coef_df_ecm, aes(x = lag, y = coef)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  labs(title = "ECM J-Curve Effect: Impact of Exchange Rate on Trade Balance",
       x = "Lag (Months)", y = "Estimated Effect on Trade Balance") +
  theme_minimal() +
  geom_hline(yintercept = 0)
