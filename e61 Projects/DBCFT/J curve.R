

# Install necessary packages
#install.packages(c("readabs", "readrba", "data.table", "fixest", "ggplot2","sandwhich"))
library(readabs)
library(readrba)
library(data.table)
library(fixest)
library(ggplot2)

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
browse_rba_series("TWI")

fx_data <- read_rba(series_id = "FXRTWI") # TWI Index from RBA
setDT(fx_data)

# Keep only necessary columns (date and TWI)
fx_data <- fx_data[, .(date, value)]

fx_data[, `:=`(year = year(date), month = month(date))]

# Calculate monthly average TWI
monthly_twi <- fx_data[, .(twi = mean(value, na.rm = TRUE)), by = .(year, month)]

# Create a 'year-month' identifier
monthly_twi[, date := as.Date(paste(year, month, "01", sep = "-"))]

# Merge Trade Balance with Exchange Rate Data
trade_data <- merge(trade_data, monthly_twi, by = "date", all.x = TRUE)

# Deflate by total trade flow - this may exaggerate the result if the phenomenon holds.

trade_data[,total_trade := exports - imports][,balance_ratio:= trade_balance/total_trade]

# === 3. Compute Lagged and Differenced Variables ===
setorder(trade_data, date)  # Ensure chronological order

# Compute First Differences
trade_data[, d_fx := twi - shift(twi, type = "lag")]
trade_data[, d_trade_balance := trade_balance - shift(trade_balance, type = "lag")]
trade_data[, d_balance_ratio := balance_ratio - shift(balance_ratio, type = "lag")]

# Generate lags for d_fx (0 to 12 months)
for (i in 0:12) {
  trade_data[, paste0("d_fx_lag", i) := shift(d_fx, i, type = "lag")]
}

# Generate lag for trade balance
trade_data[, d_trade_balance_lag1 := shift(d_trade_balance, 1, type = "lag")]
trade_data[, d_balance_ratio_lag1 := shift(d_balance_ratio, 1, type = "lag")]


# Drop missing values after differencing
trade_data <- na.omit(trade_data)

# Set dates of interest

trade_data <- trade_data[date >= start_date]

# === 4. Estimate J-Curve Using Distributed Lag Model ===
j_curve_model <- lm(d_trade_balance ~ d_fx_lag0 + d_fx_lag1 + d_fx_lag2 + 
                      d_fx_lag3 + d_fx_lag4 + d_fx_lag5 + d_fx_lag6 + d_fx_lag7 +
                      d_fx_lag8 + d_fx_lag9 + d_fx_lag10 + d_fx_lag11 + d_fx_lag12 +d_trade_balance_lag1, data = trade_data) #  +d_trade_balance_lag1

nw_se <- sqrt(diag(sandwich::NeweyWest(j_curve_model, lag = 12)))

# Extract coefficients and standard errors
coef_df <- data.table(
  lag = 0:12,
  coef = coef(j_curve_model)[2:14],  # Extract coefficients for d_fx lags
  se = nw_se[2:14]  # Extract Newey-West standard errors
)

# Compute Confidence Intervals
coef_df[, `:=`(upper = coef + 1.96 * se, lower = coef - 1.96 * se)]

# Plot J-Curve Response
ggplot(coef_df, aes(x = lag, y = coef)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  labs(title = "J-Curve Effect: Impact of Exchange Rate on Trade Balance",
       x = "Lag (Months)", y = "Estimated Effect on Trade Balance") +
  theme_minimal() +
  geom_hline(yintercept = 0) 

# Estimate second model for balance ratio

j_curve_model2 <- lm(d_balance_ratio ~ d_fx_lag0 + d_fx_lag1 + d_fx_lag2 + 
                      d_fx_lag3 + d_fx_lag4 + d_fx_lag5 + d_fx_lag6 + d_fx_lag7 +
                      d_fx_lag8 + d_fx_lag9 + d_fx_lag10 + d_fx_lag11 + d_fx_lag12 +d_balance_ratio_lag1, data = trade_data) #  +d_trade_balance_lag1

nw_se2 <- sqrt(diag(sandwich::NeweyWest(j_curve_model2, lag = 12)))

# Extract coefficients and standard errors
coef_df2 <- data.table(
  lag = 0:12,
  coef = coef(j_curve_model2)[2:14],  # Extract coefficients for d_fx lags
  se = nw_se2[2:14]  # Extract Newey-West standard errors
)

# Compute Confidence Intervals
coef_df2[, `:=`(upper = coef + 1.96 * se, lower = coef - 1.96 * se)]

# Plot J-Curve Response
ggplot(coef_df2, aes(x = lag, y = coef)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  labs(title = "J-Curve Effect: Impact of Exchange Rate on Trade Balance Ratio",
       x = "Lag (Months)", y = "Estimated Effect on Trade Balance") +
  theme_minimal() +
  geom_hline(yintercept = 0) 
