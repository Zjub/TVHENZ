rm(list = ls())
gc()

# Load required libraries
library(tidyverse)
library(data.table)
library(vars)       # For VAR models
library(tseries)    # For stationarity tests
library(ggplot2)
library(readabs)

# Load and prepare GDP data
a <- read_abs("5206.0")
setDT(a)

# Filter for the required series: GDP, Terms of Trade, and RNGDI
data_dt <- a[
  date >= as.Date("1980-01-01") & 
    series %in% c(
      "GDP per hour worked: Index ;",
      "Terms of trade: Index ;",
      "Real net national disposable income per capita: Chain volume measures ;"
    ) & 
    series_type == "Seasonally Adjusted"
]

# Calculate log differences to ensure stationarity
data_dt[, log_diff := log(value) - shift(log(value)), by = series]

# Remove rows with NA log_diff
data_dt <- data_dt[!is.na(log_diff)]

# Reshape data to wide format for modeling
data_wide <- dcast(data_dt, date ~ series, value.var = "log_diff")

# Rename columns for convenience
setnames(data_wide, 
         c("date", 
           "GDP per hour worked: Index ;", 
           "Terms of trade: Index ;", 
           "Real net national disposable income per capita: Chain volume measures ;"), 
         c("date", "log_diff_gdp", "log_diff_tot", "log_diff_rngdi"))

# Remove NA rows from lagging
data_wide <- na.omit(data_wide)

# Restrict the dataset to the last decade (2013–2023)
data_last_decade <- data_wide[date >= as.Date("2013-01-01") & date <= as.Date("2023-12-31")]

# Fit the VAR model using the full dataset
lag_selection <- VARselect(data_wide[, .(log_diff_rngdi, log_diff_gdp, log_diff_tot)], lag.max = 10, type = "const")
var_model <- VAR(data_wide[, .(log_diff_rngdi, log_diff_gdp, log_diff_tot)], 
                 p = lag_selection$selection["AIC(n)"], 
                 type = "const")

# Counterfactual scenarios
# Scenario 1: Hold Terms of Trade constant
counterfactual_tot <- data_last_decade[, .(log_diff_rngdi, log_diff_gdp, log_diff_tot = mean(log_diff_tot))]

# Scenario 2: Hold GDP constant
counterfactual_gdp <- data_last_decade[, .(log_diff_rngdi, log_diff_gdp = mean(log_diff_gdp), log_diff_tot)]

# Predict under each scenario
counterfactual_tot_pred <- predict(var_model, newdata = counterfactual_tot, n.ahead = nrow(counterfactual_tot))
counterfactual_gdp_pred <- predict(var_model, newdata = counterfactual_gdp, n.ahead = nrow(counterfactual_gdp))

# Extract predictions for RNGDI
rngdi_actual <- cumsum(data_last_decade$log_diff_rngdi)  # Actual cumulative growth
rngdi_tot <- cumsum(counterfactual_tot_pred$fcst$log_diff_rngdi[, 1])  # Growth due to GDP only
rngdi_gdp <- cumsum(counterfactual_gdp_pred$fcst$log_diff_rngdi[, 1])  # Growth due to Terms of Trade only

# Combine results into a single data table for plotting
results <- data.table(
  date = data_last_decade$date,
  actual = rngdi_actual,
  gdp_only = rngdi_tot,
  tot_only = rngdi_gdp
)

# Plot the decomposition
ggplot(results, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual RNGDI Growth")) +
  geom_line(aes(y = gdp_only, color = "Due to GDP per Capita")) +
  geom_line(aes(y = tot_only, color = "Due to Terms of Trade")) +
  labs(
    title = "Decomposition of RNGDI Growth (2013–2023)",
    x = "Year",
    y = "Cumulative Growth",
    color = "Component"
  ) +
  theme_minimal()
