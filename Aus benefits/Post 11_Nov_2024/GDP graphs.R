rm(list=ls())
gc()

library(tidyverse)
library(data.table)
library(theme61)
library(tidyr)
library(readxl)
library(gghighlight)
library(readabs)
library(seasonal)

# GDP data
a <- read_abs("5206.0")
setDT(a)

# Filter data for specific series and date range
data_dt <- a[
  date >= as.Date("1980-01-01") & 
    series %in% c("GDP per hour worked: Index ;", "Terms of trade: Index ;") & 
    series_type == "Seasonally Adjusted"
]

# Calculate log differences
data_dt[, log_diff := log(value) - shift(log(value)), by = series]

# Remove rows with NA in log_diff
data_dt <- data_dt[!is.na(log_diff)]

# Wide format to run regression
data_wide <- dcast(data_dt, date ~ series, value.var = "log_diff")

# Rename columns for simplicity
setnames(data_wide, c("date", "GDP per hour worked: Index ;", "Terms of trade: Index ;"), 
         c("date", "log_diff_gdp", "log_diff_tot"))

# Run a linear regression: log_diff_tot as predictor of log_diff_gdp
model <- lm(log_diff_gdp ~ log_diff_tot, data = data_wide)

# Summary of the model
summary(model)

# Extract residuals
data_wide$residuals <- resid(model)

# Plot residuals over time
ggplot(data_wide, aes(x = date, y = residuals)) +
  geom_line() +
  labs(title = "Residuals Over Time",
       x = "Date",
       y = "Residuals") +
  theme_minimal()

# Histogram of residuals
ggplot(data_wide, aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()
