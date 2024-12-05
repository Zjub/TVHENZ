###

rm(list = ls())
gc()

# Load required libraries
library(tidyverse)
library(data.table)
library(vars)       # For VAR models
library(tseries)    # For stationarity tests
library(ggplot2)

# Load and prepare GDP data
a <- read_abs("5206.0")
setDT(a)

filtered_a <- a %>%
  filter(str_detect(series, regex("per capita", ignore_case = TRUE)))

# Filter for the required series
data_dt <- a[
  date >= as.Date("1980-01-01") & 
    series %in% c(
      "GDP per capita: Chain volume measures ;",
      "GDP per hour worked: Index ;",
      "Terms of trade: Index ;",
      "Real net national disposable income per capita: Chain volume measures ;"
    ) & 
    series_type == "Seasonally Adjusted"
]

reference_values <- data_dt[date == as.Date("2014-03-01"), .(reference_value = value), by = series]

# Merge reference values back to the main data
data_dt <- merge(data_dt, reference_values, by = "series", all.x = TRUE)

# Scale each series to 1 in January 2014
data_dt[, scaled_value := value / reference_value]

# Remove the reference_value column (optional, for cleanliness)
data_dt[, reference_value := NULL]

ggplot(data_dt,aes(x=date,y=scaled_value,colour=series)) + geom_line() +
  theme_e61(legend = "bottom")

ggplot(data_dt[date >= as.Date("2014-03-01")],aes(x=date,y=scaled_value,colour=series,linetype=series)) + 
  geom_line() +
  #theme_e61(legend = "bottom") + 
  scale_linetype_manual(values=c("solid","solid","solid","dotted")) +
  labs_e61(title = "Relative to 2014",y="") + 
  plab(c("GDP per capita","GDP per hour","RNNDI per capita","TOT"),x=rep("2014-03-01",times=4),y=c(1.38,1.33,1.28,1.23)) + geom_hline(yintercept = 1)

save_e61("GDP Income measures.png",res=2,pad_width = 1)




# Calculate log differences to ensure stationarity
data_dt[, log_diff := log(value) - shift(log(value)), by = series]

# Remove rows with NA log_diff
data_dt <- data_dt[!is.na(log_diff)]

# Reshape data to wide format for modeling
data_wide <- dcast(data_dt, date ~ series, value.var = "log_diff")

# Rename columns for convenience
setnames(data_wide, 
         c("date", 
           "GDP per capita ;", 
           "GDP per hour worked: Index ;", 
           "Terms of trade: Index ;", 
           "Real net national disposable income per capita: Chain volume measures ;"), 
         c("date", "log_diff_gdp", "log_diff_tot", "log_diff_rngdi"))

# Remove NA rows from lagging
data_wide <- na.omit(data_wide)

# Test for stationarity using Augmented Dickey-Fuller Test
adf_test_gdp <- adf.test(data_wide$log_diff_gdp, alternative = "stationary")
adf_test_tot <- adf.test(data_wide$log_diff_tot, alternative = "stationary")
adf_test_rngdi <- adf.test(data_wide$log_diff_rngdi, alternative = "stationary")

print("ADF Test Results for GDP:")
print(adf_test_gdp)

print("ADF Test Results for Terms of Trade:")
print(adf_test_tot)

print("ADF Test Results for RNGDI:")
print(adf_test_rngdi)

# Determine optimal number of lags
lag_selection <- VARselect(data_wide[, .(log_diff_rngdi, log_diff_gdp, log_diff_tot)], lag.max = 10, type = "const")
print("Optimal Lags:")
print(lag_selection$selection)

# Fit the VAR model (using optimal lags)
var_model <- VAR(data_wide[, .(log_diff_rngdi, log_diff_gdp, log_diff_tot)], 
                 p = lag_selection$selection["AIC(n)"], 
                 type = "const")
summary(var_model)

# Granger causality test: Does Terms of Trade influence RNGDI?
granger_test_tot <- causality(var_model, cause = "log_diff_tot")
print("Granger Causality Test (Terms of Trade -> RNGDI):")
print(granger_test_tot)

# Granger causality test: Does GDP influence RNGDI?
granger_test_gdp <- causality(var_model, cause = "log_diff_gdp")
print("Granger Causality Test (GDP -> RNGDI):")
print(granger_test_gdp)

# Impulse Response Function Analysis: How do shocks in GDP and Terms of Trade affect RNGDI?
irf_rngdi_tot <- irf(var_model, impulse = "log_diff_tot", response = "log_diff_rngdi", n.ahead = 10)
irf_rngdi_gdp <- irf(var_model, impulse = "log_diff_gdp", response = "log_diff_rngdi", n.ahead = 10)

# Plot Impulse Response Functions
plot(irf_rngdi_tot, main = "IRF: Terms of Trade -> RNGDI")
plot(irf_rngdi_gdp, main = "IRF: GDP -> RNGDI")

# Forecast future values for RNGDI
forecast <- predict(var_model, n.ahead = 10, ci = 0.95)

# Plot Forecast with Fan Chart
fanchart(forecast)

# Model Diagnostics
# Check for serial correlation in residuals
serial_test <- serial.test(var_model, lags.bg = 10, type = "BG")
print("Serial Correlation Test:")
print(serial_test)

# Check model stability
stability <- stability(var_model)
plot(stability)

# End of Script
