###

# Add NGDP and fiscal impulse plots - although these are for the monetary-fiscal coordination presentation, not this post.

rm(list = ls())
gc()

# Load required libraries
library(tidyverse)
library(data.table)
library(readabs)
library(vars)       # For VAR models
library(tseries)    # For stationarity tests
library(ggplot2)
library(readrba)

# Load and prepare GDP data
a <- read_abs("5206.0")
setDT(a)

filtered_a <- a %>%
  filter(str_detect(series, regex("Current", ignore_case = TRUE)))

# Subset data for each period
data_1994_2008 <- a[series == "GDP per capita: Current prices ;" & 
                      series_type %in% c("Seasonally Adjusted") & 
                      date >= as.Date("1994-01-01") & 
                      date <= as.Date("2008-12-31")]

data_2009_2019 <- a[series == "GDP per capita: Current prices ;" & 
                      series_type %in% c("Seasonally Adjusted") & 
                      date >= as.Date("2009-01-01") & 
                      date <= as.Date("2019-12-31")]

# Fit linear models
lm_1994_2008 <- lm(log(value) ~ date, data = data_1994_2008)
lm_2009_2019 <- lm(log(value) ~ date, data = data_2009_2019)
lm_full <- lm(log(value) ~ date, data = a[series == "GDP per capita: Current prices ;" & 
                                            series_type %in% c("Seasonally Adjusted") & 
                                            date >= as.Date("1994-01-01")])

# Create projection data
future_dates <- data.table(date = seq(as.Date("2008-12-31"), as.Date("2024-12-31"), by = "1 month"))
future_dates[, `:=`(log_value_1994_2008 = predict(lm_1994_2008, newdata = .SD),
                    log_value_2009_2019 = predict(lm_2009_2019, newdata = .SD))]

# Combine future projections and original data
plot_data <- a[series == "GDP per capita: Current prices ;" & 
                 series_type %in% c("Seasonally Adjusted") & 
                 date >= as.Date("1994-01-01")]

# Define the full range of dates
full_dates <- data.table(date = seq(as.Date("1994-01-01"), as.Date("2024-12-31"), by = "1 month"))

# Generate predictions for each trend line over the full date range
full_dates[, `:=`(log_value_1994_2008 = predict(lm_1994_2008, newdata = .SD),
                  log_value_2009_2019 = predict(lm_2009_2019, newdata = .SD),
                  log_value_full = predict(lm_full, newdata = .SD))]

# Plot
ggplot(plot_data, aes(x = date, y = log(value))) +
  geom_line() +
  geom_line(data = full_dates, aes(x = date, y = log_value_1994_2008), color = "blue", linetype = "dashed", size = 0.8) +
  geom_line(data = future_dates, aes(x = date, y = log_value_2009_2019), color = "red", linetype = "dashed", size = 0.8) +
  #geom_line(data = full_dates, aes(x = date, y = log_value_full), color = "green", linetype = "solid", size = 0.8) +
  labs(title = "Log NGDP per capita",
       subtitle = "1994-2008, 2009-2019, and Full Data (1994-2024)",
       x = "Date",
       y = "Log(Value)") +
  theme_minimal() + geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dotted")


export_plot_data <- full_dates[plot_data,on=.(date)][,log_value := log(value)]

write.csv(export_plot_data,"NGDP_plot_data.csv")

### Next we want nominal government consumption to NGDP

filtered_a <- a %>%
  filter(str_detect(series, regex("Consumption", ignore_case = TRUE)))

unique(a[table_no =="5206003_expenditure_current_price"]$series)

exp <- a[series %in% c("GROSS DOMESTIC PRODUCT ;","General government ;  Final consumption expenditure ;","General government ;  Gross fixed capital formation ;" ) & series_type == "Seasonally Adjusted" & table_no =="5206003_expenditure_current_price"][,.(date,series,value)]

exp <- dcast(exp,date ~ series)
colnames(exp) <- c("date","GDP","Consumption","Investment")

exp[,':=' (Consumption_ratio = Consumption/GDP, Government_ratio = (Consumption + Investment)/GDP)]



ggplot(melt(exp[,.(date,Consumption_ratio,Government_ratio)],id.vars = "date")[date >= as.Date("1994-01-01")],aes(x=date,y=value,colour=variable)) + 
  geom_line() +
  scale_y_continuous(labels=scales::percent_format(),limits=c(0.15,0.3,0.05)) + 
  labs(title = "Government expenditure to NGDP") + theme_minimal() + geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dotted")

ggsave("Gov_spending.jpg")

cons_data_export <- melt(exp[,.(date,Consumption_ratio,Government_ratio)],id.vars = "date")[date >= as.Date("1994-01-01")]

write.csv(cons_data_export,"government_spending.csv")
write.csv(exp,"government_spending_wide.csv")


#### Now add CPI outcomes

b <- read_abs("6401.0")
setDT(b)

CPI <- b[series == "Index Numbers ;  All groups CPI ;  Australia ;" ][date >= as.Date("2001-01-01")][,.(date,value,type="CPI")]

start_value <- CPI[date == as.Date("2001-03-01"), value]
end_value <- CPI[date == as.Date("2008-12-01"), value]
years_diff <- as.numeric(difftime(as.Date("2008-12-01"), as.Date("2001-03-01"), units = "days")) / 365
annual_growth_rate <- (end_value / start_value)^(1 / years_diff) - 1

# Add the trend line using the calculated growth rate
CPI[, trend := value[1] * (1 + annual_growth_rate) ^ as.numeric(difftime(date, date[1], units = "days") / 365)]

# Extend the data to include dates after 2008
CPI <- b[series == "Index Numbers ;  All groups CPI ;  Australia ;" &
           date >= as.Date("2001-01-01")][, .(date, value, type = "CPI")]

# Add the trend line using the calculated growth rate for the entire date range
CPI[, trend := value[1] * (1 + annual_growth_rate) ^ as.numeric(difftime(date, date[1], units = "days") / 365)]

# Plot the original line and the trend line
ggplot(CPI, aes(x = date, y = value)) +
  geom_line(color = "black", size = 0.8) + # Original CPI line
  geom_line(aes(y = trend), color = "blue", linetype = "dashed", size = 0.8) + # Trend line
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dotted") + 
  labs(title = "CPI with Trend Line Based on 2001-2008 Growth Rate",
       subtitle = paste0("Annual Growth Rate: ", round(annual_growth_rate * 100, 2), "%"),
       x = "Date",
       y = "CPI Value") +
  theme_minimal()

write.csv(CPI,"CPI.csv")

CPI_full <- b[series == "Index Numbers ;  All groups CPI ;  Australia ;" ][,.(date,value,type="CPI")]
CPI_growth <- CPI_full[,.(date,value = (value/shift(value,4) - 1)*100,type)]

### Pull in some RBA information

forecasts <- rba_forecasts()
setDT(forecasts)

unique(forecasts$series_desc)

ggplot(forecasts[series_desc == "CPI - 4 quarter change" & date > (forecast_date + months(9)) & date <= (forecast_date + months(12))],aes(x=date,y=value)) + geom_line()

one_year_ahead_inf_fore <- forecasts[series_desc == "CPI - 4 quarter change" & date > (forecast_date + months(9)) & date <= (forecast_date + months(12))][,.(date,value,type="forecast")]


ggplot(rbind(CPI_growth, one_year_ahead_inf_fore)[date >= as.Date("1994-01-01")], aes(x = date, y = value, colour = type)) +
  geom_line() +
  # Add shaded region between y = 1 and y = 3
  geom_rect(aes(xmin = min(date), xmax = max(date), ymin = 1, ymax = 3),
            fill = "lightblue", alpha = 0.01, inherit.aes = FALSE) +
  # Add dotted horizontal lines at y = 1 and y = 3
  geom_hline(yintercept = 1, linetype = "dotted", color = "black") +
  geom_hline(yintercept = 3, linetype = "dotted", color = "black") +
  labs(title = "CPI Growth with Inflation Target Zone",
       x = "Date",
       y = "Value",
       colour = "Type") +
  theme_minimal()


rates <- rba_cashrate()
setDT(rates)

ggplot(rates[date >= as.Date("1994-01-01")],aes(x=date,y=value/100)) + geom_line() + scale_y_continuous(labels=scales::percent_format()) +
  labs(y="",title="Cash Rate") + theme_minimal()

write.csv(rates[date >= as.Date("1994-01-01")],"cash_rate.csv")
