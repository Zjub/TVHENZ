rm(list=ls())

library(remotes)
library(tidyverse)
library(data.table)
library(collapse)
library(readabs)
library(readr)
library(readxl)
library(theme61)
library(lubridate)
library(mFilter)
library(zoo)
library(Hmisc)
library(seasonal)

## Macro aggregates ----

Hours <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(series == "Hours worked: Index ;") %>% filter(grepl('Table 1',table_title)) %>% drop_na %>% filter(unit == "Index Numbers") %>% select("date","value")

colnames(Hours) <- c("date","Hours")

GDP <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(series == "Gross domestic product: Chain volume measures ;") %>% filter(table_title == "Table 1. Key National Accounts Aggregates") %>% select("date","series","value")

LS <- read_abs(cat_no = "6202.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original") %>% filter(series == "Employed total ;  Persons ;" | series == "Unemployed total ;  Persons ;"  | series == "Unemployment rate ;  Persons ;") %>% select("date","series","value")

LS2 <- pivot_wider(LS,id_cols = "date",names_from = "series",values_from = "value")
colnames(LS2) <- c("date","Employed","Unemployed","UR_ABS")

LS2 <- LS2 %>% mutate(UR_Derived = Unemployed/(Employed + Unemployed),QUnemp = rollmean(Unemployed,k=3,fill=NA,align="right"),QEmp =rollmean(Employed,k=3,fill=NA,align="right"),UR = UR_ABS) 

LSQ <- LS2[seq(5,nrow(LS2),3),] %>% select(date,QUnemp,QEmp,UR)

UR <- LSQ %>% select(date,UR) 


# Create the data (same as before)
LP <- left_join(LSQ, GDP, by = "date") %>%
  mutate(Labour_Prod = value / QEmp) %>%
  select(date, Labour_Prod) %>%
  drop_na()

LP$Labour_Prod <- LP$Labour_Prod / LP[LP$date == "1980-03-01", ]$Labour_Prod

LP_hour <- left_join(Hours, GDP, by = "date") %>%
  mutate(Hour_Labour_Prod = value / Hours) %>%
  select(date, Hour_Labour_Prod) %>%
  drop_na()

#LP_hour$Hour_Labour_Prod <- LP_hour$Hour_Labour_Prod / LP_hour[LP_hour$date == "1980-03-01", ]$Hour_Labour_Prod

# Add linear trends for specified periods
LP$Period <- cut(as.numeric(format(LP$date, "%Y")), 
                 breaks = c(1980, 1990, 2000, 2010, 2019),
                 labels = c("1980-1990", "1990-2000", "2000-2010", "2010-2019"),
                 include.lowest = TRUE)

# Fit linear models for each period
trends <- LP %>%
  group_by(Period) %>%
  filter(!is.na(Period)) %>%
  mutate(Trend = predict(lm(Labour_Prod ~ as.numeric(date), data = cur_data())))

# Combine with Hourly Productivity for plotting
LP_full <- left_join(trends, LP_hour, by = "date") %>%
  pivot_longer(!c(date, Period, Trend), names_to = "name", values_to = "value")

# Plot
ggplot(LP_full, aes(x = date, y = value, colour = name)) +
  geom_line() +
  geom_line(data = LP_full %>% filter(name == "Labour_Prod"),
            aes(x = date, y = Trend, group = Period, linetype = Period),
            colour = "black") + # Add linear trends
  labs_e61(
    title = "Labour Productivity",
    subtitle = "Index, 1980 = 1, HP smoothing parameter = 1600",
    y = "",
    x = ""
  ) +
  scale_y_continuous_e61(limits = c(0.9, 2, 0.2), y_top = FALSE)



####

exp_data <- LP %>%
  filter(date >= as.Date("2006-01-01") & date <= as.Date("2016-12-31"))

# Fit exponential model: Labour_Prod = a * exp(b * date)
exp_model <- nls(Labour_Prod ~ a * exp(b * as.numeric(date)), 
                 data = exp_data, 
                 start = list(a = 1, b = 0.00001))

# Extend the dates to project the trend
full_data <- LP %>%
  filter(date >= as.Date("2006-01-01")) # Filter only from 2000 onward

full_data$Exp_Trend <- predict(exp_model, newdata = data.frame(date = full_data$date))
setDT(full_data)

full_data[,Period := NULL]

graph_data <- melt(full_data,id.vars = "date")

# Plot
ggplot(graph_data, aes(x = date, y = value,colour=variable,linetype = variable)) +
  geom_line() +
  labs_e61(
    title = "Labour Productivity Gap Growing",
    subtitle = "Gap relative to 2006-2016 trend",
    y = "",
    x = ""
  ) +
  scale_y_continuous_e61(limits = c(1.2, max(full_data$Labour_Prod, full_data$Exp_Trend) * 1.1,0.1)) +
  scale_linetype_manual(values = c("solid","dashed")) + 
  geom_vline(xintercept = as.Date("2017-01-01"),linetype = "dotted",colour = palette_e61(3)[3]) +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y")

save_e61("LP_trend.png",res=2,pad_width = 1)

full_data[,Exp_Trend/Labour_Prod - 1]
