rm(list=ls())

library(remotes)
library(tidyverse)
library(data.table)
library(collapse)
library(readabs)
library(readrba)
library(readr)
library(readxl)
library(lubridate)
library(mFilter)
library(zoo)
library(Hmisc)
library(seasonal)


unemp_forecasts <- rba_forecasts() %>%
  filter(series == "unemp_rate")

unemp_forecasts %>%
  ggplot(aes(x = date, 
             y = value, 
             group = forecast_date, 
             col = forecast_date)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Unemployment rate (RBA forecasts)")

LS <- read_abs(cat_no = "6202.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original") %>% filter(series == "Employed total ;  Persons ;" | series == "Unemployed total ;  Persons ;"  | series == "Unemployment rate ;  Persons ;") %>% select("date","series","value")

LS2 <- pivot_wider(LS,id_cols = "date",names_from = "series",values_from = "value")
colnames(LS2) <- c("date","Employed","Unemployed","UR_ABS")
setDT(LS2)

setDT(unemp_forecasts)

date_start <- as.Date("2006-01-01")

UF <- unemp_forecasts[forecast_date >= date_start]

LS2 <- LS2[date >= date_start]

LS2 <- LS2[UF,on=.(date)]

ggplot(LS2,aes(x= date, y = value, group=forecast_date)) + geom_line() +
  geom_line(aes(y = UR_ABS), color = "red", size = 1.2) 
