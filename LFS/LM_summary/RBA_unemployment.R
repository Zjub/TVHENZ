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
library(fixest)


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


GDP <- read_abs(cat_no = "5206.0")
setDT(GDP)
unique(GDP$series)

unique(GDP$table_title)
GDP_E_const <- GDP[table_title == "Table 2. Expenditure on Gross Domestic Product (GDP), Chain volume measures"]
GDP_E_curr <- GDP[table_title == "Table 3. Expenditure on Gross Domestic Product (GDP), Current prices"]


unique(GDP_E_const$series)
GDP_E_const[series %like% "Changes in inventories"]

graph_cons <- GDP_E_const[(series == "Changes in inventories ;" | series == "GROSS DOMESTIC PRODUCT ;") & series_type == "Seasonally Adjusted" & date >= as.Date("1980-01-01")][,.(date,series,value)]

graph_cons[, value_norm := value / first(value), by = series]

ggplot(graph_cons,aes(x=date,y=value_norm,colour=series)) + geom_line()

# Now detrend GDP.

library(mFilter)

hp_result <- hpfilter(graph_cons[series == "GROSS DOMESTIC PRODUCT ;"]$value, freq = 1600)

graph_cons[series == "GROSS DOMESTIC PRODUCT ;", detrended_value := hp_result$cycle]

graph_cons[,value_set := fifelse(series == "Changes in inventories ;", value_norm,
                                 detrended_value/1000)]

ggplot(graph_cons,aes(x=date,y=value_set,colour=series)) + geom_line() + labs(title = "Change in inventories vs detrended GDP") +
  scale_y_continuous(limits=c(-10,15)) # Missing big GDP drop on COVID due to limits


reg_cons <- dcast(graph_cons[,.(date,series, value_set)], formula = date ~ series)
colnames(reg_cons) <- c("date","CII","GDP")

feols(data=reg_cons,GDP ~ CII + date)

# Contributions

CII_contribution <- GDP_E_const[series %like% "Contribution" & series %like% "Change"][,.(date,CII_cont = value)]

CII_contribution <- CII_contribution[graph_cons,on=.(date)]
CII_contribution[,cont_plot := fifelse(series == "Changes in inventories ;", CII_cont,
                                       value_set/10)]

ggplot(CII_contribution,aes(x=date,y=cont_plot,colour=series)) + geom_line() +
  scale_y_continuous(limits = c(-5,5))

GDP[series %like% "Inventories"]

ggplot(GDP[series %like% "Inventories"],aes(x=date,y=value)) + geom_line()

GDP[series %like% "inventory"]
