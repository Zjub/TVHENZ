
rm(list=ls())

.libPaths(new = 'C:/Rpackage')

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


Hours <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Original") %>% filter(series == "Hours worked: Index ;") %>% filter(grepl('Table 1',table_title)) %>% drop_na %>% filter(unit == "Index Numbers") %>% select("date","value")

colnames(Hours) <- c("date","Hours")



GDP <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Original") %>% filter(series == "Gross domestic product: Chain volume measures ;") %>% filter(table_title == "Table 1. Key National Accounts Aggregates") %>% select("date","series","value")

LS <- read_abs(cat_no = "6202.0") %>% filter(series_type == "Original") %>% filter(series == "Employed total ;  Persons ;" | series == "Unemployed total ;  Persons ;"  | series == "Unemployment rate ;  Persons ;") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original") %>% select("date","series","value")