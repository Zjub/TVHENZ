# Base code and graphics for the March 2022 CPI numbers

rm(list=ls()) # Clear prior memory

library(tidyverse)
library(readxl)
library(cowplot)
library(plotly)
library(reshape2)
library(ggpubr)
library(readxl)
library(viridis)

url <- "https://www.stats.govt.nz/assets/Uploads/Consumers-price-index/Consumers-price-index-December-2021-quarter/Download-data/consumers-price-index-december-2021-index-numbers.csv"

CPIdata <- read.csv(url) %>% mutate(date = as.numeric(substr(Period,1,4))) 