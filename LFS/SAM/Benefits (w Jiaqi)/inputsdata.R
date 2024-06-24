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

# HWI is help wanted index.

ABS_DATA <- read.csv("ABS_DATA.csv",stringsAsFactors=FALSE)

read.zoo(ABS_DATA,format="%d/%m/%Y")

ABS_DATA$quarters <- as.Date(ABS_DATA$quarters,format="%d/%m/%Y")

ABS_DATA <- ABS_DATA[c(2:179),c(1:11)] # Drop final quarter as the data is not for a complete quarter. First quarter also looks suspect so dropped, collection may have stared mid-quarter.

ABS_DATA[99,c("UE","EU","E","U")] <- NA # These did not appear to be complete quarters so removing

# datestart = as.Date("1/03/2014",format="%d/%m/%Y")
# 
# dateend = as.Date("1/12/2019",format="%d/%m/%Y")
# 
# df <- ABS_DATA %>% filter(quarters >= datestart & quarters <= dateend)

a1<-lapply(ABS_DATA[,c(2:4)],hpfilter,freq=1600,type="lambda")

GDPstart <- as.Date("1/09/1985",format="%d/%m/%Y")

a2<-lapply(ABS_DATA %>% filter(ABS_DATA$quarters >= GDPstart) %>% select(GDP),hpfilter,freq=1600,type="lambda")

Transtart <- as.Date("1/03/2003",format="%d/%m/%Y")

a3<-lapply(ABS_DATA %>% filter(ABS_DATA$quarters >= Transtart) %>% select(UE,EU,E,U),hpfilter,freq=1600,type="lambda")

Hourstart <- as.Date("1/03/1991",format="%d/%m/%Y")

a4<-lapply(ABS_DATA %>% filter(ABS_DATA$quarters >= Hourstart) %>% select(HRS),hpfilter,freq=1600,type="lambda")

HWIstart <- as.Date("1/03/2006",format="%d/%m/%Y")

a5<-lapply(ABS_DATA %>% filter(ABS_DATA$quarters >= HWIstart) %>% select(HWI),hpfilter,freq=1600,type="lambda")

# Need to combine with this to get productivity: %>% mutate(Prod = ifelse(is.na(GDP),NA,ifelse(is.na(HRS),NA,GDP/HRS)))

plot.ts(a1$unemployed$cycle, ylab = "") # The unemployment rate is well above aggregates and the other figures are also quite different - especially the size of the labour force is very low, which will mean the flows are as well.  Would be good to check the definition of unemployment and see how that influences the relevant results.

b1 <-data.frame(quarters = ABS_DATA$quarters,sapply(a1,function(x)x[["cycle"]]))

b2 <-data.frame(date = ABS_DATA %>% filter(quarters >= GDPstart) %>% select(quarters),sapply(a2,function(x)x[["cycle"]]))

b3 <-data.frame(date = ABS_DATA %>% filter(quarters >= Transtart) %>% select(quarters),sapply(a3,function(x)x[["cycle"]]))

b4 <-data.frame(date = ABS_DATA %>% filter(quarters >= Hourstart) %>% select(quarters),sapply(a4,function(x)x[["cycle"]]))

b5 <-data.frame(date = ABS_DATA %>% filter(quarters >= HWIstart) %>% select(quarters),sapply(a5,function(x)x[["cycle"]]))

b <- left_join(b1,b2,by="quarters")
b <- left_join(b,b3,by="quarters")
b <- left_join(b,b4,by="quarters")
b <- left_join(b,b5,by="quarters")

datestart = as.Date("1/03/2014",format="%d/%m/%Y")
 
dateend = as.Date("1/12/2019",format="%d/%m/%Y")
 
dfb <- b %>% filter(quarters >= datestart & quarters <= dateend)

# Would be good to revisit these figures before confirming - but for now lets calculate the relevant moments. First start with the base commands sd and cor

std <- sapply(dfb,sd,na.rm=TRUE)

correlation <- cor(dfb[2:12])

elasticitytoP <- cov(dfb[2:12])/std[2:12]

ac <- acf(dfb[2:12],pl=FALSE)[1,]


