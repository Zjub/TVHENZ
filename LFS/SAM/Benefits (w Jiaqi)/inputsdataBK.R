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

ABS_DATA <- read.csv("ABS_DATA.csv",stringsAsFactors=FALSE)

read.zoo(ABS_DATA,format="%d/%m/%Y")

ABS_DATA <- ABS_DATA[c(2:179),c(1:11)] # Drop final quarter as the data is not for a complete quarter. First quarter also looks suspect so dropped, collection may have stared mid-quarter.

ABS_DATA[99,c("UE","EU","E","U")] <- NA # These did not appear to be complete quarters so removing

# ABS_DATA$date <- as.Date(paste("0",ABS_DATA$quarters,sep=""),format="%d/%m/%Y")
# 
# LF <- ts(ABS_DATA$labour_force, start = c(1978,1), frequency = 4)
# 
# LF_decomp <- bkfilter(ABS_DATA$labour_force,pl=5,pu=16)
# 
# plot.ts(LF_decomp$cycle, ylab = "")

# Goal is to get the standard deviation and cross-correlation of the cyclical components from using the BK filter, so need to apply this filter across the series and then save the cyclical series.

a<-lapply(ABS_DATA[,c(2:4)],bkfilter,pl=5,pu=32)

plot.ts(a$labour_force$cycle, ylab = "")

plot.ts(a$unemployed$cycle, ylab = "") # The unemployment rate is well above aggregates and the other figures are also quite different - especially the size of the labour force is very low, which will mean the flows are as well.  Would be good to check the definition of unemployment and see how that influences the relevant results.

b<-lapply(a,function(x)x[["cycle"]])

# Would be good to revisit these figures before confirming - but for now lets calculate the relevant moments. First start with the base commands sd and cor

std <- lapply(b,sd,na.rm=TRUE)

### Just reread the 2012 paper and realised that, for this paper, they did us an HP filter prior to calculating these amounts. So shifting to do that.




