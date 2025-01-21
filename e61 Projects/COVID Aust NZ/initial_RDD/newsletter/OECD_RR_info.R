# Quick info on replacement rates for the newsletter
# Author: Matt Nolan
# Date made: 20/01/2025
# Last update: 20/01/2025

library(tidyverse)
library(data.table)
library(theme61)
library(readxl)
library(fixest)

RR <- read.csv("OECD_RR.csv")
setDT(RR)

RR_dt <- unique(RR[,.(REF_AREA,TIME_PERIOD,OBS_VALUE)][order(TIME_PERIOD)])
colnames(RR_dt) <- c("REF_AREA","TIME_PERIOD","RR")

ggplot(RR_dt[REF_AREA == "AUS"],aes(x=TIME_PERIOD,y=RR)) + geom_line() +
  scale_y_continuous_e61(limits = c(26,38,by=3),y_top = TRUE) +
  labs_e61(title = "Income Replacement Rate","Relative to Average Wage, Single",y="%") +
  scale_x_continuous(breaks = seq(min(RR_dt$TIME_PERIOD), max(RR_dt$TIME_PERIOD), by = 4))

save_e61("RR_OECD_AUS.png",res=2,pad_width = 1)

ggplot(RR_dt,aes(x=TIME_PERIOD,y=RR,colour=REF_AREA)) + geom_line() +
  scale_y_continuous_e61(limits = c(26,50,by=3),y_top = TRUE) +
  labs_e61(title = "Income Replacement Rate","Relative to Average Wage, Single",y="%") +
  scale_x_continuous(breaks = seq(min(RR_dt$TIME_PERIOD), max(RR_dt$TIME_PERIOD), by = 4)) +
  plab(c("Australia","New Zealand"),y=c(30.5,42.5),x=c(2013,2013))


ggplot(RR_dt[TIME_PERIOD >= 2014],aes(x=TIME_PERIOD,y=RR,colour=REF_AREA)) + geom_line() +
  scale_y_continuous_e61(limits = c(26,44,by=3),y_top = TRUE) +
  labs_e61(title = "Income Replacement Rate","Relative to Average Wage, Single",y="%") +
  scale_x_continuous(breaks = seq(min(RR_dt$TIME_PERIOD), max(RR_dt$TIME_PERIOD), by = 2)) +
  plab(c("Australia","New Zealand"),y=c(30.5,39.5),x=c(2017,2017))

save_e61("RR_NZ_AUS.png",res=2,pad_width = 1)

AAW <- read.csv("OECD_NA_AAW.csv") # The National Accounts imputed measure the OECD uses.
setDT(AAW)

AAW_dt <- unique(AAW[,.(REF_AREA,TIME_PERIOD,OBS_VALUE)][order(TIME_PERIOD)])
colnames(AAW_dt) <- c("REF_AREA","TIME_PERIOD","AAW")

dt <- AAW_dt[RR_dt,on=.(REF_AREA,TIME_PERIOD)][,ben_PPP := AAW*(RR/100)]

ggplot(dt[TIME_PERIOD > 2015],aes(x=TIME_PERIOD,y=ben_PPP/1000,fill=REF_AREA)) + 
  geom_col(position = "dodge") +
  scale_y_continuous_e61(limits = c(0,26,2)) +
  labs_e61(title = "Benefit payments now higher in New Zealand?",y="") +
  scale_x_continuous(breaks = seq(min(dt$TIME_PERIOD), max(dt$TIME_PERIOD), by = 1))

### Do this with actual PPPs and benefit rates over time - the graph looks great, but when I compare to current benefit levels at PPPs it is very wrong, meaning we have the wrong denominator from this data. We can pull the information from Australia easily, the NZ data will need to be manually entered.

NZ_AUS_compare <- read_excel("Benefit_comparison.xlsx", 
                                 sheet = "forR")
setDT(NZ_AUS_compare)
full_ben <- melt(NZ_AUS_compare[,.(Apr_year,NZ_PPP,AUS_PPP)],id.vars = "Apr_year")
xh_ben <- melt(NZ_AUS_compare[,.(Apr_year,NZ_PPP_xhouse,AUS_PPP_xhouse)],id.vars = "Apr_year")

ggplot(full_ben,aes(x=as.numeric(Apr_year),y=value,fill=variable)) + geom_col(position = "dodge")
  
ggplot(xh_ben,aes(x=as.numeric(Apr_year),y=value,fill=variable)) + geom_col(position = "dodge") +
  labs_e61(title = "Relative benefit payment",subtitle = "Excluding Housing Assistance*",footnote = c("The maximum rate of housing assistance in New Zealand is higher than in New Zealand by the difference between the two payments. However, the rate is only paid is limited geographic zones. For most NZ recipients a rate similar to the CRA is provided."))
