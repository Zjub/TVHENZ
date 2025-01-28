# Quick info on replacement rates for the newsletter
# Author: Matt Nolan
# Date made: 20/01/2025
# Last update: 22/01/2025

library(tidyverse)
library(data.table)
library(theme61)
library(readxl)
library(fixest)

## RR including housing assistance
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


## RR excluding housing assistance

RR_xh <- read.csv("OECD_RR_xhousing.csv")
setDT(RR_xh)

RR_xh_dt <- unique(RR_xh[,.(REF_AREA,TIME_PERIOD,OBS_VALUE)][order(TIME_PERIOD)])
colnames(RR_xh_dt) <- c("REF_AREA","TIME_PERIOD","RR")

# This looks like a data error for NZ in 2020 given it is higher than the + housing assistance rate ...
ggplot(RR_xh_dt[TIME_PERIOD >= 2014],aes(x=TIME_PERIOD,y=RR,colour=REF_AREA)) + geom_line() +
  scale_y_continuous_e61(limits = c(18,59,by=3),y_top = TRUE) +
  labs_e61(title = "Income Replacement Rate","Relative to Average Wage, Single ex Housing Assistance",y="%") +
  scale_x_continuous(breaks = seq(min(RR_dt$TIME_PERIOD), max(RR_dt$TIME_PERIOD), by = 2)) +
  plab(c("Australia","New Zealand"),y=c(20.5,29.5),x=c(2017,2017))

save_e61("RR_NZ_AUS_xhousing.png",res=2,pad_width = 1)

# Adjust to move the same way as inc housing RR for 2020, given the housing amount was unchanged - also follow up with the OECD on the data.

impute_factor <- RR_dt[REF_AREA == "NZL" & TIME_PERIOD == 2020,.(RR)]/RR_dt[REF_AREA == "NZL" & TIME_PERIOD == 2021,.(RR)]
NZ_21 <- RR_xh_dt[REF_AREA == "NZL" & TIME_PERIOD == 2021,.(RR)]

RR_xh_dt[REF_AREA == "NZL" & TIME_PERIOD == 2020]$RR <- as.integer(NZ_21*impute_factor)
RR_xh_dt[,REF_AREA := factor(REF_AREA,levels = c("NZL","AUS"))]

ggplot(RR_xh_dt[TIME_PERIOD >= 2014],aes(x=TIME_PERIOD,y=RR,colour=REF_AREA)) + geom_line() +
  scale_y_continuous_e61(limits = c(18,34,by=3),y_top = TRUE) +
  labs_e61(title = "Income Replacement Rate","Relative to Average Wage, Single ex Housing Assistance",y="%",footnotes = c("NZ 2020 figure imputed.","Excludes Coronavirus Supplement"),sources = c("e61","OECD")) +
  scale_x_continuous(breaks = seq(min(RR_dt$TIME_PERIOD), max(RR_dt$TIME_PERIOD), by = 2)) +
  plab(c("New Zealand","Australia"),y=c(31,28),x=c(2017,2017))

save_e61("RR_NZ_AUS_xhousing_impute2020.svg")

save_e61("RR_NZ_AUS_xhousing_impute2020.png",res=2,pad_width = 1)



### Do the absolute comparisons with actual PPPs and benefit rates over time - the implied graph looks great, but given the income data isn't consistent this isn't going to be accurate, meaning we have the wrong denominator from this data. We can pull the information from Australia easily, the NZ data will need to be manually entered.

NZ_AUS_compare <- read_excel("Benefit_comparison.xlsx", 
                                 sheet = "forR")
setDT(NZ_AUS_compare)
full_ben <- melt(NZ_AUS_compare[,.(Apr_year,NZ_PPP,AUS_PPP)],id.vars = "Apr_year")
xh_ben <- melt(NZ_AUS_compare[,.(Apr_year,NZ_PPP_xhouse,AUS_PPP_xhouse)],id.vars = "Apr_year")

ggplot(full_ben,aes(x=as.numeric(Apr_year),y=value,fill=variable)) + geom_col(position = "dodge")

ggplot(full_ben[Apr_year>= 2019],aes(x=as.numeric(Apr_year),y=value,fill=variable)) + geom_col(position = "dodge") +
  labs_e61(title = "Relative benefit payment",subtitle = "Fortnightly PPP adjusted, Including maximum Housing Assistance*",
           footnotes = c("The maximum rate of housing assistance in New Zealand is higher than in New Zealand by the difference between the two payments. However, the rate is only paid in limited geographic zones. For most NZ recipients a rate similar to the CRA is provided."),
           x="",
           y="",
           sources = c("e61","Service Australia","MSD")) +
  scale_y_continuous_e61(labels = scales::dollar_format(),limits = c(0,1200,300)) +
  plab(c("New Zealand","Australia"),x=c(2021,2021),y=c(800,1000)) +
  scale_x_continuous(breaks = seq(min(xh_ben$Apr_year), max(xh_ben$Apr_year), by = 1))

save_e61("rel_benefits.png",pad_width = 1,res=2)

  
ggplot(xh_ben[Apr_year>= 2019],aes(x=as.numeric(Apr_year),y=value,fill=variable)) + geom_col(position = "dodge") +
  labs_e61(title = "Relative benefit payment",subtitle = "Fortnightly PPP adjusted, Excluding Housing Assistance*",
           footnotes = c("The maximum rate of housing assistance in New Zealand is higher than in New Zealand by the difference between the two payments. However, the rate is only paid in limited geographic zones. For most NZ recipients a rate similar to the CRA is provided."),
           x="",
           y="",
           sources = c("e61","Service Australia","MSD")) +
  scale_y_continuous_e61(labels = scales::dollar_format(),limits = c(0,1200,300)) +
  plab(c("New Zealand","Australia"),x=c(2021,2021),y=c(800,1000)) +
  scale_x_continuous(breaks = seq(min(xh_ben$Apr_year), max(xh_ben$Apr_year), by = 1))

save_e61("rel_benefits_xhousing.png",pad_width = 1,res=2)


xh_xCS_ben <- melt(NZ_AUS_compare[,.(Apr_year,NZ_PPP_xhouse,AUS_PPP_xh_xCS)],id.vars = "Apr_year")

ggplot(xh_xCS_ben[Apr_year>= 2019],aes(x=as.numeric(Apr_year),y=value,fill=variable)) + geom_col(position = "dodge") +
  labs_e61(title = "Relative benefit payment",subtitle = "Fortnightly PPP adjusted, Excluding Housing Assistance and Coronavirus Support*",
           footnotes = c("The maximum rate of housing assistance in New Zealand is higher than in New Zealand by the difference between the two payments. However, the rate is only paid in limited geographic zones. For most NZ recipients a rate similar to the CRA is provided."),
           x="",
           y="",
           sources = c("e61","Service Australia","MSD")) +
  scale_y_continuous_e61(labels = scales::dollar_format(),limits = c(0,800,200)) +
  plab(c("New Zealand","Australia"),x=c(2021,2021),y=c(550,700)) +
  scale_x_continuous(breaks = seq(min(xh_ben$Apr_year), max(xh_ben$Apr_year), by = 1))

save_e61("rel_benefits_xhousing_xCS.svg")

save_e61("rel_benefits_xhousing_xCS.png",pad_width = 1,res=2)
