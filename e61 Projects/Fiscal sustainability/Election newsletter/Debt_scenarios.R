## Last update:  27/05/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Simple debt scenario for internal note

library(cli)
library(tidyverse)
library(data.table)
library(theme61)
library(tidyr)
library(readxl)
library(gghighlight)
library(readabs)
library(OECD)
library(jsonlite)
library(httr)
library(Synth)
library(mFilter)

rm(list=ls())
gc()

debt_dt <- read_excel("Debt scenario2.xlsx")

setDT(debt_dt)

debt_long_rev <- melt(debt_dt[,.(Year,Forecast,Mig_Prod_TOT,Mig_Prod)], id.vars = "Year",
                          variable.name = "Type",
                          value.name = "value")

ggplot(debt_long_rev,aes(x=Year,y=value,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(30,70)) +
  plab(c("Forecast","Productivity and TOT shock","Lower Productivity"),x=c(2024,2024,2024),y=c(66,58,62)) +
  labs_e61(#title = "Gross debt projections",
           #subtitle = "% of GDP",
           y="% GDP",
           x="",
           sources = c("PBO","e61"))

ggplot(debt_long_rev,aes(x=Year,y=value,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(30,70)) +
  plab(c("Forecast","Productivity and TOT shock","Lower Productivity"),x=c(2024,2024,2024),y=c(66,58,62)) +
  labs_e61(title = "Gross debt projections (Revenue shocks)",
    subtitle = "% of nominal GDP",
    y="%",
    x="",
    sources = c("PBO","e61"),
    footnotes = c("Scenario reduces net migration and productivity, alongside lower real wage growth to match. Terms of trade shock reflects a 45% decline in key export prices."))

save_e61("Projections_debt_rev.png",res=2)

## Do expenses

debt_long_exp <- melt(debt_dt[,.(Year,Forecast,Defence,Defence_NDIS,Defence_interest_NDIS)], id.vars = "Year",
                      variable.name = "Type",
                      value.name = "value")


ggplot(debt_long_exp,aes(x=Year,y=value,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(30,70)) +
  plab(c("Forecast","Defence spending","+ NDIS","+ Interest increase"),x=c(2024,2024,2024,2024),y=c(66,62,58,55)) +
  labs_e61(title = "Gross debt projections (expenditure shock)",
    subtitle = "% of nominal GDP",
    y="% GDP",
    x="",
    sources = c("PBO","e61"))

save_e61("Projections_debt_exp.png",res=2)

## Overall

debt_long <- melt(debt_dt[,.(Year,Forecast,Mig_Prod_TOT,Defence_interest_NDIS,Expenditure_Revenue)], id.vars = "Year",
                      variable.name = "Type",
                      value.name = "value")


ggplot(debt_long,aes(x=Year,y=value,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(30,90)) +
  plab(c("Forecast","Expenditure Shock","Revenue Shock","Both shocks"),x=c(2024,2024,2024,2024),y=c(82,77,72,67)) +
  labs_e61(title = "Gross debt projections",
           subtitle = "% of nominal GDP",
           y="%",
           x="",
           sources = c("PBO","e61"),
           )

save_e61("Projections_debt.png",res=2)

## Add the 2015 EATR and EMTR for different business investments


