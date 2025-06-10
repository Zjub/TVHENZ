## Last update:  10/06/2025
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

debt_long_rev <- melt(debt_dt[,.(Year,Forecast,Mig_Prod_TOT,Mig_Prod,Mig)], id.vars = "Year",
                          variable.name = "Type",
                          value.name = "value")

debt_long_rev$Type <- factor(debt_long_rev$Type,levels = c("Forecast","Mig","Mig_Prod","Mig_Prod_TOT"))

ggplot(debt_long_rev,aes(x=Year,y=value,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(30,70)) +
  plab(c("Forecast","Lower Net Migration","+ Lower Productivity","+ TOT shock"),x=c(2024,2024,2024,2024),y=c(66,62,58,54)) +
  labs_e61(#title = "Gross debt projections",
           #subtitle = "% of GDP",
           y="% GDP",
           x="",
           sources = c("PBO","e61"))

ggplot(debt_long_rev,aes(x=Year,y=value,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(30,70)) +
  plab(c("Forecast","Lower Net Migration","+ Lower Productivity","+ Lower Export Prices"),x=c(2024,2024,2024,2024),y=c(66,62,58,54)) +
  labs_e61(title = "Gross debt projections (Revenue shocks)",
    subtitle = "% of nominal GDP",
    y="%",
    x="",
    sources = c("PBO","e61"),
    footnotes = c("Scenario reduces net migration and productivity, alongside lower real wage growth to match. Terms of trade shock reflects a 45% decline in key export prices."))

save_e61("Projections_debt_rev.png",res=2)

ggplot(debt_long_rev,aes(x=Year,y=value,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(30,70)) +
  plab(c("Forecast","Lower Net Migration","+ Lower Productivity","+ Lower Export Prices"),x=c(2024,2024,2024,2024),y=c(66,62,58,54)) +
  labs_e61(#title = "Gross debt projections (Revenue shocks)",
           subtitle = "% of nominal GDP",
           y="%",
           x="",
           sources = c("PBO","e61"),
           footnotes = c("Scenario reduces net migration and productivity, alongside lower real wage growth to match. Terms of trade shock reflects a 45% decline in key export prices."))

save_e61("Projections_debt_rev.pdf")

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
  plab(c("Forecast","Revenue Only Shock","Expenditure Only Shock","Both shocks"),x=c(2024,2024,2024,2024),y=c(82,77,72,67)) +
  labs_e61(#title = "Gross debt projections",
           subtitle = "% of nominal GDP",
           y="%",
           x="",
           sources = c("PBO","e61"),
           footnotes = c("Expenditure shock includes higher defence, NDIS, and interest spending. Revenue shock includes decline in net migration, lower productivity, and a decline in export prices.")
           )

save_e61("Projections_debt.png",res=2)
save_e61("Projections_debt.pdf")

## PBO bracket creep and deficits estimates

PBO_dt <- read_excel("Debt scenario2.xlsx",sheet = "PBO_deficit")
setDT(PBO_dt)

PBO_long <- melt(PBO_dt[,.(FY,Baseline,B_BC,B_total)],id.vars = "FY",variable.name = "Type",value.name = "value")

ggplot(PBO_long,aes(x=FY,y=value,colour=Type)) + geom_line() + geom_hline(yintercept = 0) +
  labs_e61(subtitle = "Deficit % GDP, Financial Year",y="%",x="") +
  scale_y_continuous_e61(limits = c(-4,1,1)) +
  scale_x_continuous_e61(expand_left = 0,expand_right = 0,limits=c(24,35,1)) +
  plab(c("Baseline","Remove Bracket Creep","+ Spending Allowances"),x=c(25.8,29.2,25),y=c(-0.4,-1.4,-3.5))

save_e61("Bracket_creep_deficit.pdf")
save_e61("Bracket_creep_deficit.png",res=2)

## Budget paper forecast error

forecast_error <- read_excel("bp1-bs7.xlsx", sheet = "7.08",
                             skip = 1)

setDT(forecast_error)

colnames(forecast_error) <- c("FY","UCB","Receipt_Error","Payment_Error")

error_long <- melt(forecast_error[,.(FY,Receipt_Error,Payment_Error)],id.vars = "FY",variable.name = "Type",value.name = "value")

ggplot(error_long,aes(x=FY,y=value,fill=Type)) + geom_col(position="dodge")


## Revenue scenario



