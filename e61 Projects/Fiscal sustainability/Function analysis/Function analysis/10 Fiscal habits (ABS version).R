## Last update:  29/09/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Construct the "fiscal habits" plot up until 2024 using ABS data

## Setup ----

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
library(dendextend)
library(FactoMineR)
library(factoextra)
library(ggalluvial)
library(pheatmap)
library(gridExtra)
library(TSclust)
library(dtwclust)
library(moments)

rm(list=ls())
gc()

## Import data ----

work = TRUE
expense_only = TRUE
comp_year <- 1999

if (work == TRUE){
  consolidate_dt <- read_csv("C:/Users/MattNolan/Git/TVHENZ/e61 Projects/Fiscal sustainability/Function analysis/Data/abs_gfs_data_clean.csv")
} else{
  consolidate_dt <- read_csv("~/GitHub/TVHENZ/e61 Projects/Fiscal sustainability/Function analysis/Data/abs_gfs_data_clean.csv")
}
setDT(consolidate_dt)

if (expense_only == TRUE){
  consolidated_expenses_dt <- consolidate_dt[etf_type_name == "Revenue and expenses"]
} else {
  consolidated_expenses_dt <- consolidate_dt
}

dt <- consolidated_expenses_dt[fin_year <= 2024,.(nom_spend = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_div_name,fin_year)]

totals <- dt[, .(nom_spend = sum(nom_spend)), by = fin_year][
  , cofog_div_name := "Total"]


## Prior NGDP and deflator data

pay_ngdp <- read_excel("Expenditure plots.xlsx",
                       sheet = "Sheet1", range = "A1:D27")
setDT(pay_ngdp)

colnames(pay_ngdp) <- c("fin_year","Payments","NGDP","GDPD")

dt <- totals[pay_ngdp[fin_year <= 2024],on=.(fin_year)][,":=" (real_spend = nom_spend/GDPD, RGDP = NGDP/GDPD)]

base_year <- "2000"
base_row <- dt[fin_year == base_year]
dt[, `:=`(
  Payments_norm = real_spend / base_row$real_spend,
  RGDP_norm = RGDP / base_row$RGDP,
  Pay_norm_nom = nom_spend / base_row$nom_spend,
  NGDP_norm = NGDP / base_row$NGDP
)]

ggplot(melt(dt[fin_year <= 2024,.(fin_year,Payments_norm,RGDP_norm)],id.vars = "fin_year"),aes(x=fin_year,y=value,colour=variable)) + geom_line()

ggplot(melt(dt[fin_year <= 2024,.(fin_year,Pay_norm_nom,NGDP_norm)],id.vars = "fin_year"),aes(x=fin_year,y=value,colour=variable)) + geom_line()

trend_data <- dt[fin_year <= 2014 & fin_year != 2009]
trend_model <- lm(log(RGDP_norm) ~ fin_year, data = trend_data)
dt[, RGDP_trend := exp(predict(trend_model, newdata = .SD))]

ggplot(dt, aes(x = fin_year)) +
  geom_line(aes(y = Payments_norm, color = "Payments")) +
  geom_line(aes(y = RGDP_norm, color = "NGDP")) +
  geom_line(aes(y = RGDP_trend, color = "Pre-2014 NGDP Trend"), linetype = "dashed") +
  scale_color_manual(values = c("Payments" = palette_e61(3)[1], "NGDP" = palette_e61(3)[2], "Pre-2014 NGDP Trend" = "black")) +
  labs(y = "Indexed to FY2000 = 1", x = "Financial Year", color = "Series") +
  plab(c("Real Govt Payments","GDP","2000-2014 GDP trend"),x=c(2000.5,2000.5,2000.5),y=c(2.1,1.85,1.7),colour = c(palette_e61(3)[1],palette_e61(3)[2],"black")) +
  labs_e61(title = "Spending follows old GDP trends",
           subtitle = "Deflated by GDPD, indexed to 1 in FY99/20",
           y="",
           x="",
           sources = c("ABS","e61")) +
  scale_y_continuous_e61(limits = c(1,2.2,0.5))


dt[,.(fin_year,Payments_norm,RGDP_norm,RGDP_trend)]
