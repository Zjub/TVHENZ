## Last update:  13/07/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Fiscal habit plot including consolidation

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

## Fiscal things (habits) - this is the Federal level example from internal report

pay_ngdp <- read_excel("Expenditure plots.xlsx",
                       sheet = "Sheet1", range = "A1:D27")
setDT(pay_ngdp)

colnames(pay_ngdp) <- c("FY","Payments","NGDP","GDPD")

# Change things to real
pay_ngdp[,RPayments := Payments/GDPD]
pay_ngdp[,RGDP := NGDP/GDPD]

base_year <- "2000"
base_row <- pay_ngdp[FY == base_year]
pay_ngdp[, `:=`(
  Payments_norm = RPayments / base_row$RPayments,
  RGDP_norm = RGDP / base_row$RGDP
)]

pay_ngdp[, Year := as.integer(FY)]
trend_data <- pay_ngdp[Year <= 2014 & Year != 2009]
trend_model <- lm(log(RGDP_norm) ~ Year, data = trend_data)
pay_ngdp[, RGDP_trend := exp(predict(trend_model, newdata = .SD))]

ggplot(pay_ngdp, aes(x = Year)) +
  geom_line(aes(y = Payments_norm, color = "Payments")) +
  geom_line(aes(y = RGDP_norm, color = "NGDP")) +
  geom_line(aes(y = RGDP_trend, color = "Pre-2014 NGDP Trend"), linetype = "dashed") +
  scale_color_manual(values = c("Payments" = palette_e61(3)[1], "NGDP" = palette_e61(3)[2], "Pre-2014 NGDP Trend" = "black")) +
  labs(y = "Indexed to FY2000 = 1", x = "Financial Year", color = "Series") +
  plab(c("Real Govt Payments","GDP","2000-2014 GDP trend"),x=c(2000.5,2000.5,2000.5),y=c(2.2,1.85,1.7),colour = c(palette_e61(3)[1],palette_e61(3)[2],"black")) +
  labs_e61(subtitle = "Deflated by GDPD, indexed to 1 in FY99/20",
           y="",
           x="") +
  scale_y_continuous_e61(limits = c(1,2.4,0.5))

## Bring in consolidated government spending


Consol_total <- read_excel("table22-consolidated-cofog-expenditure-spent-by-approach.xlsx", 
                         sheet = "COFOGexp", skip = 1)


setDT(Consol_total)

### Set up data ----

Aus_total <- Consol_total[Country == "Australia"][,':=' (Country = NULL,ISO = NULL, `COFOG Code` = NULL, `Government level code` = NULL,`1995` = NULL,`1996` = NULL,`1997` = NULL)]

Aus_total <- melt(Aus_total, id.vars = c(colnames(Aus_total)[1],colnames(Aus_total)[2]),variable.name = "Year",value.name = "value")

colnames(Aus_total)[1] <- "COFOG_Area"
colnames(Aus_total)[2] <- "Government_level"

Aus_total[, Government_level := fifelse(Government_level %in% c("Local", "State"), 
                                      "Non-Federal", "Federal")]

Aus_total <- Aus_total[, .(value = sum(value,na.rm=TRUE)), 
                   by = .(COFOG_Area, Government_level, Year)]

Consolidated_info <- Aus_total[COFOG_Area == "Total",.(Expenses = sum(value)),by=.(Year)][,Year := as.integer(as.character(Year))][pay_ngdp,on=.(Year)]


Consolidated_info[,RConsolidated := Expenses/GDPD]

base_year <- "2000"
base_row <- Consolidated_info[FY == base_year]
Consolidated_info[, `:=`(
  Payments_norm_con = RConsolidated / base_row$RConsolidated,
  nom_Payments_norm_con = Expenses / base_row$Expenses
)]

ggplot(Consolidated_info[!is.na(Payments_norm_con)], aes(x = Year)) +
  geom_line(aes(y = Payments_norm_con, color = "Payments")) +
  geom_line(aes(y = RGDP_norm, color = "NGDP")) +
  geom_line(aes(y = RGDP_trend, color = "Pre-2014 NGDP Trend"), linetype = "dashed") +
  scale_color_manual(values = c("Payments" = palette_e61(3)[1], "NGDP" = palette_e61(3)[2], "Pre-2014 NGDP Trend" = "black")) +
  plab(c("Real Govt Payments","GDP","2000-2014 GDP trend"),x=c(2000.5,2000.5,2000.5),y=c(2.1,1.85,1.7),colour = c(palette_e61(3)[1],palette_e61(3)[2],"black")) +
  labs_e61(title = "Spending follows old GDP trends",
           subtitle = "Deflated by GDPD, indexed to 1 in FY99/20",
           y="",
           x="",
           sources = c("OECD","e61")) +
  scale_y_continuous_e61(limits = c(1,2.2,0.5))

save_e61("Cons_spending_GDP.png",res=2)
save_e61("Cons_spending_GDP.svg")


Consolidated_info[!is.na(Payments_norm_con) & Year %in% c(2000,2014,2022)]
