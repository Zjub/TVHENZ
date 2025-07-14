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

# ggplot(debt_long_rev,aes(x=Year,y=value,colour=Type)) + geom_line() +
#   scale_y_continuous_e61(limits = c(30,70)) +
#   plab(c("Forecast","Lower Net Migration","+ Lower Productivity","+ TOT shock"),x=c(2024,2024,2024,2024),y=c(66,62,58,54)) +
#   labs_e61(#title = "Gross debt projections",
#            #subtitle = "% of GDP",
#            y="% GDP",
#            x="",
#            sources = c("PBO","e61"))

ggplot(debt_long_rev,aes(x=Year,y=value,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(30,70)) +
  plab(c("Forecast","Lower Net Migration","+ Lower Productivity","+ Lower Export Prices"),x=c(2024,2024,2024,2024),y=c(66,62,58,54)) +
  labs_e61(title = "Gross debt projections (Revenue shocks)",
    subtitle = "% of nominal GDP",
    y="%",
    x="",
    sources = c("PBO","e61"),
    footnotes = c("Scenario reduces net migration by 35k and halves productivity growth to 0.6%pa, alongside lower real wage growth to match. Terms of trade shock reflects a 45% decline in key export prices."))

save_e61("Projections_debt_rev.png",res=2)

ggplot(debt_long_rev,aes(x=Year,y=value,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(30,70)) +
  scale_x_continuous_e61(limits = c(2024,2036,3),expand_left = 0.05,expand_right = 0.05,hide_first_last = FALSE) +
  plab(c("Forecast","Lower Net Migration","+ Lower Productivity","+ Lower Export Prices"),x=c(2024,2024,2024,2024),y=c(66,62,58,54)) +
  labs_e61(#title = "Gross debt projections (Revenue shocks)",
           subtitle = "% of nominal GDP",
           y="%",
           x="",
           sources = c("PBO","e61"),
           footnotes = c("Annual net migration declines by 35,000.","Productivity decline reduces trend growth from 1.2% to 0.6%.","Export price decline is a 45% drop from projections, to 2016 levels."))

save_e61("Projections_debt_rev.pdf")

## Do expenses

debt_long_exp <- melt(debt_dt[,.(Year,Forecast,Defence,Defence_NDIS,Defence_interest_NDIS)], id.vars = "Year",
                      variable.name = "Type",
                      value.name = "value")


ggplot(debt_long_exp,aes(x=Year,y=value,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(30,70)) +
  scale_x_continuous_e61(limits = c(2024,2036,3),expand_left = 0.05,expand_right = 0.05,hide_first_last = FALSE) +
  plab(c("Forecast","Defence spending","+ NDIS","+ Interest increase"),x=c(2024,2024,2024,2024),y=c(66,62,58,55)) +
  labs_e61(#title = "Gross debt projections (expenditure shock)",
    subtitle = "% of nominal GDP",
    y="% GDP",
    x="",
    sources = c("PBO","e61"),
    footnotes = c("Defence spending increased from 2.2% of GDP to 3.2%.","NDIS spending grows at 10%pa rather than 8%pa.","Ten-year goverment bond rate rises to 5.5%pa from a 4.5%pa projection."))

save_e61("Projections_debt_exp.pdf")

ggplot(debt_long_exp,aes(x=Year,y=value,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(30,70)) +
  scale_x_continuous_e61(limits = c(2024,2036,3),expand_left = 0.05,expand_right = 0.05,hide_first_last = FALSE) +
  plab(c("Forecast","Defence spending","+ NDIS","+ Interest increase"),x=c(2024,2024,2024,2024),y=c(66,62,58,55)) +
  labs_e61(title = "Gross debt projections (expenditure shock)",
    subtitle = "% of nominal GDP",
    x="",
    sources = c("PBO","e61"),
    footnotes = c("Defence spending increased from 2.2% of GDP to 3.2%.","NDIS spending grows at 10%pa rather than 8%pa.","Ten-year goverment bond rate rises to 5.5%pa from a 4.5%pa projection."))

save_e61("Projections_debt_exp.png",res=2)

## Overall

debt_long <- melt(debt_dt[,.(Year,Forecast,Mig_Prod_TOT,Defence_interest_NDIS,Expenditure_Revenue)], id.vars = "Year",
                      variable.name = "Type",
                      value.name = "value")


ggplot(debt_long,aes(x=Year,y=value,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(30,90)) +
  scale_x_continuous_e61(limits = c(2024,2036,3),expand_left = 0.05,expand_right = 0.05,hide_first_last = FALSE) +
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

ggplot(PBO_long,aes(x=FY+2000,y=value,colour=Type)) + geom_line() + geom_hline(yintercept = 0) +
  labs_e61(subtitle = "Deficit % GDP, Financial Year",y="%",x="",
           footnotes = c("Underlying Cash Balance as a % of Nominal GDP."),
           sources = c("PBO")) +
  scale_y_continuous_e61(limits = c(-4,1,1)) +
  scale_x_continuous_e61(expand_left = 0.05,expand_right = 0.05,limits=c(2024,2035,2),hide_first_last = FALSE) +
  plab(c("Baseline","Remove Bracket Creep","+ Spending Allowances"),x=c(2025.8,2029.2,2025),y=c(-0.4,-1.4,-3.5))

save_e61("Bracket_creep_deficit.pdf")
save_e61("Bracket_creep_deficit.png",res=2)

## Budget paper forecast error

forecast_error <- read_excel("bp1-bs7.xlsx", sheet = "7.08",
                             skip = 1)

setDT(forecast_error)

colnames(forecast_error) <- c("FY","UCB","Receipt_Error","Payment_Error")

forecast_error[, FY := as.character(as.numeric((str_extract(FY, "\\d{2}$")))+2000)]

error_long <- melt(forecast_error[,.(FY,Receipt_Error)],id.vars = "FY",variable.name = "Type",value.name = "value")

error_long[, highlight := ifelse(FY %in% tail(sort(unique(FY)), 4), "Last4", "Other")]

ggplot(error_long,aes(x=FY,y=value,fill=highlight)) + geom_col(position="dodge") +
  scale_fill_manual(values = c("Last4" = palette_e61(2)[2], "Other" = palette_e61(2)[1])) +
  plab(c("Pre-COVID","Post-COVID"),x=c("2005","2005"),y=c(3.5,2.5)) +
  scale_y_continuous_e61(limits = c(-2,6,2)) +
  geom_hline(yintercept = 0) +
  labs_e61(subtitle = "% of GDP",
           y= "%",
           x="",
           sources = c("Treasury","Budget 2025")
           ) +
  scale_x_discrete(
    breaks = function(x) x[seq(1, length(x), by = 3)]
  )

save_e61("Receipt_error.pdf")
save_e61("Receipt_error.png",res=2)

## Revenue scenario

Rev_gap <- read_excel("Debt scenario2.xlsx",
                             sheet = "Rev_gap")
setDT(Rev_gap)

colnames(Rev_gap) <- c("FY","Total","Exports","Compounding")

Rev_gap[, FY := as.numeric(str_extract(FY, "\\d{2}$"))]

Rev_gap_long <- melt(Rev_gap[,.(FY,Exports,Compounding)],id.vars = "FY",variable.name = "Type",value.name = "value")

ggplot(Rev_gap_long,aes(x=FY+2000,y=value/1000,colour=Type)) + geom_line()+
  scale_x_continuous_e61(limits = c(2024,2036,3),expand_left = 0.05,expand_right = 0.05,hide_first_last = FALSE) +
   scale_y_continuous_e61(limits = c(-150,25,25)) +
  geom_hline(yintercept = 0) +
  labs_e61(subtitle = "Revenue decline attributed to a shock",
           y = "$bn",
           x = "",
           sources = c("PBO","e61"),
           footnotes = c("Export price decline is a 45% drop from projections, to 2016 levels.","Productivity decline reduces trend growth from 1.2% to 0.6%.","Annual net migration declines by 35,000.")) +
  plab(c("Lower Export Prices","Lower Productivity + Migration"),x=c(2024.2,2024.2),y=c(-62,-87))

save_e61("Revenue_drivers.pdf")
save_e61("Revenue_drivers.png",res=2)

## Expense plots

Expense_wcore <- read_excel("Expenditure plots.xlsx",
                                sheet = "Actuarial exp")
setDT(Expense_wcore)

core <- mean(Expense_wcore[FY < 20]$Core)
total <- mean(Expense_wcore[FY < 20]$Total)


ggplot(melt(Expense_wcore,id.vars = "FY",variable.name = "Type",value.name = "value"),aes(x=as.numeric(FY)+2000,y=value*100,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(18,33,4)) +
  scale_x_continuous_e61(limits = c(2010,2028,by=3),hide_first_last = FALSE,expand_left = 0.05,expand_right = 0.05) +
  geom_hline(yintercept = core * 100, linetype = "dashed", colour = palette_e61(2)[2], linewidth = 0.8) +
  geom_hline(yintercept = total * 100, linetype = "dashed", colour = palette_e61(2)[1], linewidth = 0.8) +
  geom_vline(xintercept = 2025,linetype = "dashed") +
  labs_e61(subtitle = "Spending as a % of GDP",
           y= "%",
           x="",
           sources = c("PBO","e61"),
           footnotes = c("Core expenditure excludes interest and transfers to States and Local governments.","Dashed lines reflect the pre-COVID average of the category.")) +
  plab(c("Total","Core"),y=c(31.5,28),x=c(11,11))

save_e61("Expenses.pdf")
save_e61("Expenses.png",res=2)

## Expense five year averages

Five_year <- read_excel("Expenditure plots.xlsx",
                                sheet = "Sheet3", range = "E1:H11")
setDT(Five_year)

Five_year[, FY := str_extract(Five_year, "\\d{2}$")]
Five_year[, FY := factor(FY,levels=c("84","89","94","99","04","09","14","19","24","29"))]

ggplot(Five_year, aes(x = FY, y = Ratio*100)) +
  geom_col() +
  coord_cartesian(ylim = c(22, 28)) +
  scale_y_continuous_e61(breaks = seq(22, 28, by = 1)[-length(seq(22, 28, by = 1))]) +
  labs_e61(subtitle = "Five year expenditure % GDP, to FY",
           x="",
           sources = c("PBO","e61"))

save_e61("Expenses_5y.pdf",auto_scale = FALSE)

ggplot(Five_year, aes(x = FY, y = Ratio*100)) +
  geom_col() +
  coord_cartesian(ylim = c(22, 28)) +
  scale_y_continuous_e61(breaks = seq(22, 28, by = 1)[-length(seq(22, 28, by = 1))]) +
  labs_e61(title = "Federal Expenditure",
    subtitle = "Five year expenditure % GDP, to FY",
           x="",
           sources = c("PBO","e61"))

save_e61("Expenses_5y.png",res=2,auto_scale = FALSE)

## Migrant flows

Age_migrant <- read_excel("Age migrant.xlsx")
setDT(Age_migrant)

colnames(Age_migrant) <- c("Age","drop","drop2","migrant","population")

Age_migrant[, AgeGroup := fifelse(Age == "0–14", "0–14",
                         fifelse(Age %in% c("15–19", "20–24"), "15–24",
                                 fifelse(Age %in% c("25–29", "30–34"), "25–34",
                                         fifelse(Age %in% c("35–39", "40–44"), "35–44",
                                                 fifelse(Age %in% c("45–49", "50–54"), "45–54",
                                                         fifelse(Age %in% c("55–59", "60–64"), "55–64",
                                                                 "65+" ))))))]

Age_migrant2 <- Age_migrant[, .(
  drop = sum(drop),
  drop2 = sum(drop2),
  migrant = sum(migrant),
  population = sum(population)
), by = AgeGroup]

# Optional: order age groups nicely
age_order <- c("0–14", "15–24", "25–34", "35–44", "45–54", "55–64", "65+")
Age_migrant2[, AgeGroup := factor(AgeGroup, levels = age_order)]
setorder(Age_migrant2, AgeGroup)

Age_long <- melt(Age_migrant2[,.(AgeGroup,migrant,population)],id.vars = "AgeGroup",variable.name = "Type",value.name = "value")

ggplot(Age_long,aes(x=AgeGroup,y=value*100,fill=Type)) + geom_col(position = "dodge") +
  scale_y_continuous_e61(limits = c(0,35,5)) +
  labs_e61(subtitle = "Proportion of group in each Age Category",
           y = "%",
           x="",
           sources = c("ABS","e61"),
           footnotes = c("Population reflects both citizens and non-citizens in 2024. Migrants reflect new arrivals in 2024.")) +
  plab(c("Migrants","Australian Population"),x=c(4.2,4.2),y=c(27,22))

save_e61("Migrant.pdf")
save_e61("Migrant.png",res=2)


## Fiscal things (habits)

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

save_e61("Habit.pdf")

ggplot(pay_ngdp, aes(x = Year)) +
  geom_line(aes(y = Payments_norm, color = "Payments")) +
  geom_line(aes(y = RGDP_norm, color = "NGDP")) +
  geom_line(aes(y = RGDP_trend, color = "Pre-2014 NGDP Trend"), linetype = "dashed") +
  scale_color_manual(values = c("Payments" = palette_e61(3)[1], "NGDP" = palette_e61(3)[2], "Pre-2014 NGDP Trend" = "black")) +
  labs(y = "Indexed to FY2000 = 1", x = "Financial Year", color = "Series") +
  plab(c("Real Govt Payments","GDP","2000-2014 GDP trend"),x=c(2000.5,2000.5,2000.5),y=c(2.2,1.85,1.7),colour = c(palette_e61(3)[1],palette_e61(3)[2],"black")) +
  labs_e61(title = "Expenditure rises with old GDP trends",
    ,subtitle = "Deflated by GDPD, indexed to 1 in FY99/20",
           y="",
           x="") +
  scale_y_continuous_e61(limits = c(1,2.4,0.5))

save_e61("Habit.png",res=2)
