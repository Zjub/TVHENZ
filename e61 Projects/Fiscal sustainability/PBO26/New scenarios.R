## Last update:  23/09/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Update debt scenarios for external report using latest PBO tool (the finalised FY26 tool, rather than the interim one used in the internal report)

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

debt_dt <- read_excel("Scenarios.xlsx", 
                      sheet = "For_plot")

setDT(debt_dt)

debt_long_rev <- melt(debt_dt[,.(Year,Baseline,NetMig,NetMig_prod,NetMig_prod_TOT)], id.vars = "Year",
                      variable.name = "Type",
                      value.name = "value")

debt_long_rev$Type <- factor(debt_long_rev$Type,levels = c("Baseline","NetMig","NetMig_prod","NetMig_prod_TOT"))

ggplot(debt_long_rev,aes(x=Year,y=value*100,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(30,70)) +
  plab(c("Forecast","Lower Net Migration","+ Lower Productivity","+ Lower Export Prices"),x=c(2024,2024,2024,2024),y=c(66,62,58,54)) +
  labs_e61(title = "Gross debt projections (Revenue shocks)",
           y = "% of nominal GDP",
           x="",
           sources = c("PBO Build Your Own Budget 25/26","e61"),
           footnotes = c("Scenario reduces net migration by 80k and halves productivity growth to 0.6%pa, alongside lower real wage growth to match. Terms of trade shock reflects a 45% decline in key export prices to 2016 levels."))

save_e61("Projections_debt_rev.png",res=2)
save_e61("Projections_debt_rev.svg")

ggplot(debt_long_rev,aes(x=Year,y=value*100,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(30,70)) +
  plab(c("Forecast","Lower Net Migration","+ Lower Productivity","+ Lower Export Prices"),x=c(2024,2024,2024,2024),y=c(66,62,58,54)) +
  labs_e61(
           y = "% of nominal GDP",
           x="",
           sources = c("PBO Build Your Own Budget 25/26","e61"),
           footnotes = c("Scenario reduces net migration by 80k and halves productivity growth to 0.6%pa, alongside lower real wage growth to match. Terms of trade shock reflects a 45% decline in key export prices to 2016 levels."))



debt_long_exp <- melt(debt_dt[,.(Year,Baseline,Defence,Def_NDIS,Def_NDIS_Inter)], id.vars = "Year",
                      variable.name = "Type",
                      value.name = "value")

debt_long_exp$Type <- factor(debt_long_exp$Type,levels = c("Baseline","Defence","Def_NDIS","Def_NDIS_Inter"))

ggplot(debt_long_exp,aes(x=Year,y=value*100,colour=Type)) + geom_line() +
  scale_y_continuous_e61(limits = c(30,70)) +
  scale_x_continuous_e61(limits = c(2024,2036,3),expand_left = 0.05,expand_right = 0.05,hide_first_last = FALSE) +
  plab(c("Forecast","Defence spending","+ NDIS","+ Interest increase"),x=c(2024,2024,2024,2024),y=c(66,62,58,55)) +
  labs_e61(title = "Gross debt projections (expenditure shock)",
           y = "% of nominal GDP",
           x="",
           sources = c("PBO Build Your Own Budget 25/26","e61"),
           footnotes = c("Defence spending increased from 2.2% of GDP to 3.0% over five years.","NDIS spending grows at 10%pa rather than 8%pa.","Ten-year goverment bond rate rises to 5.5%pa from a 4.5%pa projection."))

save_e61("Projections_debt_exp.png",res=2)
save_e61("Projections_debt_exp.svg")


