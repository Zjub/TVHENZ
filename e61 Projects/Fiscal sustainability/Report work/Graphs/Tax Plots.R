# Topic: For Nov 10 presentation on Horizontal Equity
# Author: Matt Nolan
# Created: 9/11/2025
# Last edit: 9/11/2025
# Last editor: Matt Nolan


rm(list=ls())

#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(readxl)
library(tidyverse)
library(data.table)
library(Hmisc)
library(tidysynth)
library(readabs)


tax_dt <- read_excel("taxpercentilesATODefinition.xlsx")
setDT(tax_dt)

ggplot(tax_dt[percentilex < 999], aes(x = percentilex/10)) +
  geom_ribbon(aes(ymin = p10*100, ymax = p90*100), alpha = 0.4) +
  geom_line(aes(y = median*100), linewidth = 1) +
  #geom_line(aes(y = p10*100), colour = palette_e61(2)[1], linewidth = 0.2) +
  #geom_line(aes(y = p90*100), colour = palette_e61(2)[1], linewidth = 0.2) +
  labs_e61(
    title = "Distribution of Tax Rates",
    subtitle = "At each income percentile\nMedian, and p90–p10 %",
    x = "Taxable Income Percentile",
    y = NULL,
    source = c("ABS","e61")
  ) +
  scale_y_continuous(limits = c(0, 50)) +
  plab(c("Median","p90","p10"),x=c(80,60,60),y=c(42,25,12),colour = c(palette_e61(2)[2],palette_e61(2)[1],palette_e61(2)[1]))

save_e61("Taxable income distribution.svg")
save_e61("Taxable income distribution.pdf")

tax10_dt <- read_excel("taxpercentilesdecadeallyears.xlsx")
setDT(tax10_dt)
tax10_dt[,p10 := fifelse(p10 <0,0,p10)]

ggplot(tax10_dt[mean_income >= 0 & percentile <= 998], aes(x = percentile/10)) +
  geom_ribbon(aes(ymin = p10*100, ymax = p90*100), alpha = 0.4) +
  geom_line(aes(y = median*100), linewidth = 0.8) +
  geom_line(aes(y = p10*100), colour = palette_e61(2)[1], linewidth = 0.2) +
  geom_line(aes(y = p90*100), colour = palette_e61(2)[1], linewidth = 0.2) +
  labs_e61(
    title = "Distribution of Tax Rates",
    subtitle = "Earnings 2011/12-2021/22",
    x = "Income Percentile",
    y = NULL,
    source = c("ABS","e61")
  ) +
  scale_y_continuous(limits = c(0, 50)) +
  plab(c("Median","p90","p10"),x=c(80,60,60),y=c(32,25,5),colour = c(palette_e61(2)[2],palette_e61(2)[1],palette_e61(2)[1]))

save_e61("10yr Income distribution.svg")
save_e61("10yr Income distribution.pdf")
<<<<<<< Updated upstream

tax10_dt[percentile == 500]

## Federal gross debt

Fed_gross_dt <- read_excel("2025-26 Medium-Term Budget Outlook - Beyond the Budget - Data.xlsx", 
                                                                         sheet = "Fed_gross_graph")

setDT(Fed_gross_dt)

ggplot(Fed_gross_dt,aes(x=year,y=Gross_debt)) + geom_line() + geom_vline(xintercept =  2025,linetype="dashed") +
  labs_e61(title = "Federal Gross Debt",subtitle = "Percentage of GDP",y = "",x = "",
           sources = c("e61","PBO Medium Term Outlook")) +
  scale_y_continuous_e61(limits = c(0,40,10))


## Add additional plots out of income (rather than percentiles)

ggplot(tax10_dt[mean_income >= 0 & percentile <= 998], aes(x = log(mean_income/1000))) +
  geom_ribbon(aes(ymin = p10*100, ymax = p90*100), alpha = 0.4) +
  geom_line(aes(y = median*100), linewidth = 0.8) +
  geom_line(aes(y = p10*100), colour = palette_e61(2)[1], linewidth = 0.2) +
  geom_line(aes(y = p90*100), colour = palette_e61(2)[1], linewidth = 0.2) +
  labs_e61(
    title = "Distribution of Tax Rates",
    subtitle = "Earnings 2011/12-2021/22",
    x = "Log Incomee",
    y = NULL,
    source = c("ABS","e61")
  ) +
  scale_y_continuous(limits = c(0, 90)) #+
  #plab(c("Median","p90","p10"),x=c(80,60,60),y=c(32,25,5),colour = c(palette_e61(2)[2],palette_e61(2)[1],palette_e61(2)[1]))

ggplot(tax10_dt[mean_income >= 0 & percentile <= 998], aes(x = (mean_income/1000))) +
  geom_ribbon(aes(ymin = p10*100, ymax = p90*100), alpha = 0.4) +
  geom_line(aes(y = median*100), linewidth = 0.8) +
  geom_line(aes(y = p10*100), colour = palette_e61(2)[1], linewidth = 0.2) +
  geom_line(aes(y = p90*100), colour = palette_e61(2)[1], linewidth = 0.2) +
  labs_e61(
    title = "Distribution of Tax Rates by Income",
    subtitle = "Earnings 2011/12-2021/22",
    x = "Income ($000s)",
    source = c("ABS","e61")
  ) +
  scale_y_continuous(limits = c(0, 50)) +
  plab(c("Median","p90","p10"),x=c(100,100,1000),y=c(32,25,15),colour = c(palette_e61(2)[2],palette_e61(2)[1],palette_e61(2)[1])) +
  scale_x_continuous_e61(c(0,5600,1000))

save_e61("10yr Income dollar distribution.svg")
save_e61("10yr Income dollar distribution.pdf")

## Add the relative capital gains plots
# First import our 1 year income measure (graphs above are ATO graphs)

tax1_dt <- read_excel("taxpercentiles 1.xlsx")
setDT(tax1_dt)

tax1_dt[,cg_share := total_CG/total_income]
tax10_dt[,cg_share := total_CG/total_income]

ggplot(tax10_dt[mean_income >= 0 & percentile <= 998 & percentile >= 200 ],aes(x= mean_income/1000/10,y=cg_share*100)) + geom_line() + 
  labs_e61(title = "By dollars earned",
           subtitle = "Capital Gains Share of Total Income",
           x= "Income (000s)",
           sources = c("ABS","e61"))+
  scale_x_continuous_e61(c(0,560,100))

save_e61("10yr Cap Gain Share Income.svg")
save_e61("10yr Cap Gain Share Income.pdf")

tax1_dt[, dataset := "1-year ETR"]
tax10_dt[, dataset := "10-year ETR"]

tax_plot_dt <- rbindlist(list(
  tax1_dt[, .(percentile = percentile, cg_share, total_income, dataset)],
  tax10_dt[, .(percentile, cg_share, total_income,dataset)]
))


ggplot(tax_plot_dt[total_income >=0 & percentile >= 200 & percentile <= 998], aes(x = percentile/10, y = cg_share*100, colour = dataset)) +
  geom_line() +
  labs( title = "By earning percentile",
    x = "Income Percentile",
    subtitle = "Capital Gains Share of Total Income",
    source = c("ABS","e61")
  ) +
  plab(c("1-year ETR","10-year ETR"),x=c(20,20),y=c(13,8))

save_e61("10yr Cap Gain Share Compare.svg")
save_e61("10yr Cap Gain Share Compare.pdf")

## The 1 years

ggplot(tax1_dt[percentile < 999], aes(x = percentile/10)) +
  geom_ribbon(aes(ymin = p10*100, ymax = p90*100), alpha = 0.4) +
  geom_line(aes(y = median*100), linewidth = 1) +
  #geom_line(aes(y = p10*100), colour = palette_e61(2)[1], linewidth = 0.2) +
  #geom_line(aes(y = p90*100), colour = palette_e61(2)[1], linewidth = 0.2) +
  labs_e61(
    title = "Distribution of Tax Rates",
    subtitle = "At each income percentile\nMedian, and p90–p10 %",
    x = "Income Percentile",
    y = NULL,
    sources = c("ABS","e61")
  ) +
  scale_y_continuous(limits = c(0, 50)) +
  plab(c("Median","p90","p10"),x=c(80,60,60),y=c(42,25,12),colour = c(palette_e61(2)[2],palette_e61(2)[1],palette_e61(2)[1]))

save_e61("1yr income distribution.svg")
save_e61("1yr income distribution.pdf")

ggplot(tax1_dt[percentile < 999], aes(x = mean_income/1000)) +
  geom_ribbon(aes(ymin = p10*100, ymax = p90*100), alpha = 0.4) +
  geom_line(aes(y = median*100), linewidth = 1) +
  #geom_line(aes(y = p10*100), colour = palette_e61(2)[1], linewidth = 0.2) +
  #geom_line(aes(y = p90*100), colour = palette_e61(2)[1], linewidth = 0.2) +
  labs_e61(
    title = "Distribution of Tax Rates",
    subtitle = "At each income percentile\nMedian, and p90–p10 %",
    x = "Income (000s))",
    y = NULL,
    sources = c("ABS","e61")
  ) +
  scale_y_continuous(limits = c(0, 50)) +
  plab(c("Median","p90","p10"),x=c(80,80,200),y=c(45,35,12),colour = c(palette_e61(2)[2],palette_e61(2)[1],palette_e61(2)[1]))

save_e61("1yr income dollar distribution.svg")
save_e61("1yr income dollar distribution.pdf")
=======
>>>>>>> Stashed changes
