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
    y = NULL
  ) +
  scale_y_continuous(limits = c(0, 50)) +
  plab(c("Median","p90","p10"),x=c(80,60,60),y=c(42,25,12),colour = c(palette_e61(2)[2],palette_e61(2)[1],palette_e61(2)[1]))

save_e61("Taxable income distribution.svg")

tax10_dt <- read_excel("taxpercentilesdecadeallyears.xlsx")
setDT(tax10_dt)
tax10_dt[,p10 := fifelse(p10 <0,0,p10)]

ggplot(tax10_dt[percentile < 998], aes(x = percentile/10)) +
  geom_ribbon(aes(ymin = p10*100, ymax = p90*100), alpha = 0.4) +
  geom_line(aes(y = median*100), linewidth = 0.8) +
  geom_line(aes(y = p10*100), colour = palette_e61(2)[1], linewidth = 0.2) +
  geom_line(aes(y = p90*100), colour = palette_e61(2)[1], linewidth = 0.2) +
  labs_e61(
    title = "Distribution of Tax Rates",
    subtitle = "Earnings 2011/12-2021/22",
    x = "Income Percentile",
    y = NULL
  ) +
  scale_y_continuous(limits = c(0, 50)) +
  plab(c("Median","p90","p10"),x=c(80,60,60),y=c(32,25,5),colour = c(palette_e61(2)[2],palette_e61(2)[1],palette_e61(2)[1]))

save_e61("10yr Income distribution.svg")


## Federal gross debt

Fed_gross_dt <- read_excel("2025-26 Medium-Term Budget Outlook - Beyond the Budget - Data.xlsx", 
                                                                         sheet = "Fed_gross_graph")

setDT(Fed_gross_dt)

ggplot(Fed_gross_dt,aes(x=year,y=Gross_debt)) + geom_line() + geom_vline(xintercept =  2025,linetype="dashed") +
  labs_e61(title = "Federal Gross Debt",subtitle = "Percentage of GDP",y = "",x = "",
           sources = c("e61","PBO Medium Term Outlook")) +
  scale_y_continuous_e61(limits = c(0,40,10))






