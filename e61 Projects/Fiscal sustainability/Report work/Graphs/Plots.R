# Topic: Takes background data and produces plots
# Author: Matt Nolan
# Created: 19/10/2025
# Last edit: 19/10/2025
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

## Figure 2: Net debt

f2_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_2")

setDT(f2_dt)

ggplot(melt(f2_dt,id.vars = "Year"),aes(x=Year,y=value,colour =variable)) + geom_line() +
  scale_y_continuous_e61(limits = c(-10,40,10)) + geom_hline(yintercept = 0) +
  labs_e61(title = "Net debt projected to keep rising",
           y = "% NGDP",
           sources = c("PBO","e61"),
           footnotes = c("Actuals and projections come from the 2024/25 National Fiscal Outlook")) +
  geom_vline(xintercept = 2023,linetype = "dashed") +
  plab(c("State","Federal","National"),x=c(2013,2005,2005),y=c(-5,25,35))

save_e61("Figure_2.png",res=2)



## Figure 5: Untied data ratio

f5_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_5")

setDT(f5_dt)

ggplot(f5_dt,aes(x=Year,y=Untied_Ratio*100)) + geom_line() + scale_y_continuous_e61(limits = c(0,80,20)) +
  labs_e61(title = "Federal share of general funding for states.",
           y= "% of total funding",
           sources = c("Budget Papers 3","e61"),
           footnotes = c("General funding includes both GST revenue and general revenue assistance at the state and local level."))

save_e61("Figure_5.png",res=2)



# Figure 8: Low vs high


f8_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_8")


setDT(f8_dt)

ggplot(f8_dt, aes(year, index, colour = series)) +
  geom_line() +
  labs_e61(title = "Low spending countries catchup",
           x = NULL, y = "1999 expenditure %GDP = 1", sources = c("e61","OECD"),
           footnotes = c("Countries classified by average spending as a % of GDP in the 1999s.","Low countries are the bottom quartile: United Kingdom, United States,Estonia, Latvia, Lithuania, Luxembourg, Switzerland.","High countries are the top quartile: Finland, Australia, Belgium, Denmark, France, Israel, Sweden.")) +
  plab(c("Australia","High Spenders","Low Spenders"),y=c(1.22,1.12,1.17),x = c(1999,1999,1999)) +
  scale_y_continuous_e61(limits=c(0.9,1.3,0.1))

save_e61("Figure_8.png",res=2)


# Figure 9: Synthetic control exercise aggregate

f9_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_9")


setDT(f9_dt)


ggplot(f9_dt, aes(x = year, y = value, colour = series)) +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  geom_line() +
  labs_e61(title = "Australian Fiscal Expenditure remains high",
           x = "Year", y = "Expenditure/GDP Index", linetype = "",
           sources = c("OECD","e61"),
           footnotes = c("An index of nominal government expenditure to nominal GDP, relative to its 1999 level.",
                         "Australia's Fiscal Year ends in June rather than December. For this reason the Australian data is averaged across consecutive years.","Five main donor countries are United States, Israel, Norway, Iceland, and New Zealand. Weights are provided in Appendix A.")) +
  geom_hline(yintercept = 1)  +
  plab(c("Observed","Synthetic"),x=c(2000,2000),y=c(1.13,1.07)) +
  scale_y_continuous_e61(limits = c(0.9,1.3,0.1))

save_e61("Figure_9.png",res=2)
