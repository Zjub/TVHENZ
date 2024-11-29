# Script to pull incomes
# Author: Matt Nolan
# Date made: 31/10/2024
# Last update: 31/10/2024
# Appears they deleted a bunch of the data - can look to request the rest for the next clearance, not sure what the issue was though (the data they removed was the "tests", we don't need it)

rm(list=ls())

library(tidyverse)
library(data.table)
library(theme61)
library(rdrobust)
library(readxl)


Income <- read.csv("initial_RDD/income_mean_balance 1_safe .csv")
setDT(Income)

Income <- Income[, !c("X", "variable"), with = FALSE]

# Melt the data by "nz" status
Income <- melt(Income, id.vars = "nz", variable.name = "year", value.name = "income")

Income <- Income[year != "pre_income"] # This is unconditional earnings - so adding zeros

Income[, year := sub(".*(..)$", "\\1", year)]

ggplot(Income,aes(x=year,y=income,fill=as.factor(nz))) + geom_col(position="dodge")

Income[, starting_income := income[year == min(year)], by = nz]

Income[, income_index := income / starting_income]

ggplot(Income[year != 19],aes(x=year,y=income_index,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,1.5,0.3)) +
  labs_e61(title = "Earnings relative to FY19",subtitle="For employed individuals",y="",sources=c("ABS","e61")) + geom_hline(yintercept = 1,linetype = "dashed") +
  plab(c("Australia","NZ"),x=c(1,1),y=c(1.4,1.25))

save_e61("Incomes.png",res=2,pad_width = 1)


ggplot(Income[year != 19],aes(x=year,y=income_index,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,1.5,0.3)) +
  labs_e61(subtitle="For employed individuals",y="",sources=c("ABS","e61")) + geom_hline(yintercept = 1,linetype = "dashed") +
  plab(c("Australia","NZ"),x=c(1,1),y=c(1.4,1.25))


save_e61("Incomes.png",res=2,pad_width = 1)
