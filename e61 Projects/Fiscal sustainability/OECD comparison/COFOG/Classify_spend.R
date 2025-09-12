## Script for classifying spending by a broad public purpose
## Created 12/09/2025
## Last edit 12/09/2025

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

## Import data

cons_dt <- read_csv("abs_gfs_data_clean.csv")
setDT(cons_dt)

class_table <- read_csv("Table A1C.2 - The detailed classification of the COFOG-A.csv",skip=1)
setDT(class_table)

class_table <- rename(class_table,cofog_group_code = `COFOG-A`)

class_table <- class_table[,.(Descriptor,cofog_group_code,Type)]

dt <- class_table[cons_dt,on=.(cofog_group_code)]

# Now adjust to focus on inscope spending categories

dt <- dt[etf_type_name == "Revenue and expenses"]

dt_type <- dt[,.(type_spend = sum(gov_expenses_mn,na.rm=TRUE)),by=.(fin_year,Type)]

ggplot(dt_type,aes(x=fin_year,y=type_spend,fill=as.factor(Type))) + geom_col() + theme_e61(legend = "bottom")

dt_type <- dt_type[,.(fy_spend = sum(type_spend)),by=.(fin_year)][dt_type,on=.(fin_year)]

dt_type[,prop := type_spend/fy_spend]

ggplot(dt_type,aes(x=fin_year,y=prop,fill=as.factor(Type))) + geom_col() +
  scale_x_continuous(limits = c(1985,2024)) + 
  scale_y_continuous_e61(limits = c(0,1,0.2)) +
  plab(c("Administration","Public Goods","Transfers","Direct Payment","Other"),x=rep(1985,5),y=c(0.9,0.8,0.6,0.4,0.2)) +
  labs_e61(title = "Purpose of spending")

save_e61("Purpose.png",res=2)
