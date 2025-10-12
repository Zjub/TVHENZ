## First:  26/09/2025
## Last update:  26/09/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Adding some aggregate expenses for the main report

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

rm(list=ls())
gc()

## Import data ----

work = FALSE

if (work == TRUE){
  consolidate_dt <- read_csv("C:/Users/MattNolan/Git/TVHENZ/e61 Projects/Fiscal sustainability/Function analysis/Data/abs_gfs_data_clean.csv")
} else{
  consolidate_dt <- read_csv("~/GitHub/TVHENZ/e61 Projects/Fiscal sustainability/Function analysis/Data/abs_gfs_data_clean.csv")
}


setDT(consolidate_dt)

colnames(consolidate_dt)

unique(consolidate_dt$cofog_div_name)
unique(consolidate_dt$cofog_group_name)
unique(consolidate_dt$etf_subclass_name)
unique(consolidate_dt$etf_class_name)
unique(consolidate_dt$etf_type_name)

### Defence ----

consolidated_expenses_dt <- consolidate_dt[etf_type_name == "Revenue and expenses"]

unique(consolidated_expenses_dt$etf_class_name)
unique(consolidated_expenses_dt$etf_subclass_name)

exp_dt <- consolidated_expenses_dt[,.(nominal = sum(gov_expenses_mn,na.rm=TRUE),real = sum(gov_expenses_mn_real_1112,na.rm=TRUE)),by=.(etf_class_name,fin_year)]

ggplot(consolidated_expenses_dt,aes(x=fin_year,y=gov_expenses_mn,fill=etf_class_name)) + geom_col()

ggplot(consolidated_expenses_dt,aes(x=fin_year,y=gov_expenses_mn_real_1112/1000,fill=etf_class_name)) + geom_col()

ggplot(consolidated_expenses_dt,aes(x=fin_year,y=gov_expenses_mn_real_1112,fill=etf_class_name)) + geom_col(position = "fill") + theme_e61(legend = "bottom")

summary(consolidated_expenses_dt)

consolidated_expenses_dt[gov_expenses_mn < 0]

ggplot(exp_dt,aes(x=fin_year,y=real/1000,fill=etf_class_name)) + 
  geom_col() + theme_e61(legend = "bottom") +
  labs_e61(title = "Total expenses",
           sources = c("ABS","e61"),
           y="$m (2012 prices)") 

ggplot(exp_dt,aes(x=fin_year,y=real/1000,fill=etf_class_name)) + 
  geom_col(position="fill") + theme_e61(legend = "bottom") +
  labs_e61(title = "Total expenses",
           sources = c("ABS","e61"),
           y="$m (2012 prices)") 

## Cut down number of groups for the plot

exp_dt[,etf_broad := fcase(etf_class_name %in% c("Current transfer expenses","Capital transfer expenses"),"Transfers",
                                             etf_class_name %in% c("Superannuation expenses","Other employee expenses"),"Employees",
                           etf_class_name %in% "Other property expenses","Depreciation",
                                             default = etf_class_name)]

exp_broad_dt <- exp_dt[,.(nominal = sum(nominal,na.rm=TRUE),real = sum(real,na.rm=TRUE)),by=.(etf_broad,fin_year)]

exp_broad_dt$etf_broad <- factor(exp_broad_dt$etf_broad,levels = c("Depreciation","Interest expenses","Employees","Non-employee expenses","Transfers"))

ggplot(exp_broad_dt,aes(x=fin_year,y=real/1000,fill=etf_broad)) + 
  geom_col() + theme_e61(legend = "bottom") +
  labs_e61(title = "Total expenses",
           sources = c("ABS","e61"),
           y="$m (2012 prices)") 

exp_shares_broad <- copy(exp_broad_dt)[
  , .(value = sum(real, na.rm = TRUE)), by = .(fin_year, etf_broad)
][
  , total := sum(value), by = fin_year
][
  , share := fifelse(total > 0, value / total, NA_real_)
]

ggplot(exp_shares_broad,aes(x=fin_year,y=share,fill=etf_broad)) + 
  geom_col() + 
  labs_e61(title = "Total expenses",
           sources = c("ABS","e61"),
           y="Proportion of total expenditure") +
  scale_y_continuous_e61(limits = c(0,1.2,0.25),expand_top = 0.05)+
  plab(c("Depreciation","Interest","Employee","Non-employee","Transfers"),x=c(1999,1999,2008,2008,2018),y=c(1.1,1.2,1.1,1.2,1.2))

save_e61("Consolidated_expenses.png",res=2)

exp_shares_broad[etf_broad == "Non-employee expenses"]
exp_shares_broad[etf_broad == "Employees"]

### Investigate non-employee expenses

exp_NEE_dt <- consolidated_expenses_dt[etf_class_name == "Non-employee expenses"][,.(nominal = sum(gov_expenses_mn,na.rm=TRUE),real = sum(gov_expenses_mn_real_1112,na.rm=TRUE)),by=.(etf_subclass_name,fin_year)]

exp_NEE_shares <- copy(exp_NEE_dt)[
  , .(value = sum(real, na.rm = TRUE)), by = .(fin_year, etf_subclass_name)
][
  , total := sum(value), by = fin_year
][
  , share := fifelse(total > 0, value / total, NA_real_)
]

ggplot(exp_NEE_shares,aes(x=fin_year,y=share,fill=etf_subclass_name)) + 
  geom_col() + 
  labs_e61(title = "Non-employee expenses",
           sources = c("ABS","e61"),
           y="Proportion of total NEE expenditure") +
  scale_y_continuous_e61(limits = c(0,1.2,0.25),expand_top = 0.05) +
  plab(c("Support for households","Goods and services"),x=c(1999,1999),y=c(1.2,1.1))

save_e61("Consolidated_expenses_non-EE.png",res=2)

cont_data_NEE <- exp_NEE_shares[fin_year %in% c(1999,2024)]

## Work out payment if the share hadn't changed, and what that would mean for the change in share for the total (i.e. is this all due to payments for household services?)

cont_data_NEE

exp_shares_broad[fin_year %in% c(1999,2024)]

XXX [Calculate share changes] 


exp_EE_dt <- consolidated_expenses_dt[etf_class_name == "Other employee expenses"][,.(nominal = sum(gov_expenses_mn,na.rm=TRUE),real = sum(gov_expenses_mn_real_1112,na.rm=TRUE)),by=.(etf_subclass_name,fin_year)]

ggplot(exp_EE_dt,aes(x=fin_year,y=real/1000,fill=etf_subclass_name)) + 
  geom_col(position="fill") + theme_e61(legend = "bottom") +
  labs_e61(title = "Other employee expenses",
           sources = c("ABS","e61"),
           y="Proportion of total EE expenditure") 

consolidated_expenses_dt[,etf_broad := fcase(etf_class_name %in% c("Current transfer expenses","Capital transfer expenses"),"Transfers",
                           etf_class_name %in% c("Superannuation expenses","Other employee expenses"),"Employees",
                           etf_class_name %in% "Other property expenses","Depreciation",
                           default = etf_class_name)]

exp_trans_dt <- consolidated_expenses_dt[etf_broad == "Transfers"][,.(nominal = sum(gov_expenses_mn,na.rm=TRUE),real = sum(gov_expenses_mn_real_1112,na.rm=TRUE)),by=.(etf_subclass_name,fin_year)]

ggplot(exp_trans_dt[fin_year != 1999],aes(x=fin_year,y=real/1000,fill=etf_subclass_name)) + 
  geom_col(position="fill") + theme_e61(legend = "bottom") +
  labs_e61(title = "Transfer expenses",
           sources = c("ABS","e61"),
           y="Proportion of total transfer expenditure") 

unique(exp_trans_dt$etf_subclass_name)
