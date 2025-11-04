## Last update:  15/09/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Creating economic affairs plots for report.

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

### Economic Affairs ----

consolidated_expenses_dt <- consolidate_dt[cofog_div_name == "Economic affairs" & etf_type_name == "Revenue and expenses"]

exp_dt <- consolidated_expenses_dt[,.(nominal = sum(gov_expenses_mn,na.rm=TRUE),real = sum(gov_expenses_mn_real_1112,na.rm=TRUE)),by=.(cofog_group_name,cofog_group_code,fin_year)]

unique(consolidated_expenses_dt$cofog_group_name)

ggplot(consolidated_expenses_dt,aes(x=fin_year,y=gov_expenses_mn,fill=cofog_group_name)) + geom_col()

ggplot(consolidated_expenses_dt,aes(x=fin_year,y=gov_expenses_mn_real_1112/1000,fill=cofog_group_name)) + geom_col()

ggplot(consolidated_expenses_dt,aes(x=fin_year,y=gov_expenses_mn_real_1112,fill=cofog_group_name)) + geom_col(position = "fill") + theme_e61(legend = "bottom")

summary(consolidated_expenses_dt)

consolidated_expenses_dt[gov_expenses_mn < 0]

ggplot(exp_dt,aes(x=fin_year,y=real/1000,fill=cofog_group_name)) + 
  geom_col() + 
  labs_e61(title = "Economic Affairs",
           sources = c("ABS","e61"),
           y="$m (2012 prices)") +
  plab(c("General","Other Primary Industries","Fuel","Mining and heavy industries","Communication","Other Industry","R&D","Other"),x=rep(1999,8),y=c(90,80,70,60,50,40,30,20))

ggplot(exp_dt,aes(x=fin_year,y=real/1000,fill=cofog_group_name)) + geom_col(position = "fill") + theme_e61(legend = "bottom")

exp_dt[,agg_group := fcase(cofog_group_code == "041","General",
                           cofog_group_code %in% c("042","043"),"Primary ex Mining",
                           cofog_group_code == "044","Mining and heavy industry",
                           default = "Other")]

exp_dt2 <- exp_dt[,.(nominal = sum(nominal,na.rm=TRUE),real = sum(real,na.rm=TRUE)),by=.(agg_group,fin_year)]

exp_dt2$agg_group <- factor(exp_dt2$agg_group,levels = c("General","Primary ex Mining","Mining and heavy industry","Other"))

ggplot(exp_dt2,aes(x=fin_year,y=real/1000,fill=agg_group)) + 
  geom_col() + 
  labs_e61(title = "Economic Affairs",
           sources = c("ABS","e61"),
           y="$m (2012 prices)") +
  plab(c("General","Primary Industries (ex Mining)","Mining and heavy industries","Other"),x=rep(1999,4),y=c(85,75,65,55))

save_e61("Economic_affairs_type.png",res=2)
save_e61("Economic_affairs_type.svg")

### Understand more about general

general <- consolidated_expenses_dt[cofog_group_code == "041"]

general_dt <- general[,.(nominal = sum(gov_expenses_mn,na.rm=TRUE),real = sum(gov_expenses_mn,na.rm=TRUE)),by=.(fin_year,etf_class_name)]

general_dt[,agg_expense := fcase(etf_class_name %in% c("Capital transfer expenses","Current transfer expenses"),"Transfers",
                                 default = "Capital and labour expenses")]

general_dt2 <- general_dt[,.(nominal = sum(nominal,na.rm=TRUE),real = sum(real,na.rm=TRUE)),by=.(fin_year,agg_expense)]

ggplot(general_dt2,aes(x=fin_year,y=nominal,fill=agg_expense)) + geom_col() + theme_e61(legend = "bottom")

g_dt <- general_dt2[,.(nom_total = sum(nominal)),by=.(fin_year)][general_dt2,on=.(fin_year)][,prop_nom_total := nominal/nom_total]

ggplot(g_dt,aes(x=fin_year,y=prop_nom_total*100,fill=agg_expense)) + geom_col() + 
  labs_e61(title = "Economic Activity expenses",
           y = "% share of annual spending",
           sources = c("ABS","e61")) +
  scale_y_continuous_e61()

save_e61("Economic_activity_expenses.png",res=2,auto_scale = FALSE)
save_e61("Economic_activity_expenses.svg",auto_scale = FALSE)

plot_1 <- ggplot(exp_dt2,aes(x=fin_year,y=real/1000,fill=agg_group)) + 
  geom_col() + 
  labs_e61(title = "A. Sub-function",
           y="$m (2012 prices)") +
  plab(c("General","Primary Industries (ex Mining)","Mining and heavy industries","Other"),x=rep(1999,4),y=c(85,75,65,55))


plot_2 <- ggplot(g_dt,aes(x=fin_year,y=prop_nom_total*100,fill=agg_expense)) + geom_col() +
  labs_e61(title = "B. Expenses",
           y = "% share of annual spending") +
  scale_y_continuous_e61()

save_e61("Econ_activity.png",plot_1,plot_2,res=2,sources = c("ABS","e61"),title = "Economic Affairs",auto_scale = FALSE)
save_e61("Econ_activity.svg",plot_1,plot_2,sources = c("ABS","e61"),title = "Economic Affairs",auto_scale = FALSE)
