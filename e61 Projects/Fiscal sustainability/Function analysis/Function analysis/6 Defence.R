## First:  15/09/2025
## Last update:  23/09/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Creating defence plots for report.

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

consolidated_expenses_dt <- consolidate_dt[cofog_div_name == "Defence" & etf_type_name == "Revenue and expenses"]

exp_dt <- consolidated_expenses_dt[,.(nominal = sum(gov_expenses_mn,na.rm=TRUE),real = sum(gov_expenses_mn_real_1112,na.rm=TRUE)),by=.(cofog_group_name,cofog_group_code,fin_year)]

unique(consolidated_expenses_dt$cofog_group_name)

ggplot(consolidated_expenses_dt,aes(x=fin_year,y=gov_expenses_mn,fill=cofog_group_name)) + geom_col()

ggplot(consolidated_expenses_dt,aes(x=fin_year,y=gov_expenses_mn_real_1112/1000,fill=cofog_group_name)) + geom_col()

ggplot(consolidated_expenses_dt,aes(x=fin_year,y=gov_expenses_mn_real_1112,fill=cofog_group_name)) + geom_col(position = "fill") + theme_e61(legend = "bottom")

summary(consolidated_expenses_dt)

consolidated_expenses_dt[gov_expenses_mn < 0]

ggplot(exp_dt,aes(x=fin_year,y=real/1000,fill=cofog_group_name)) + 
  geom_col() + theme_e61(legend = "bottom") +
  labs_e61(title = "Defence",
           sources = c("ABS","e61"),
           y="$m (2012 prices)") 

ggplot(exp_dt,aes(x=fin_year,y=real/1000,fill=cofog_group_name)) + geom_col(position = "fill") + theme_e61(legend = "bottom")

## Costs

cost_defence <- consolidated_expenses_dt[,.(nominal = sum(gov_expenses_mn,na.rm=TRUE),real = sum(gov_expenses_mn,na.rm=TRUE)),by=.(fin_year,etf_class_name)]

ggplot(cost_defence,aes(x=fin_year,y=nominal,fill=etf_class_name)) + geom_col() + theme_e61(legend = "bottom")

ggplot(cost_defence,aes(x=fin_year,y=nominal,fill=etf_class_name)) + geom_col(position = "fill") + theme_e61(legend = "bottom")

cost_defence[,agg_expense := fcase(etf_class_name %in% c("Capital transfer expenses","Current transfer expenses"),"Transfers",
                                 default = "Capital and labour expenses")]

cost_defence2 <- cost_defence[,.(nominal = sum(nominal,na.rm=TRUE),real = sum(real,na.rm=TRUE)),by=.(fin_year,agg_expense)]

ggplot(cost_defence2,aes(x=fin_year,y=nominal,fill=agg_expense)) + geom_col() + theme_e61(legend = "bottom")


## Look at defence as % of GDP from this measure - and then what it would look like excluding superannuation



## Our world in data data

if (work == TRUE){
  sipri_dt <- read_csv("C:/Users/MattNolan/Git/TVHENZ/e61 Projects/Fiscal sustainability/Function analysis/Data/military-spending-as-a-share-of-gdp-sipri.csv")
} else{
  sipri_dt <- read_csv("~/GitHub/TVHENZ/e61 Projects/Fiscal sustainability/Function analysis/Data/military-spending-as-a-share-of-gdp-sipri.csv")
}

setDT(sipri_dt)

unique(sipri_dt$Code)

sipri_dt[Code == "CHN"]

AUS_defence <- sipri_dt[Code %in% c("AUS","USA","GBR","CHN")]
colnames(AUS_defence) <- c("Country","Code","Year","value","region")

ggplot(AUS_defence[Year >= 1990],aes(x=Year,y=value,colour=Code)) + geom_line() +
  scale_y_continuous_e61(limits = c(0,8,2)) +
  labs_e61(title = "Defence spending",
           y = "% GDP",
           sources = c("SIPRI","e61"),
           footnotes = c("Defence characterisation based on NATO definitions.")) +
  plab(c("Australia","China","United Kingdom","United States"),x=rep(1995,4),y=c(5.5,4.5,6.5,7.5))

save_e61("SIPRI_defence.png",res=2)


