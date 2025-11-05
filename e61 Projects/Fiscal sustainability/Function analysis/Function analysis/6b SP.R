## First:  15/09/2025
## Last update:  23/09/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Creating Social protection plots for report.

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

work = TRUE

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

### Social protection ----

consolidated_expenses_dt <- consolidate_dt[cofog_div_name == "Social protection" & etf_type_name == "Revenue and expenses"]

exp_dt <- consolidated_expenses_dt[,.(nominal = sum(gov_expenses_mn,na.rm=TRUE),real = sum(gov_expenses_mn_real_1112,na.rm=TRUE)),by=.(cofog_group_name,cofog_group_code,fin_year)]

unique(consolidated_expenses_dt$cofog_group_name)

ggplot(consolidated_expenses_dt,aes(x=fin_year,y=gov_expenses_mn,fill=cofog_group_name)) + geom_col()

ggplot(consolidated_expenses_dt,aes(x=fin_year,y=gov_expenses_mn_real_1112/1000,fill=cofog_group_name)) + geom_col()

ggplot(consolidated_expenses_dt,aes(x=fin_year,y=gov_expenses_mn_real_1112,fill=cofog_group_name)) + geom_col(position = "fill") + theme_e61(legend = "bottom")

summary(consolidated_expenses_dt)

consolidated_expenses_dt[gov_expenses_mn < 0]

ggplot(exp_dt,aes(x=fin_year,y=real/1000,fill=cofog_group_name)) + 
  geom_col() + theme_e61(legend = "bottom") +
  labs_e61(title = "Social protection",
           sources = c("ABS","e61"),
           y="$m (2012 prices)") 

ggplot(exp_dt,aes(x=fin_year,y=real/1000,fill=cofog_group_name)) + geom_col(position = "fill") + theme_e61(legend = "bottom")

## Costs

costSocial <- consolidated_expenses_dt[,.(nominal = sum(gov_expenses_mn,na.rm=TRUE),real = sum(gov_expenses_mn,na.rm=TRUE)),by=.(fin_year,etf_class_name)]

ggplot(costSocial,aes(x=fin_year,y=nominal,fill=etf_class_name)) + geom_col() + theme_e61(legend = "bottom")

ggplot(costSocial,aes(x=fin_year,y=nominal,fill=etf_class_name)) + geom_col(position = "fill") + theme_e61(legend = "bottom")

costSocial[,agg_expense := fcase(etf_class_name %in% c("Capital transfer expenses","Current transfer expenses"),"Transfers",
                                   etf_class_name %in% c("Other employee expenses","Superannuation expenses"), "Labour expenses",
                                   etf_class_name %in% c("Depreciation","Non-employee expenses"),etf_class_name,
                                 default = "Capital and labour expenses")]

costSocial[,agg_expense := factor(agg_expense,levels = c("Transfers","Depreciation","Non-employee expenses","Labour expenses"))]

costSocial2 <- costSocial[, .(
  nominal = sum(nominal, na.rm = TRUE),
  real    = sum(real, na.rm = TRUE)
), by = .(fin_year, agg_expense)]

costSocial2[, total_nominal := sum(nominal), by = fin_year]
costSocial2[, share_nominal := nominal / total_nominal]

ggplot(costSocial2, aes(x = fin_year, y = share_nominal*100, fill = agg_expense)) +
  geom_col() +
  labs_e61(title = "Social protection expenses", y = "Share of total", x = "",
           sources = c("ABS","e61")) +
  plab(c("Transfers","Depreciation","Non-employee","Employee"),y=c(120,110,120,110),x=c(1999,1999,2010,2010)) +
  scale_y_continuous_e61(limits = c(0,125,25),add_space = TRUE)

save_e61("Social protection_exp.png",res=2,auto_scale = FALSE)
save_e61("Social protection_exp.svg",auto_scale = FALSE)

## Look at Social protection as % of GDP from this measure - and then what it would look like excluding superannuation

GDP <- read_abs("5206.0")
setDT(GDP)

GDP <- GDP[table_no == "5206001_key_aggregates"]

unique(GDP$series)

# Filter for the required series: GDP, Terms of Trade, and RNGDI
GDP_dt <- GDP[
  date >= as.Date("1980-01-01") & 
    series %in% c(
      "Gross domestic product: Current prices ;"
    ) & 
    series_type == "Original"
]

GDP_dt <- GDP_dt[,.(date,month = as.numeric(month(date)),year = as.numeric(year(date)),value)]
GDP_dt[,fin_year := fcase(month < 7, year - 1,
                          default = year)]

GDP_dt[,.N,by=.(fin_year)]

GDP_fin_year <- GDP_dt[,.(GDP = sum(value)),by=.(fin_year)]

def_GDP <- GDP_fin_year[exp_dt[cofog_group_code == "029"],on=.(fin_year)][,prop := nominal/GDP]

costSocial2[agg_expense == "Non-employee expenses"]

(def_GDP[fin_year == 2024]$nominal-10000)/(def_GDP[fin_year == 2024]$GDP)
