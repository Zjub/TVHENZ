## Last update:  29/09/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Calculate the sector contributions across all categories with ABS data - comparison to "OECD_comparison" file.

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
library(moments)

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

unique(consolidate_dt$etf_type_name)

consolidated_expenses_dt <- consolidate_dt[etf_type_name == "Revenue and expenses"]

dt <- consolidated_expenses_dt[fin_year %in% c(2014,2024)][,.(nom_spend = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_div_name,fin_year)]

totals <- dt[, .(nom_spend = sum(nom_spend)), by = fin_year][
  , cofog_div_name := "Total"]

dt <- rbind(dt, totals)

wide <- dcast(dt, cofog_div_name ~ fin_year, value.var = "nom_spend")
colnames(wide) <- c("cofog_div_name","a2014","a2024")


wide[,change := a2024 - a2014]

wide[,contribution := change/wide[cofog_div_name == "Total"]$change]
wide[,size := a2014/wide[cofog_div_name == "Total"]$a2014]

wide

ggplot(melt(wide[cofog_div_name != "Total",.(cofog_div_name,contribution = contribution*100,size = size*100)],id.vars = "cofog_div_name"),aes(x=cofog_div_name,y=value,fill=variable)) + geom_col(position = "dodge") + 
  coord_flip() +
  scale_y_continuous_e61(limits = c(0,50,10)) + 
  plab(c("Contribution to growth","Size in 2014"),y=c(20,20),x=c(1.5,2.5)) +
  labs_e61(title = "Contributions to spending growth",
           y="%",
           x="",
           sources = c("e61","ABS"))

save_e61("ABS_contribution_growth.png",res=2)

## Check the post 1999 figure
# 
# dt2 <- consolidated_expenses_dt[fin_year %in% c(1999,2024)][,.(nom_spend = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_div_name,fin_year)]
# 
# totals2 <- dt2[, .(nom_spend = sum(nom_spend)), by = fin_year][
#   , cofog_div_name := "Total"]
# 
# dt2 <- rbind(dt2, totals2)
# 
# wide2 <- dcast(dt2, cofog_div_name ~ fin_year, value.var = "nom_spend")
# colnames(wide2) <- c("cofog_div_name","a2014","a2024")
# 
# wide2[,change := a2024 - a2014]
# 
# wide2[,contribution := change/wide2[cofog_div_name == "Total"]$change]
# wide2[,size := a2024/wide2[cofog_div_name == "Total"]$a2024]
# 
# wide2

## Suspect that people have added transactions in non-financial assets into expenses.
# 
# dt_check <- consolidate_dt[fin_year %in% c(1999,2024)][,.(nom_spend = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_div_name,fin_year)]
# 
# totals_check <- dt_check[, .(nom_spend = sum(nom_spend)), by = fin_year][
#   , cofog_div_name := "Total"]
# 
# dt_check <- rbind(dt_check, totals_check)
# 
# wide_check <- dcast(dt_check, cofog_div_name ~ fin_year, value.var = "nom_spend")
# 
# wide_check[,change := `2024` - `1999`]
# 
# wide_check[,contribution := change/wide_check[cofog_div_name == "Total"]$change]
# 
# wide_check
