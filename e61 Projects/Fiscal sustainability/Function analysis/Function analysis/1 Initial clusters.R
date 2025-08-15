## Last update:  13/08/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Looking at the initial consolidated data to consider i) defence and interest, ii) capital spending, iii) clustering.

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

k <- 5

## Import data ----

work = TRUE

if (work == TRUE){
  consolidate_dt <- read_csv("C:/Users/MattNolan/Git/TVHENZ/e61 Projects/Fiscal sustainability/Function analysis/Data/abs_gfs_data_clean.csv")
} else{
  consolidate_dt <- read_csv("~/GitHub/TVHENZ/e61 Projects/Fiscal sustainability/Function analysis/Data/abs_gfs_data_clean.csv")
}


setDT(consolidate_dt)

colnames(consolidate_dt)

unique(consolidate_dt$cofog_group_name)
unique(consolidate_dt$etf_subclass_name)
unique(consolidate_dt$etf_class_name)
unique(consolidate_dt$etf_type_name)

### Defence and interest ----

consolidated_expenses_dt <- consolidate_dt[etf_type_name == "Revenue and expenses"]

# Two categories that are swapped between in data. Combine them
#019 General public services not elsewhere classified
#011 Executive and legislative organs financial and fiscal affairs external affairs

consolidated_expenses_dt[
  cofog_group_name %in% c(
    "019 General public services not elsewhere classified",
    "011 Executive and legislative organs financial and fiscal affairs external affairs"
  ),
  cofog_group_name := "01 General public services"
]

group_exp_dt <- consolidated_expenses_dt[,.(nom_expense = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_group_name,fin_year)]

unique(as.numeric(consolidated_expenses_dt$cofog_group_code))

consolidated_expenses_dt[, cofog_group_name := fcase(
  !substr(cofog_group_name, 1, 2) %in% c("07","09","10"),substr(cofog_group_name, 1, 2),
  as.numeric(cofog_group_code) %in% seq(74,79,by=1), "74 Other Health",
  as.numeric(cofog_group_code) %in% c(72,73), "73 Hospitals",
  as.numeric(cofog_group_code) > 93 & as.numeric(cofog_group_code) <= 99,"94 Other Education",
  as.numeric(cofog_group_code) == 102 | (as.numeric(substr(cofog_group_name, 1, 3)) > 105 & as.numeric(substr(cofog_group_name, 1, 3)) <= 109),"106 Other Social Protection",default = cofog_group_name)]

consolidated_expenses_dt[, cofog_group_name := fcase(
  cofog_group_name == "01","01 General Public Service",
  cofog_group_name == "02","02 Defence",
  cofog_group_name == "03","03 Public Order",
  cofog_group_name == "04","04 Economic Affairs",
  cofog_group_name == "05","05 Environmental Protection",
  cofog_group_name == "06","06 Amenities",
  #cofog_group_name == "07","07 Health",
  cofog_group_name == "08","08 Culture",
  #cofog_group_name == "09","09 Education",
  cofog_group_name == "11","11 Transport",
  default = cofog_group_name)]

group_exp_dt <- consolidated_expenses_dt[,.(nom_expense = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_group_name,fin_year)]

for(i in unique(group_exp_dt$cofog_group_name)){
  a <- ggplot(group_exp_dt[cofog_group_name == i],aes(x=fin_year,y=nom_expense)) +
    geom_line() + 
    labs_e61(title = i)
  
  print(a)
}

### Cluster analysis test ----
# Note clustering is fairly detailed work, so put most of this in another script - just set up some basic NN clustering and PCA to give an example of looking at i) what is similar ii) what drives similarity.

# Start with total spending by function.

group_exp_dt <- consolidated_expenses_dt[,.(nom_expense = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_group_name,fin_year)]
div_exp_dt <- consolidated_expenses_dt[,.(nom_expense = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_div_name,fin_year)]

## Cluster example
wide_exp_dt <- dcast(
  group_exp_dt,
  cofog_group_name ~ fin_year,
  value.var = "nom_expense",
  fun.aggregate = sum,
  na.rm = TRUE
)

colnames(wide_exp_dt)

# Set cofog_group_name as rownames
row_names <- wide_exp_dt$cofog_group_name
wide_exp_dt <- as.data.frame(wide_exp_dt[, -1, with = FALSE])
rownames(wide_exp_dt) <- row_names

# --- 1. Nominal levels ---
dist_nominal <- as.dist(1 - cor(t(wide_exp_dt), method = "pearson"))
hc_nominal <- hclust(dist_nominal, method = "ward.D2")
plot_nominal <- fviz_dend(hc_nominal, k = 10, horiz = TRUE, rect = TRUE, rect_fill = TRUE,
                          main = "Pearson correlation - Nominal levels",
                          cex = 0.6, lwd = 0.4)

# --- 2. Scaled levels (remove magnitude differences) ---
scaled_exp_dt <- scale(wide_exp_dt)
dist_scaled <- as.dist(1 - cor(t(scaled_exp_dt), method = "pearson"))
hc_scaled <- hclust(dist_scaled, method = "ward.D2")
plot_scaled <- fviz_dend(hc_scaled, k = 10, horiz = TRUE, rect = TRUE, rect_fill = TRUE,
                         main = "Pearson correlation - Scaled levels",
                         cex = 0.6, lwd = 0.4)

# --- 3. Year-on-year growth rates ---
wide_exp_growth_dt <- wide_exp_dt %>%
  mutate(across(everything(), ~ (.- dplyr::lag(.)) / dplyr::lag(.),
                .names = "diff_{.col}")) %>%
  select(-1) %>% # drop first growth column
  drop_na()

dist_growth <- as.dist(1 - cor(t(wide_exp_growth_dt), method = "pearson"))
hc_growth <- hclust(dist_growth, method = "ward.D2")
plot_growth <- fviz_dend(hc_growth, k = 10, horiz = TRUE, rect = TRUE, rect_fill = TRUE,
                         main = "Pearson correlation - YoY growth",
                         cex = 0.6, lwd = 0.4)

# --- Arrange in a grid ---
grid.arrange(plot_scaled, plot_growth, ncol = 1)
plot_nominal
plot_scaled
plot_growth
