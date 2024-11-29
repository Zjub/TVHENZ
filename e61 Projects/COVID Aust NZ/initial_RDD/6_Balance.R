# Script to pull together the balance statistics for our exercise
# Author: Matt Nolan
# Date made: 29/11/2024
# Last update: 29/11/2024

library(tidyverse)
library(data.table)
library(theme61)
library(readxl)
library(readr)
library(fixest)

## Age balance

age_balance <- read_csv("age_balance.csv")
setDT(age_balance)

ggplot(age_balance,aes(x=age_bin,y=result_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,10000,2000)) + plab(c("Australian","New Zealander"),x=c("35-44","35-44"),y=c(8800,7200)) +
  labs_e61(title = "Age distribution",subtitle = "Matched JSP dataset",y="",x="",sources=c("ABS","e61"))

ggplot(age_balance,aes(x=age_bin,y=matched_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,30000,6000)) + plab(c("Australian","New Zealander"),x=c("35-44","35-44"),y=c(27000,23000)) +
  labs_e61(title = "Age distribution",subtitle = "Full Matched dataset",y="",x="",sources=c("ABS","e61"))


## Industry balance

ind_balance <- read_csv("ind_balance.csv")
setDT(ind_balance)

ggplot(ind_balance,aes(x=industry,y=result_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,8000,2000)) + plab(c("Australian","New Zealander"),x=c("A","A"),y=c(5400,4600)) +
  labs_e61(title = "Industry distribution",subtitle = "Matched JSP dataset",y="",x="",sources=c("ABS","e61"))

ggplot(ind_balance,aes(x=industry,y=matched_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,20000,4000)) + plab(c("Australian","New Zealander"),x=c("A","A"),y=c(17000,14000)) +
  labs_e61(title = "Industry distribution",subtitle = "Full Matched dataset",y="",x="",sources=c("ABS","e61"))


## Occupation balance

occ_balance <- read_csv("occ_balance.csv")
setDT(occ_balance)

ggplot(occ_balance,aes(x=occ,y=result_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,7000,2000),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(0,0),y=c(5400,4600)) +
  labs_e61(title = "Occupation distribution",subtitle = "Matched JSP dataset",y="",x="",sources=c("ABS","e61")) +
  scale_x_continuous(breaks = seq(0, 9, 1))

ggplot(occ_balance,aes(x=occ,y=matched_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,22000,4000),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(0,0),y=c(19000,17000)) +
  labs_e61(title = "Occupation distribution",subtitle = "Full Matched dataset",y="",x="",sources=c("ABS","e61")) +
  scale_x_continuous(breaks = seq(0, 9, 1))

## region balance

region_balance <- read_csv("region_balance.csv")
setDT(region_balance)

ggplot(region_balance,aes(x=region,y=result_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,9000,2000),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(4,4),y=c(7400,6600)) +
  labs_e61(title = "Region distribution",subtitle = "Matched JSP dataset",y="",x="",sources=c("ABS","e61"))

ggplot(region_balance,aes(x=region,y=matched_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,30000,7000),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(4,4),y=c(20000,17000)) +
  labs_e61(title = "Region distribution",subtitle = "Full Matched dataset",y="",x="",sources=c("ABS","e61"))

# Also add relative size of each region in each of the samples. [will want to do this for each balance - so there are three balance plots, two comparing the balance of the samples, and one comparing it to the aggregate data]

region_shares <- region_balance[, .(
  total_result_data = sum(result_data),
  total_matched_data = sum(matched_data),
  total_all_data = sum(all_data)
), by = region]

region_shares[, `:=`(
  share_result_data = total_result_data / sum(total_result_data),
  share_matched_data = total_matched_data / sum(total_matched_data),
  share_all_data = total_all_data / sum(total_all_data)
)]

region_plot <- melt(region_shares[,.(region,share_result_data,share_matched_data,share_all_data)],id.vars = "region")

ggplot(region_plot,aes(x=region,y=value,fill=variable)) + geom_col(position="dodge") + 
  plab(c("Matched JSP","Matched Total","Total Aus-NZ"),x = c("4","4","4"),y=c(0.28,0.25,0.22)) +
  scale_y_continuous_e61(labels=scales::percent_format(),limits = c(0,0.38,0.1),y_top = FALSE) +
  labs_e61(title = "Proportion of sample by region",x="",y="",sources=c("ABS","e61"))


