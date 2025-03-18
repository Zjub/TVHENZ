## Last update:  17/03/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan 
# Investigate spending data

library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(tidyverse)
library(data.table)
library(readxl)
library(Hmisc)

rm(list=ls())
gc()

col_names <- c("RprtDt","StateCd","SrvcDstrctNm","DsbltyGrpNm","AgeBnd","SuppClass","AvgAnlsdCmtdSuppBdgt","PrtcpntCnt","count","cost","age_band","datasets")  

full_table <- data.table(matrix(ncol = length(col_names),nrow=0))

colnames(full_table) <- col_names

## Import Dec 2024 dataset to investigate
# This is an example - there are other quarters of cost numbers available in the same folder, and also other items that can be investigated.

datasets <- list.files("NDIS_CSVs", pattern = "^Participant_numbers.*\\.csv$", full.names = TRUE)

for (i in c(seq(1:6),8,9)){
  df <- read_csv(datasets[i])
  setDT(df)
  
  if ("ActvPrtcpnt" %in% names(df)) {
    setnames(df, "ActvPrtcpnt", "PrtcpntCnt")
  }
  
  df[StateCd == SrvcDstrctNm]
  
  unique(df[StateCd == "ALL" & SrvcDstrctNm == "ALL" & SuppClass == "ALL"]$DsbltyGrpNm)
  
  dt <- df[StateCd == "ALL" & SrvcDstrctNm == "ALL" & SuppClass == "ALL" & DsbltyGrpNm == "ALL"][,count := as.numeric(PrtcpntCnt)][,cost := AvgAnlsdCmtdSuppBdgt*count]
  
  dt[,age_band := factor(AgeBnd,levels = c("0 to 8","9 to 14","15 to 18","19 to 24","25 to 34","35 to 44","45 to 54","55 to 64","65+","ALL"))]
  
  
  ggplot(dt[AgeBnd != "ALL"],aes(x=age_band,y=cost/1000000)) + geom_col() +
    labs_e61(title = "NDIS cost by age group",subtitle = "In December 2024",y="$m",x="Age Band")
  
  dt2 <- df[StateCd == "ALL" & SrvcDstrctNm == "ALL" & SuppClass == "ALL" & DsbltyGrpNm %in% c("ALL","Autism")]
  
  dt2[,count := as.numeric(PrtcpntCnt)][,cost := AvgAnlsdCmtdSuppBdgt*count]
  
  dt_exa <- dt2[DsbltyGrpNm == "ALL"][dt2[DsbltyGrpNm == "Autism"],on =.(AgeBnd)][,.(RprtDt = RprtDt, StateCd = StateCd, SrvcDstrctNm = SrvcDstrctNm, DsbltyGrpNm = "Ex-Autism", AgeBnd = AgeBnd, SuppClass = SuppClass, AvgAnlsdCmtdSuppBdgt = AvgAnlsdCmtdSuppBdgt - i.AvgAnlsdCmtdSuppBdgt, PrtcpntCnt = as.numeric(PrtcpntCnt) - as.numeric(i.PrtcpntCnt),count = count - i.count,cost = cost - i.cost)]
  
  dt2 <- rbind(dt2,dt_exa)
  
  dt2[,age_band := factor(AgeBnd,levels = c("0 to 8","9 to 14","15 to 18","19 to 24","25 to 34","35 to 44","45 to 54","55 to 64","65+","ALL"))]
  dt2[,datasets := datasets[i]]
  
  ggplot(dt2[AgeBnd != "ALL" & DsbltyGrpNm != "ALL"],aes(x=age_band,y=cost/1000000,fill = DsbltyGrpNm)) + geom_col() +
    labs_e61(title = "NDIS cost by age group",subtitle = "In December 2024",y="$m",x="Age Band") + theme_e61(legend = "bottom")
  
  
  dt2[AgeBnd == "ALL" & DsbltyGrpNm == "Autism"]$cost/dt2[AgeBnd == "ALL" & DsbltyGrpNm == "ALL"]$cost
  
  colnames(dt2)
  
  full_table <- rbind(full_table,dt2)
}

full_table[, month_year := gsub(".*_([A-Za-z]+_[0-9]{4})_.*", "\\1", datasets)]
full_table[, month_year := gsub("_", " ", month_year)]  # Replace "_" with space

unique(full_table$month_year)

full_table[,month_year := fifelse(month_year == "NDIS CSVs/Participant numbers and plan budgets data March 2024CSV 68MB.csv", "March 2024",month_year)]

full_table[,month_year := factor(month_year,levels = c("March 2023","June 2023","September 2023","December 2023","March 2024","June 2024","September 2024","December 2024"))]

ggplot(full_table[AgeBnd != "ALL" & DsbltyGrpNm == "ALL"],aes(x=age_band,y=cost/1000000,fill = month_year)) + geom_col(position = "dodge") +
  labs_e61(title = "NDIS cost by age group",subtitle = "In December 2024",y="$m",x="Age Band") + theme_e61(legend = "bottom")



