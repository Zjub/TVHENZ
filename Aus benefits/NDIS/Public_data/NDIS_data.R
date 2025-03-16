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

## Import Dec 2024 dataset to investigate
# This is an example - there are other quarters of cost numbers available in the same folder, and also other items that can be investigated.

Dec_2024 <- read_csv("NDIS_CSVs/Participant_numbers_and_plan_budgets_data_December_2024_CSV_7MB.csv")
setDT(Dec_2024)

Dec_2024[StateCd == SrvcDstrctNm]

unique(Dec_2024[StateCd == "ALL" & SrvcDstrctNm == "ALL" & SuppClass == "ALL"]$DsbltyGrpNm)

dt <- Dec_2024[StateCd == "ALL" & SrvcDstrctNm == "ALL" & SuppClass == "ALL" & DsbltyGrpNm == "ALL"][,count := as.numeric(PrtcpntCnt)][,cost := AvgAnlsdCmtdSuppBdgt*count]

dt[,age_band := factor(AgeBnd,levels = c("0 to 8","9 to 14","15 to 18","19 to 24","25 to 34","35 to 44","45 to 54","55 to 64","65+","ALL"))]


ggplot(dt[AgeBnd != "ALL"],aes(x=age_band,y=cost/1000000)) + geom_col() +
  labs_e61(title = "NDIS cost by age group",subtitle = "In December 2024",y="$m",x="Age Band")

dt2 <- Dec_2024[StateCd == "ALL" & SrvcDstrctNm == "ALL" & SuppClass == "ALL" & DsbltyGrpNm %in% c("ALL","Autism")]

dt2[,count := as.numeric(PrtcpntCnt)][,cost := AvgAnlsdCmtdSuppBdgt*count]

dt_exa <- dt2[DsbltyGrpNm == "ALL"][dt2[DsbltyGrpNm == "Autism"],on =.(AgeBnd)][,.(RprtDt = RprtDt, StateCd = StateCd, SrvcDstrctNm = SrvcDstrctNm, DsbltyGrpNm = "Ex-Autism", AgeBnd = AgeBnd, SuppClass = SuppClass, AvgAnlsdCmtdSuppBdgt = AvgAnlsdCmtdSuppBdgt - i.AvgAnlsdCmtdSuppBdgt, PrtcpntCnt = as.numeric(PrtcpntCnt) - as.numeric(i.PrtcpntCnt),count = count - i.count,cost = cost - i.cost)]

dt2 <- rbind(dt2,dt_exa)

dt2[,age_band := factor(AgeBnd,levels = c("0 to 8","9 to 14","15 to 18","19 to 24","25 to 34","35 to 44","45 to 54","55 to 64","65+","ALL"))]

ggplot(dt2[AgeBnd != "ALL" & DsbltyGrpNm != "ALL"],aes(x=age_band,y=cost/1000000,fill = DsbltyGrpNm)) + geom_col() +
  labs_e61(title = "NDIS cost by age group",subtitle = "In December 2024",y="$m",x="Age Band") + theme_e61(legend = "bottom")


dt2[AgeBnd == "ALL" & DsbltyGrpNm == "Autism"]$cost/dt2[AgeBnd == "ALL" & DsbltyGrpNm == "ALL"]$cost
