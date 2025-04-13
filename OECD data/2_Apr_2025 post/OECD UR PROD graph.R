## Last update:  2/04/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan 
# Unemployment and productivity comparisons for blog post

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


OECD_df <- read_csv("OECD.SDD.TPS,DSD_LFS@DF_IALFS_INDIC,1.0+NZL+USA+AUS.UNE_LF...Y._T.Y_GE15..Q.csv")
setDT(OECD_df)

OECD_dt <- OECD_df[,.(REF_AREA,TIME_PERIOD,OBS_VALUE)][, date := as.Date(paste0(substr(TIME_PERIOD, 1, 4), "-", 
                                                                                  match(substr(TIME_PERIOD, 6, 7), c("Q1", "Q2", "Q3", "Q4")) * 3, "-01"))]

ggplot(OECD_dt,aes(x=date,y=OBS_VALUE,colour=REF_AREA)) + geom_line() +
  geom_vline(xintercept = as.Date("2020-06-01"),linetype="dashed") +
  geom_vline(xintercept = as.Date("2021-03-01"),linetype="dashed",colour="gold") +
  labs_e61(title = "Unemployment Rates",
           subtitle = "Quarterly, seasonally adjusted",
           y="",
           x="") +
  scale_y_continuous_e61(limits = c(0,14,4),y_top = FALSE) +
  plab(label = c("Aus","NZ","US"),x=rep(as.Date("2018-03-01")),y=c(11,10,9)) +
  plab(label = c("NZ WS End","Aus JK End"),x=c(as.Date("2018-09-01"),as.Date("2021-05-01")),y=c(1,1),colour = c("black","gold"))

save_e61("UR.png",res=2)
