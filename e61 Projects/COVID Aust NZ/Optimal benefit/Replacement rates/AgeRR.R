### Adding in the age RR data from internal calculations.
## Data not stored centrally for security - will need a synthetic version for final paper.
# Author: Matt Nolan
# Date made: 6/01/2025
# Last update: 6/01/2025

library(tidyverse)
library(data.table)
library(theme61)
library(readxl)
library(readr)

rm(list=ls())
gc()

Age_RR <- read_csv("Age_RR.csv")
setDT(Age_RR)

ggplot(Age_RR[Age != "65+"],aes(x=Year,y=Median,colour=Age)) + geom_line() +
  labs_e61(title = "Age-based Replacement Rates")

ggplot(Age_RR[Age != "65+"],aes(x=Year,y=Median,fill=Age)) + geom_col(position = "dodge") +
  labs_e61(title = "Age Net Replacement Rates*",subtitle="Benefit replacement of median weekly earnings",y="",sources=c("ABS","e61"),footnotes = "Net JobSeeker payment relative to net median labour earnings by age group.") + 
  scale_y_continuous_e61(limits = c(0,1,0.2),labels=scales::percent_format()) +
  plab(label = c("24 and Under","25 to 34","35 to 44","45 to 54","55 to 64","Total"),x=rep(2018,times=6),y=c(0.85,0.78,0.71,0.64,0.57,0.51))

save_e61("Age_RR_time.png",res=2,pad_width = 1)

MW_RR <- read_csv("MW_RR.csv")
setDT(MW_RR)

MW_RR[,year := fifelse(year < 30, 2000 + as.numeric(year),1900 +as.numeric(year))]

ggplot(melt(MW_RR,id.vars = "year"),aes(x=year,y=value,colour=variable)) + geom_line() +
  scale_y_continuous_e61(limits = c(0,1,0.2),labels=scales::percent_format()) +
  labs_e61(title = "Full-time Net Replacement Rates",subtitle = "Full-time (38hrs) for the full year",y="",x="") +
  plab(c("Minimum Wage","Average Wage"),x=c(2008,2008),y=c(0.76,0.68)) +
  scale_x_continuous(breaks = seq(min(MW_RR$year), max(MW_RR$year), by = 3))

save_e61("MW_RR_time.png",res=2,pad_width = 1)

ggplot(melt(MW_RR,id.vars = "year"),aes(x=year,y=value,colour=variable)) + geom_line() +
  scale_y_continuous_e61(limits = c(0,1,0.2),labels=scales::percent_format()) +
  labs_e61(subtitle = "Full-time (38hrs) for the full year",y="",x="",sources = c("ABS","e61")) +
  plab(c("Minimum Wage","Average Wage"),x=c(2008,2008),y=c(0.76,0.68)) +
  scale_x_continuous(breaks = seq(min(MW_RR$year), max(MW_RR$year), by = 3))

save_e61("MW_RR_time.pdf")

### 

ggplot(MW_RR,aes(x=year,y=MW)) + geom_line() +
  scale_y_continuous_e61(limits = c(0,1,0.2),labels=scales::percent_format()) +
  labs_e61(title = "Minimum Wage Net Replacement Rates",subtitle = "Full-time (38hrs) for the full year",y="",x="") +
  scale_x_continuous(breaks = seq(min(MW_RR$year), max(MW_RR$year), by = 3))

EIAC <- MW_RR[year == 2024]$MW*(1.25) 

ACOSS <- MW_RR[year == 2024]$MW*(1.47) 

ggplot(MW_RR, aes(x = year, y = MW)) + 
  geom_line() +
  geom_point(aes(x = 2024, y = EIAC), color = "red", size = 3) +  # EIAC point
  geom_point(aes(x = 2024, y = ACOSS), color = "blue", size = 3) +  # ACOSS point
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  labs(title = "Minimum Wage Net Replacement Rates",
       subtitle = "Single, full-time (38hrs) for the full year",
       y = "", x = "") +
  plab(c("EIAC (64%)","ACOSS (75%)"),x=c(2008,2008),y=c(0.60,0.71),colour=c("red","blue")) +
  scale_x_continuous(breaks = seq(min(MW_RR$year), max(MW_RR$year), by = 4))

save_e61("MW_RR_only_withproposals.png",res=2,pad_width = 1,auto_scale = FALSE)

ggplot(MW_RR, aes(x = year, y = MW)) + 
  geom_line() +
  #geom_point(aes(x = 2024, y = EIAC), color = "red", size = 3) +  # EIAC point
  #geom_point(aes(x = 2024, y = ACOSS), color = "blue", size = 3) +  # ACOSS point
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  labs_e61(#title = "Minimum Wage Net Replacement Rates",
       subtitle = "Single, full-time (38hrs) for the full year",
       y = "", x = "",
       sources = c("e61","ABS")) +
  #plab(c("EIAC (64%)","ACOSS (75%)"),x=c(2008,2008),y=c(0.60,0.71),colour=c("red","blue")) +
  scale_x_continuous(breaks = seq(min(MW_RR$year), max(MW_RR$year), by = 4))

save_e61("MW_RR_only.png",res=2,pad_width = 1,auto_scale = FALSE)
save_e61("MW_RR_only.pdf",pad_width = 1,auto_scale = FALSE)
