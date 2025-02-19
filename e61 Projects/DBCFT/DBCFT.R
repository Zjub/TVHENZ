## A script that creates some plots for the DBCFT article
# Author: Matt Nolan
# Last edit: 19/02/2025

### Libraries and import data ----

library(tidyverse)
library(data.table)
library(theme61)
library(readxl)
library(readabs)

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Tax_take <- read_excel("PBO_based_tax.xlsx", sheet = "data")
setDT(Tax_take)

colnames(Tax_take) <- c("Type","FY23")

corp_value <- Tax_take[Type == "Corporate income", FY23]

# Create the two new rows
new_rows <- data.table(
  Type = c("Safe", "Risk"),
  FY23 = c(corp_value * 0.60, corp_value * 0.40)
)

# Remove the original Corporate income row
Tax_take <- Tax_take[Type != "Corporate income"]

# Add new rows
Tax_take <- rbind(Tax_take, new_rows)



Tax_take[,prop := FY23/sum(FY23)]

#Tax_take[,Type := factor(Type,levels=c("Individual income","Corporate income","GST","Other"))]
Tax_take[,Type := factor(Type,levels=c("Individual income","GST","Other","Safe","Risk"))]

ggplot(Tax_take, aes(y = prop, x = 1, fill = Type, alpha = Type != "Risk")) +
  geom_col() +
  scale_alpha_manual(values = c("TRUE" = 0.5, "FALSE" = 1)) +  # Transparent for others, solid for "At Risk"
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank(), # Remove x-axis ticks
    #legend.position = "bottom"      # Move legend for better readability
  ) +
  labs_e61(title = "Revenue at risk",subtitle = "Using Financial Year 2023",y="",x="") +
  scale_y_continuous_e61(labels=scales::percent_format()) +
  plab(c("Individual Tax","GST","Other","Corporate (safe)","At risk (10%)"),x=rep(1.5,times=5),y=c(0.6,0.45,0.3,0.17,0.05)) +
  scale_x_continuous(limits=c(0.5,2.2,1))

save_e61("Rev_risk.png",res=2,auto_scale = FALSE)


OECD_trade <- read_excel("OECD trade.xls", 
                         sheet = "OECD trade")
setDT(OECD_trade)

OECD_trade <- OECD_trade[REF_AREA %in% c("AUS","USA","CHN","GBR")]

OECD_trade[, c("Year", "Quarter") := tstrsplit(TIME_PERIOD, "-Q")]
OECD_trade[, c("Year", "Quarter") := tstrsplit(TIME_PERIOD, "-Q")]

OECD_trade[, Month := fifelse(Quarter == "1", "01",
                      fifelse(Quarter == "2", "04",
                              fifelse(Quarter == "3", "07", "10")))]

OECD_trade[, Date := as.Date(paste0(Year, "-", Month, "-01"))]

OECD_trade[, c("Year", "Quarter", "Month") := NULL]

ggplot(OECD_trade,aes(x=Date,y=TB_ratio*100,colour=REF_AREA)) + geom_line() +
  add_baseline() +
  #theme_e61(legend = "bottom") +
  scale_y_continuous_e61(limits = c(-40,30,10)) +
  labs_e61(title = "Merchandise trade surplus",subtitle="As a share of total gross trade", y="%") +
  plab(c("Australia","China","United Kingdom","United States"),x=rep(as.Date("2010-01-01"),times=4),y=c(15,25,-25,-35))
  
save_e61("Mer_trade.png",res=2)

save_e61("Mer_trade.jpg",res=2)

save_e61("Mer_trade.svg")
