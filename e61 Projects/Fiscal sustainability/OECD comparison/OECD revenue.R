# Topic: Pull revenue, and revenue relative to expense imbalance, from OECD consolidated data
# Author: Matt Nolan
# Created: 19/9/2025
# Last edit: 24/9/2025
# Last editor: Matt Nolan

#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(readxl)
library(tidyverse)
library(data.table)
library(Hmisc)
library(tidysynth)
library(readabs)

rm(list=ls())


## Conditions

year_data <- 1999 # Find countries available from a given year.
outliers <- c("Ireland") # Remove countries

### Data ----

OECD_exp_GDP <- read_excel("table4_gov_exp-gdp.xlsx", 
                           sheet = "exp_%_gpd", range = "B2:BC88")

setDT(OECD_exp_GDP)
setnames(OECD_exp_GDP, c("...1", "...2"), c("country", "level"))

# Collapse by country
# OECD_dt <- OECD_exp_GDP[, lapply(.SD, sum,na.rm=TRUE), by = country, .SDcols = patterns("^\\d{4}$")]
# setDT(OECD_dt)
# 
# OECD_dt[,.(country,`2022`)][order(`2022`)]

exp_dt <- OECD_exp_GDP[country == "Australia" & level != "Local"][,country := NULL]

exp <- melt(exp_dt,id.vars = "level",value.name = "Expenses")

OECD_rev_GDP <- read_excel("table6_gov_rev-gdp.xlsx", 
                           sheet = "gtr_%_gdp", range = "B2:BJ88")

setDT(OECD_rev_GDP)
setnames(OECD_rev_GDP, c("...1", "...2"), c("country", "level"))

rev_dt <- OECD_rev_GDP[country == "Australia" & level != "Local"][,country := NULL]

rev <- melt(rev_dt,id.vars = "level",value.name = "Revenue")

dt <- exp[rev,on=.(level,variable)]

dt[,year := as.numeric(variable) + 1964]


ggplot(dt[year >= 1972],aes(x=year,y=Expenses,colour=level)) + geom_line() + 
  labs_e61(title = "Expenses") +
  theme_e61(legend = "bottom") # Note, this isn't by "spent" this is just raw spending data


ggplot(dt[year >= 1972],aes(x=year,y=Revenue,colour=level)) + geom_line() + 
  labs_e61(title = "Revenue") +
  theme_e61(legend = "bottom") 

ggplot(dt[year >= 2002],aes(x=year,y=Revenue,colour=level)) + geom_line() + 
  labs_e61(title = "Revenue by Government level",
           y= "% NGDP",
           sources = c("OECD","e61")) +
  scale_y_continuous_e61(limits=c(10,26,4)) +
  plab(c("Federal","Non-Federal"),x=c(2009,2009),y=c(21,13))

save_e61("Revenue_split.png",res=2)

dt[year == 2014]

dt[,gap := Expenses - Revenue]

ggplot(dt[year >= 1972],aes(x=year,y=gap,colour=level)) + geom_line() + 
  labs_e61(title = "Expenses - Revenue") +
  theme_e61(legend = "bottom") 

total_rev <- dt[,.(rev = sum(Revenue,na.rm=TRUE)),by=.(year)]

# Implied by OECD statistics
ggplot(total_rev[year >= 2002],aes(x=year,y=rev)) + geom_line()

gdp_data <- read_abs(cat_no = "5206.0", tables = "1")
setDT(gdp_data)

gdp_data <- gdp_data[series == "Gross domestic product: Current prices ;" & series_type == "Original"][,year := year(date)]

gdp_data[,month := as.numeric(month(date))]

gdp_data[,FY := fcase(month < 7 , as.numeric(year),
                      default = as.numeric(year) + 1)]

ann_nom_GDP <- gdp_data[,.(GDP = sum(value)),by=.(FY)]

total_rev <- total_rev[ann_nom_GDP,on=.(year=FY)]

total_rev[,.(rev_total = rev*GDP,year=year)]

# Cross check against GFS - reckon better to use GFS for the introductory section.
# Did this manually, it isn't exactly the same but close.

#### OECD country comparison "role of consolidation" plot for revenue

rev_int <-melt(OECD_rev_GDP,id.vars = c("country","level"))[!is.na(value)]

rev_int[,year := as.numeric(variable) + 1964]

total_rev_int <- rev_int[year == 2022][,rev := value] #[,.(rev = sum(value)),by=.(country)]


total_order <- total_rev_int[, .(total_value = sum(value, na.rm=TRUE)), by = country][order(total_value)]$country
total_rev_int[, country := factor(country,
                                    levels = total_order)]
total_rev_int[, level := factor(level,levels = c("Central","State","Local"))]

total_rev_int[level == "Central", level := "Federal"]

total_rev_int[level %in% c("State", "Local"), level := "Non-Federal"]

total_rev_int <- total_rev_int[, .(value = sum(value, na.rm = TRUE)), 
                                by = .(country, level)]

total_rev_int[, level := factor(level, levels = c("Non-Federal","Federal"))]

## Ranking place

ranking_dt <- total_rev_int[,.(value = sum(value)),by=.(country)]

ranking_dt[order(value)]

## Overall plot
ggplot(total_rev_int[country %in% c("France","Finland","Austria","Greece","Belgium","Sweden","United Kingdom","Denmark","Luxembourg","Portugal","Slovak Republic","Netherlands","United States","Norway","Australia","Israel","Lithuania","Switzerland","Ireland")], aes(x = country, y = value, fill = level)) +
  geom_col() +
  geom_col(
    data = total_rev_int[country == "Australia"],
    aes(x = country, y = value, group = level),
    fill = NA, color = "gold", linewidth = 2, inherit.aes = TRUE
  ) +
  coord_flip() +
  labs_e61(title = "Revenue: Cross-country (2022)",
           y = "% NGDP",
           subtitle = "",
           footnotes = c("Dark blue is spending by Federal Govt % GDP. Light blue is additional spending attributed to non-Federal entities.")) +
  plab(c("Non-Federal","Federal"),x=c(1.5,3.5),y=c(40,40))

save_e61("Consolidation_cc_rev.png",res=2)
