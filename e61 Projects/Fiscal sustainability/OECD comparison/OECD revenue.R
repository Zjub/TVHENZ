# Topic: Pull revenue, and revenue relative to expense imbalance, from OECD consolidated data
# Author: Matt Nolan
# Created: 19/9/2025
# Last edit: 19/9/2025
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
                           sheet = "gtr_%_gdp", range = "B2:BC88")

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
  labs_e61(title = "Expenses") +
  theme_e61(legend = "bottom") 

dt[,gap := Expenses - Revenue]

ggplot(dt[year >= 1972],aes(x=year,y=gap,colour=level)) + geom_line() + 
  labs_e61(title = "Expenses") +
  theme_e61(legend = "bottom") 
