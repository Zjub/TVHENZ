# Topic: Grab some of the distributional information about STIK to talk about the distribution of benefits from core spending
# Author: Matt Nolan
# Created: 10/7/2025
# Last edit: 10/7/2025
# Last editor: Matt Nolan

rm(list=ls())

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



## Import data ----
# Note there are a set of rows called "all households" - only keeping to act as a cross-check on the base data, as all should have the same values.

STIK_dt <- read_excel("Expenditure plots.xlsx", 
                                sheet = "STIK")
setDT(STIK_dt)

STIK_dt

pop_dt <- STIK_dt[Measure == "Number"]
STIK_no_number <- STIK_dt[Measure != "Number"]

value_cols <- setdiff(names(STIK_no_number), c("Year", "Measure"))
pop_cols <- paste0(value_cols, "_pop")

pop_dt[, (value_cols) := lapply(.SD, function(x) x / 1000000), .SDcols = value_cols]

STIK_per_person <- merge(STIK_no_number, pop_dt, by = "Year", suffixes = c("", "_pop"))

STIK_per_person[, (value_cols) := Map(`/`, .SD[, value_cols, with=FALSE], .SD[, pop_cols, with=FALSE])]
STIK_per_person[, (pop_cols) := NULL]

STIK_per_person

## Create graphs
# Age
age_dt <- STIK_per_person[,.(Year,Measure,`15-24`,`25-34`,`35-44`, `45-54`, `55-64`, `65 and over`)]

age_dt_long <- melt(age_dt,id.vars = c("Year","Measure"))

ggplot(age_dt_long[Measure == "Health"],aes(x=Year,y=value,fill=variable)) + geom_col(position = "dodge") + 
  coord_flip() + theme_e61(legend = "bottom")

# Income

colnames(STIK_per_person)

income_dt <-STIK_per_person[,.(Year,Measure,`Lowest (EDHI)`,`Second (EDHI)`,`Third (EDHI)`,`Fourth (EDHI)`,`Highest (EDHI)`)]

income_dt_long <- melt(income_dt,id.vars = c("Year","Measure"))

ggplot(income_dt_long[Measure == "Health"],aes(x=Year,y=value,fill=variable)) + geom_col(position = "dodge") + 
  coord_flip() + theme_e61(legend = "bottom")

prop_income_dt <- income_dt_long[, .(
  total_spend = sum(value, na.rm=TRUE),
  highest_spend = sum(value[variable == "Highest (EDHI)"], na.rm=TRUE)
), by = .(Year, Measure)]

prop_income_dt[, top_income_share := highest_spend / total_spend]

ggplot(prop_income_dt[Measure == "Health"],aes(x=Year,y=top_income_share)) + geom_col(position = "dodge") + 
  coord_flip()

ggplot(prop_income_dt[Measure == "Education"],aes(x=Year,y=top_income_share)) + geom_col(position = "dodge") + 
  coord_flip()

ggplot(prop_income_dt[Measure == "Other"],aes(x=Year,y=top_income_share)) + geom_col(position = "dodge") + 
  coord_flip()

prop_income_dt$Measure <- factor(prop_income_dt$Measure, levels = c("Gross Disposable Income","Private Final Consumption","Other","Education", "Health"))

ggplot(prop_income_dt[Measure %in% c("Health","Education","Other")],aes(x=Year,y=top_income_share,fill=Measure)) + geom_col() + 
  coord_flip()


prop_income_dt_full <- income_dt_long[!Measure %in% c("Gross Disposable Income","Private Final Consumption")][, .(
  total_spend = sum(value, na.rm=TRUE),
  highest_spend = sum(value[variable == "Highest (EDHI)"], na.rm=TRUE)
), by = .(Year, Measure)]

prop_income_dt_full[, top_income_share := highest_spend / total_spend]

ggplot(prop_income_dt_full,aes(x=Year,y=top_income_share)) + geom_col(position = "dodge")

ggplot(prop_income_dt_full, aes(x = Year, y = top_income_share)) +
  geom_col(position = "dodge") +
  coord_cartesian(ylim = c(0.1, 0.2)) +  # Show y-axis from 10% to 100%
  scale_y_continuous(labels = scales::percent) +
  theme_e61()
