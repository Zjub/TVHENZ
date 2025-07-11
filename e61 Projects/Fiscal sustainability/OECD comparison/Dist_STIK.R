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

age_dt <- STIK_per_person[,.(Year,Measure,`15-24`,`25-34`,`35-44`, `45-54`, `55-64`, `65 and over`)]

age_dt_long <- melt(age_dt,id.vars = c("Year","Measure"))
