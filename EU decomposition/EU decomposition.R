# Last edit: 22-07-2024
# Authors:  Zach H and Matt N
# Using age, gender, and state to think through decompositions of the change in EU rates in Australia.

rm(list=ls())
gc()

.libPaths(new = 'C:/Rpackage')

#library(devtools)

# devtools::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

library(tidyverse)
library(data.table)
library(theme61)
library(tidyr)
library(readxl)
library(gghighlight)
library(readabs)
library(seasonal)

### Transitions

## Check weights - have gone with "previous"

dfgf <- read_lfs_grossflows(weights = "previous")
setDT(dfgf)

colnames(dfgf)

unique(dfgf$state)

gf <- dfgf[,.(persons = sum(persons)),by=.(date,age,lfs_current,lfs_previous)] # sex,state.

gf_E <- gf[lfs_previous %in% c("Employed full-time","Employed part-time")][,.(emp_persons = sum(persons)),by=.(date,age)]

gf_EU <- gf[lfs_previous %in% c("Employed full-time","Employed part-time")][lfs_current %in% c("Unemployed")][,.(EU = sum(persons)),by=.(date,age)]

dt <- gf_E[gf_EU,on=.(date,age)][,.(date,age,prop = EU/emp_persons,EU,emp_persons)]

ggplot(dt,aes(x=date,y=prop,colour =age)) + geom_line()

dt[,age_start := substr(age,1,2)][,age_group := fcase(age_start <25,"15-24",
                                                      age_start < 35, "25-34",
                                                      age_start < 45, "35-44",
                                                         age_start >= 55, "55+",
                                                         default = "45-54")]


dt <- dt[,.(EU = sum(EU),emp_persons = sum(emp_persons)),by=.(age_group,date)]

#dt[,FY := fifelse(as.numeric(substr(date,6,7)) < 7,as.numeric(substr(date,1,4)),as.numeric(substr(6,7))-1)]

dt[, FY := ifelse(month(date) >= 7, year(date), year(date) - 1)]

dt <- dt[,.(EU = sum(EU),emp_persons = sum(emp_persons)),by=.(age_group,FY)]

total_EU <- dt[,.(total_EU = sum(EU),total_emp_persons = sum(emp_persons)),by=.(FY)] # Change FY to date if doing monthly



a <- total_EU[dt,on=.(FY)][,":=" (EU_rate_age = EU/emp_persons,total_EU_rate = total_EU/total_emp_persons,EU_share = EU/total_EU,emp_share = emp_persons/total_emp_persons)]

ggplot(a,aes(x=FY,y=EU_rate_age,fill=age_group)) + geom_col(position = "dodge")

#ggplot(a,aes(x=FY,y=EU_share,fill=age_group)) + geom_col(position = "dodge")

ggplot(a,aes(x=FY,y=emp_share,fill=age_group)) + geom_col(position = "dodge")

#ggplot(a,aes(x=FY,y=total_EU_rate)) + geom_col(position = "dodge")

initial_emp_share <- a[FY == 2005,.(age_group,initial_emp_share = emp_share)]

initial_EU_rate <- a[FY == 2005,.(age_group,initial_EU_rate = EU_rate_age)]

a <- initial_emp_share[a,on=.(age_group)]
a <- initial_EU_rate[a,on=.(age_group)]

# Our aggregate EU rate can be constructed as the sum of the individual rates weighted by their employment share. So we calculate a counterfactual by holding the initial share constant.

b <- a[,.(FY,age_group,total_EU_rate,counter_ER_fixed_share = EU_rate_age*initial_emp_share,counter_ER_fixed_rate = initial_EU_rate*emp_share)]

ggplot(melt(b[,.(actual = mean(total_EU_rate),CF = sum(counter_ER_fixed_share)),by=FY],id.vars = "FY"),aes(x=FY,y=value,colour=variable)) + geom_line() + labs_e61(title = "If population shares were fixed (2005)") + theme_e61(legend = "bottom")

ggplot(melt(b[,.(actual = mean(total_EU_rate),CF = sum(counter_ER_fixed_rate)),by=FY],id.vars = "FY"),aes(x=FY,y=value,colour=variable)) + geom_line() + labs_e61(title = "If EU rates were fixed (2005)") + theme_e61(legend = "bottom")

### Which rate change was most consequential?

c <- a[,.(FY,age_group,total_EU_rate,counter_ER_fixed_rate = initial_EU_rate*emp_share, fixed_rate = EU_rate_age*emp_share)]

calculate_sums <- function(dt) {
  unique_age_groups <- unique(dt$age_group)
  for (i in 1:length(unique_age_groups)) {
    dt[, paste0("counter_", unique_age_groups[i], "_ER_fixed") := sum(counter_ER_fixed_rate[unique_age_groups[i] != .SD$age_group]) + fixed_rate[unique_age_groups[i] == .SD$age_group], by =.(FY)]
  }
  return(dt)
}

c <- calculate_sums(c)

ggplot(melt(c[,.(actual = mean(total_EU_rate),CF = sum(counter_ER_fixed_rate),CF_15_vary = mean(`counter_15-24_ER_fixed`),CF_25_vary = mean(`counter_25-34_ER_fixed`),CF_35_vary = mean(`counter_35-44_ER_fixed`),CF_45_vary = mean(`counter_45-54_ER_fixed`),CF_55_vary = mean(`counter_55+_ER_fixed`)),by=FY],id.vars = "FY"),aes(x=FY,y=value,colour=variable)) + geom_line() + labs_e61(title = "If EU rates were fixed (2005)") + theme_e61(legend = "bottom")

colnames(c)



