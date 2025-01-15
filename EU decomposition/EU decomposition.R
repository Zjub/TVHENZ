# Last edit: 22-07-2024
# Authors:  Zach H and Matt N
# Using age, gender, and state to think through decompositions of the change in EU rates in Australia.

rm(list=ls())
gc()

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


gf_EU[,.(EU_total = sum(EU)),by=.(date)][, .(EU_12_month_rolling_mean = frollmean(EU_total, n = 12, align = "right"))]

### Add Employment Services data

es_data <- read_csv("employment_services_data.csv")
setDT(es_data)
colnames(es_data) <- c("Date","Mainstream","Disability")

es_data[is.na(Disability)]$Disability <- 0

es_data[,total := Mainstream + Disability]

pop <- read_abs(cat_no = "3101.0")
setDT(pop)

unique(pop$series)

es_data$pop <- pop[series == "Estimated Resident Population ;  Persons ;  Australia ;" & month(date) == 6 & date >= as.Date("2005-06-01") & date <= as.Date("2022-07-01")]$value

es_data[,prop := total/pop]

pop[, age := as.numeric(gsub(".*;\\s*Persons\\s*;\\s*(\\d+)\\s*;", "\\1", series))]

unique(pop$age)

pop[!is.na(age) & age >= 15 & age <= 64 & month(date) == 6 & date >= as.Date("2005-06-01") & date <= as.Date("2022-07-01")]

wa_pop <- pop[table_no == 3101059 & !is.na(age) & age >= 15 & month(date) == 6 & date >= as.Date("2005-06-01") & date <= as.Date("2022-07-01"),.(wa_pop = sum(value)),by=.(date)]

es_data <- cbind(es_data,wa_pop)

es_data[,wa_prop := total/wa_pop]

lfs <- read_abs(cat_no = "6202.0")
setDT(lfs)
unique(lfs$series)

filtered_lfs <- lfs[grepl("15-64 years", series)]
unique(filtered_lfs$series)

UR_data <- filtered_lfs[series == "Unemployment rate ;  Persons ;  15-64 years ;" & series_type == "Seasonally Adjusted" & date >= as.Date("2004-07-01"),.(date,value)][,survey_year := fifelse(month(date) >= 7,year(date)+1,year(date))]

es_data <- cbind(es_data,UR_data[,.(UR = mean(value)/100),by=.(survey_year)][survey_year <= 2022])

melt(es_data[,.(date,wa_prop,UR)],id.vars = "date")

ggplot(melt(es_data[,.(date,wa_prop,UR)],id.vars = "date"),aes(x=date,y=value,colour=variable)) + geom_line() +
  labs_e61(title = "Employment Services are increasingly important",subtitle = "June year average, working age population",y="",x="") +
  plab(c("Employment Service use","Unemployment Rate"),x=c(as.Date("2005-07-01"),as.Date("2005-07-01")),y=c(0.075,0.065)) +
  scale_y_continuous_e61(labels = scales::percent_format(),limits = c(0.03,0.09,0.01)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y")

save_e61("ES_rate.png",res=2,pad_width = 1)
