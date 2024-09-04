### Using different synthetic control estimates based on the aggregated cleared data from datalab.
# Last author: Matt Nolan
# Date data cleared: 29/06/2023
# Last edit: 02/09/2024

rm(list=ls())

# .libPaths(new = 'C:/Rpackage')

#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

library(tidyverse)
library(data.table)
library(collapse)
library(readabs)
library(readr)
library(readxl)
library(theme61)
library(gsynth)
library(Synth)
library(SCtools)
library(rvest)
library(zoo)

SCdata <- read_csv("SCdata.csv")[,1:8]

##### SC esetimate in Raw levels ----

### Synth example

basedata <- as.data.frame(SCdata) %>% mutate(time_index = rep(seq(1,144,by=1),times=11))

balancecheck <- basedata %>% pivot_wider(names_from = age,values_from = N)

dataprep.out <- 
  dataprep(
    basedata,
    dependent = "N",
    predictors = c("gender1ratio","NSWratio","VICratio","Queenratio","eduratio"),
    time.predictors.prior = 1:72, #as.Date("2006-07-01"):as.Date("2012-06-01"),
    unit.variable = "age",
    time.variable = "time_index",
    treatment.identifier = 21,
    controls.identifier = c(22:28),
    time.optimize.ssr = 1:72,
    time.plot = 1:144 #as.Date("2006-07-01"):as.Date("2018-06-01")
  )

synth.out <- synth(data.prep.obj = dataprep.out)

path.plot(synth.out, dataprep.out)

abline(v   = 73,
       lty = 2)


gaps.plot(synth.out, dataprep.out)

abline(v   = 73,
       lty = 2)

abline(h = -15000,
       lty = 3)


# Make manual graphs

actual_21 <- dataprep.out$Y1plot
synthetic_21 <- dataprep.out$Y0plot %*% synth.out$solution.w

date <- seq.Date(as.Date("2006-07-01"),as.Date("2018-06-01"),by="month")

graphdf <- data.frame(date = date,actual = actual_21[,1],synthetic = synthetic_21[,1]) %>% pivot_longer(!date,names_to = "variable",values_to = "value")

ggplot(graphdf,aes(x=date,y=value/1000,colour=variable)) + geom_line() + 
  labs_e61(subtitle = "Monthly recipient number (000s)",y="",x="") + 
  scale_colour_manual(values = c(palette_e61(n=2)[1],palette_e61(n=2)[2])) + 
  scale_x_date(date_breaks = "2 years",date_labels = "%Y",limits=c(as.Date("2006-07-01"),as.Date("2017-12-01")),expand=c(0,0)) + 
  geom_vline(xintercept = as.Date("2012-07-01")) + 
  plot_label(c("Actual 21 year olds","Synthetic 21 year olds"),x=c(as.Date("2007-01-01"),as.Date("2007-01-01")),y=c(12,14))


save_e61("SC_UB21.pdf",pad_width = 1)

ggplot(graphdf,aes(x=date,y=value/1000,colour=variable)) + geom_line() + 
  labs_e61(title = "Unemployment receipt for the young",subtitle = "Monthly recipient number (000s)",y="",x="",sources = c("ABS","e61")) + 
  scale_colour_manual(values = c(palette_e61(n=2)[1],palette_e61(n=2)[2])) + 
  scale_x_date(date_breaks = "2 years",date_labels = "%Y",limits=c(as.Date("2006-07-01"),as.Date("2017-12-01")),expand=c(0,0)) + 
  geom_vline(xintercept = as.Date("2012-07-01")) + 
  plot_label(c("Actual 21 year olds","Synthetic 21 year olds"),x=c(as.Date("2007-01-01"),as.Date("2007-01-01")),y=c(12,14))

save_e61("SC_UB21.png",pad_width = 1,res=2)


# Below is only in SCtools now - which isn't working https://bcastanho.github.io/install_sctools
# install.packages(c("cvTools", "furrr", "stringr","ggplot2",
#                    "stats","dplyr","magrittr","purrr"))

library(devtools)
#install_github('bcastanho/SCtools', repos =NULL, force = T)

library(SCtools)

placebos <- generate.placebos(dataprep.out, synth.out, Sigf.ipop = 3)

plot_placebos(placebos)

mspe.plot(placebos, discard.extreme = TRUE, mspe.limit = 1, plot.hist = TRUE)

### gsyth example

##### SC estimate for an index ----
# Part of the weighting is due to the level similarity between different groups - also useful to look at how these change if you use the proportion.

Base <- basedata %>% filter(month_year == "2006-07-01") %>% select(age,N)
basedata1 <- basedata %>% left_join(Base, by="age") %>% mutate(Prop = N.x/N.y)

### Synth example

dataprep.out2 <- 
  dataprep(
    basedata1,
    dependent = "Prop",
    predictors = c("gender1ratio","NSWratio","VICratio","Queenratio","eduratio"),
    time.predictors.prior = 1:72, #as.Date("2006-07-01"):as.Date("2012-06-01"),
    unit.variable = "age",
    time.variable = "time_index",
    treatment.identifier = 21,
    controls.identifier = c(22:28),
    time.optimize.ssr = 1:72,
    time.plot = 1:144 #as.Date("2006-07-01"):as.Date("2018-06-01")
  )

synth.out2 <- synth(data.prep.obj = dataprep.out2)

path.plot(synth.out2, dataprep.out2)

placebos2 <- generate.placebos(dataprep.out2, synth.out2, Sigf.ipop = 3)

plot_placebos(placebos2)


#### Survival rate plots ----

## Survival curves by year for FY12 and FY13

SC <- read.csv("SC1213.csv")
setDT(SC)

SC <- melt(SC)

SC[, period := as.integer(sub("X([12])_.*", "\\1", variable))]
SC[, age := as.integer(sub("X[12]_(.*)", "\\1", variable))]

SC[, time := seq_len(.N),by=.(variable)]

ggplot(SC[age %in% c(19,20,21,22,23)],aes(x=time,y=value,colour=as.factor(age))) + geom_line() + 
  facet_wrap(~ period) + 
  theme_e61(legend = "bottom") + 
  scale_y_continuous_e61(limits = c(0,1,0.2),labels=scales::percent_format())

setkey(SC, age, time, period)

SC_cast <- dcast(SC, age + time ~ period, value.var = "value", fill = NA, fun.aggregate = mean)

SC_cast[, diff := `2` - `1`]

ggplot(SC_cast,aes(x=time,y=diff,colour=as.factor(age))) + geom_line() + #[age %in% c(19,20,21,22,23)]
  theme_e61(legend = "bottom") + 
  scale_y_continuous_e61(labels=scales::percent_format(),limits=c(-0.3,0.1,0.1))

## Three and six month survival rates 2006 to 2018

SC_time <- read.csv("AgetimeSC.csv")
setDT(SC_time)

SC_time[,year := year + 2005]

SC_time <- melt(SC_time,id.vars = c("age","year"))

ggplot(SC_time,aes(x=year,y=value,colour=as.factor(age))) + geom_line() +
  theme_e61(legend = "bottom") +
  scale_y_continuous_e61(limits = c(0.1,0.8,0.2),labels=scales::percent_format(),y_top=FALSE) +
  labs_e61(y="") +
  facet_wrap(~ variable) + 
  geom_vline(xintercept = 2012,linetype="dotted")


#### Exit and Job Transition plots ----

exit_dt <- read.csv("Exitrate.csv")
setDT(exit_dt)

exit_dt[,date := as.IDate(datemonth)]

ggplot(exit_dt[IDage %in% c(19,20,21,22,23)],aes(x=date,y=exitrate,colour=as.factor(IDage))) + geom_line() +
  labs_e61(title="Exit Rate",y="") +
  geom_vline(xintercept = as.IDate("2012-07-01"),linetype="dotted") +
  theme_e61(legend = "bottom")

job_trans_dt <- read.csv("JobTrans.csv")
setDT(job_trans_dt)

emp_prob_dt <- melt(job_trans_dt[,.(age,year,prop_work_curr,prop_work_fut)],id.vars = c("age","year"))

ggplot(emp_prob_dt[age %in% c(19,20,21,22,23)],aes(x=year,y=value,colour=as.factor(age))) + geom_line() +
  facet_wrap(~variable) +
  theme_e61(legend = "bottom") +
  labs_e61(title = "Work probabilities",subtitle = "Recorded in PAYG in the Financial Year") +
  scale_y_continuous_e61(limits = c(0.2,0.8,0.2)) +
  geom_vline(xintercept = 2012,linetype="dotted")

cond_earnings_dt <- melt(job_trans_dt[,.(age,year,avg_labour_inc_curr,avg_labour_inc_fut)],id.vars = c("age","year"))

ggplot(cond_earnings_dt[age %in% c(19,20,21,22,23)],aes(x=year,y=value/1000,colour=as.factor(age))) + geom_line() +
  facet_wrap(~variable) +
  theme_e61(legend = "bottom") +
  scale_y_continuous_e61(limits = c(4,14,2)) +
  geom_vline(xintercept = 2012,linetype="dotted")
  


#### Add some of the aggregate labour force data ----

LS <- read_abs(cat_no = "6202.0") 
setDT(LS)

LS <- LS[series_type == "Seasonally Adjusted" | series_type == "Trend" ][grep("^Table (13|17|18)\\.", table_title)][series == "Unemployment rate ;  Persons ;"]

LS <- LS[, age_range := sub(".*status for ([0-9-]+) year olds.*", "\\1", table_title)][,.(date,series_type,age_range,value)]

ggplot(LS,aes(x=date,y=value,colour=age_range)) + geom_line() + theme_e61(legend = "bottom")

ggplot(LS[date >= as.Date("2010-01-01")], aes(x = date, y = value/100, colour = age_range, linetype = series_type)) +
  geom_line() +
  theme_e61(legend = "bottom") +
  scale_linetype_manual(values = c("Seasonally Adjusted" = "dashed", "Trend" = "solid")) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") + 
  labs_e61(title = "Unemployment rates", subtitle ="Monthly, percent of labour force", y="",sources = c("ABS")) +
  scale_y_continuous_e61(labels=scales::percent_format(),limits=c(0,0.25,0.05)) +
  plot_label(label = c("15-19","15-24","15-64"),x=c(as.Date("2022-01-01"),as.Date("2022-01-01"),as.Date("2022-01-01")),y=c(0.23,0.21,0.19))

ggplot(LS[date >= as.Date("2010-01-01")], aes(x = date, y = value/100, colour = age_range, linetype = series_type)) +
  geom_line() +
  scale_linetype_manual(values = c("Seasonally Adjusted" = "dashed", "Trend" = "solid")) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") + 
  labs_e61(subtitle ="Monthly, percent of labour force", y="") +
  scale_y_continuous_e61(labels=scales::percent_format(),limits=c(0,0.25,0.05)) +
  plot_label(label = c("15-19","15-24","15-64"),x=c(as.Date("2022-01-01"),as.Date("2022-01-01"),as.Date("2022-01-01")),y=c(0.23,0.21,0.19))


save_e61("Unemployment_Rates.pdf")

ggplot(LS[date >= as.Date("2017-01-01")], aes(x = date, y = value/100, colour = age_range, linetype = series_type)) +
  geom_line() +
  scale_linetype_manual(values = c("Seasonally Adjusted" = "dashed", "Trend" = "solid")) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") + 
  labs_e61(title="Youth Unemployment rising",subtitle ="Monthly, percent of labour force", y="",x="",sources = c("ABS","e61")) +
  scale_y_continuous_e61(labels=scales::percent_format(),limits=c(0,0.25,0.05)) +
  plot_label(label = c("15-19","15-24","15-64"),x=c(as.Date("2022-01-01"),as.Date("2022-01-01"),as.Date("2022-01-01")),y=c(0.23,0.21,0.19))

save_e61("Unemployment_Rates.png",res=2)

# Contribution

LS2 <- read_abs(cat_no = "6202.0") 
setDT(LS2)

LS2 <- LS2[series_type == "Seasonally Adjusted" | series_type == "Trend" ][grep("^Table (13|17|18)\\.", table_title)]


### Add in payment rate plots

legislation_url <- "https://guides.dss.gov.au/social-security-guide/5/2/1/20"
webpage <- read_html(legislation_url)

tables <- webpage %>% html_nodes("table") %>% html_table()

# Table 6 (3 on the site) is youth allowance with columns for both at home and not at home rates.  Table 9 is the UB (6 on the site).

YA <- tables[[6]]
setDT(YA)

YA <- YA[4:nrow(YA)][!30]

YA[, home_pay := as.numeric(gsub("[^0-9.]", "", `At home ($ pw)`))][,away_pay := as.numeric(`Away from home ($ pw)`)]

YA[30:.N,":=" (home_pay = home_pay/2, away_pay = away_pay/2)]
YA[, Date := as.Date(Date, format = "%d/%m/%Y")]

JSP <- tables[[9]]
setDT(JSP)

JSP <- JSP[9:.N][!59]
JSP[, JSP_pay := as.numeric(gsub("[^0-9.]", "", `Rate ($ pw)`))]
JSP[59:.N, JSP_pay := JSP_pay/2]
JSP[, Date := as.Date(Date, format = "%d/%m/%Y")]

all <- merge(YA[,.(Date,home_pay,away_pay)],JSP[,.(Date,JSP_pay)],by="Date",all=TRUE)

all[, JSP_pay := na.locf(JSP_pay, na.rm = FALSE)]
all[, away_pay := na.locf(away_pay, na.rm = FALSE)]
all[, home_pay := na.locf(home_pay, na.rm = FALSE)]

ggplot(all[Date >= as.Date("2010-01-01")], aes(x = Date)) +
  geom_line(aes(y = JSP_pay), colour = palette_e61(n=3)[1]) +
  geom_line(aes(y = away_pay), colour = palette_e61(n=3)[2]) +
  geom_line(aes(y = home_pay), colour = palette_e61(n=3)[3]) +
  labs_e61(subtitle = "Weekly Payment rates*",y="") + # Start is to point out this excludes the COVID Supplement
  scale_y_continuous_e61(labels=scales::dollar_format(),limits=c(100,400,50)) + 
  plot_label(c("JSP","YA away from home","YA at home"),y=c(370,340,310),x=c(as.Date("2010-01-01"),as.Date("2010-01-01"),as.Date("2010-01-01"))) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")

save_e61("Payment_rates.pdf")

ggplot(all[Date >= as.Date("2010-01-01")], aes(x = Date)) +
  geom_line(aes(y = JSP_pay), colour = palette_e61(n=3)[1]) +
  geom_line(aes(y = away_pay), colour = palette_e61(n=3)[2]) +
  geom_line(aes(y = home_pay), colour = palette_e61(n=3)[3]) +
  labs_e61(title = "Youth payments are lower",subtitle = "Weekly Payment rates*",y="",x="",sources = c("e61","Social Security Act")) + # Start is to point out this excludes the COVID Supplement
  scale_y_continuous_e61(labels=scales::dollar_format(),limits=c(100,400,50)) + 
  plot_label(c("JSP","YA away from home","YA at home"),y=c(370,340,310),x=c(as.Date("2010-01-01"),as.Date("2010-01-01"),as.Date("2010-01-01"))) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")

save_e61("Payment_rates.png",res=2)



