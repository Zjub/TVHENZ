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
  labs_e61(title = "21 year olds receiving UB",subtitle = "Monthly recipient number (000s)",y="",x="",sources = c("ABS","e61")) + 
  scale_colour_manual(values = c(palette_e61(n=2)[1],palette_e61(n=2)[2])) + 
  scale_x_date(date_breaks = "2 years",date_labels = "%Y",limits=c(as.Date("2006-07-01"),as.Date("2017-12-01")),expand=c(0,0)) + 
  geom_vline(xintercept = as.Date("2012-07-01")) + 
  annotate("text",label="Synthetic",x=as.Date("2014-07-01"),y=22,colour=e61_palette(n=2)[2],size=4) + 
  annotate("text",label="Actual",x=as.Date("2014-07-01"),y=13,colour=e61_palette(n=2)[1],size=4)


#save_e61("SC_21.png",height=9,width=12)


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
  scale_y_continuous_e61(limits = c(4,))
  


#### Add some of the aggregate labour force data ----


