## Create plots for prior GFC Wage Scarring work and put out data publicly.
## Author:  Matt Nolan
## Date: 26/02/2024

## Setup ----

rm(list=ls())

library(pacman)

p_load(
  tidyverse,
  data.table,
  collapse,
  readabs,
  readr,
  readxl,
  mFilter,
  zoo
)

library(theme61)

### Base wage scarring

GFC_ES_base <- read_csv("Data/FY2009_reg_No_reemp.csv")
setDT(GFC_ES_base)

ggplot(GFC_ES_base[t >= -5],aes(x=t,y=estimate)) + geom_point() +
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high),width =0.2,colour = palette_e61(3)[1]) +
  geom_line() +
  labs_e61(title = "Level Wage Scar",y="($)",x="Year from Event")

GFC_base_earnings <- read_csv("Data/FY2009_avg_No_reemp.csv")
setDT(GFC_base_earnings)

GFC_base_earnings[,t := fyear - 2009]

prop_ES <- GFC_base_earnings[GFC_ES_base[t >= -5],on=.(t)]

prop_ES <- prop_ES[,":=" (prop = estimate/(-estimate + wageinc_treated),lci = conf.low/(-estimate + wageinc_treated),hci = conf.high/(-estimate + wageinc_treated))]

ggplot(prop_ES,aes(x=t,y=prop)) + geom_point() +
  geom_errorbar(aes(ymin = lci,ymax = hci),width =0.2,colour = palette_e61(3)[1]) +
  geom_line() +
  labs_e61(title = "Wage Scar",y="(%)",x="Year from Event")


### Matched ES - as same treated group the proportional calculations are not affected

GFC_ES_base_matched <- read_csv("Data/match_FY2009_reg_No_reemp.csv")
setDT(GFC_ES_base_matched)

prop_ES_matched <- GFC_base_earnings[GFC_ES_base_matched[t >= -5],on=.(t)]

prop_ES_matched <- prop_ES_matched[,":=" (prop = estimate/(-estimate + wageinc_treated),lci = conf.low/(-estimate + wageinc_treated),hci = conf.high/(-estimate + wageinc_treated))]

ggplot(prop_ES_matched,aes(x=t,y=prop*100)) + geom_point() +
  geom_errorbar(aes(ymin = lci*100,ymax = hci*100),width =0.2,colour = palette_e61(3)[1]) +
  geom_line() +
  labs_e61(#title = "Global Financial Crisis Wage Scars","Matched sample*",
           y="(%)",x="Year from Job Loss",
           sources = c("ABS","e61"),
           footnotes = c("Control sample matched to treated units using 1:1 Propensity Score Matching. Propensities calculated using wage income in 2006 and 2007, age, gender, and the number of employees at the initial firm.")) +
  add_baseline() +
  scale_y_continuous_e61(limits = c(-30,10,by=10))

save_e61("Wage_scar.png",res=2)

save_e61("Wage_scar.svg")

### Reemployed group

GFC_1yrloss_earnings <- read_csv("Data/FY2009_avg_1yr_stayend.csv")
setDT(GFC_1yrloss_earnings)

GFC_1yrloss_earnings[,t := fyear - 2009]


GFC_5yrloss_earnings <- read_csv("Data/FY2009_avg_5yr_stayend.csv")
setDT(GFC_5yrloss_earnings)

GFC_5yrloss_earnings[,t := fyear - 2009]

# 1 year
GFC_ES_1yrloss_matched <- read_csv("Data/match_FY2009_reg_1yr_stayend.csv")
setDT(GFC_ES_1yrloss_matched)

prop_ES_1yr_matched <- GFC_1yrloss_earnings[GFC_ES_1yrloss_matched[t >= -5],on=.(t)]

prop_ES_1yr_matched <- prop_ES_1yr_matched[,":=" (prop_1yr = estimate/(-estimate + wageinc_treated),lci_1yr = conf.low/(-estimate + wageinc_treated),hci_1yr = conf.high/(-estimate + wageinc_treated))]

ggplot(prop_ES_1yr_matched,aes(x=t,y=prop_1yr*100)) + geom_point() +
  geom_errorbar(aes(ymin = lci_1yr*100,ymax = hci_1yr*100),width =0.2,colour = palette_e61(3)[1]) +
  geom_line() +
  labs_e61(title = "Global Financial Crisis Wage Scars","Matched sample*, re-employed within a year",y="(%)",x="Year from Job Loss",
           sources = c("ABS","e61"),
           footnotes = c("Control sample matched to treated units using 1:1 Propensity Score Matching. Propensities calculated using wage income in 2006 and 2007, age, gender, and the number of employees at the initial firm.")) +
  add_baseline() +
  scale_y_continuous_e61(limits = c(-30,10,by=10))

# 5 year
GFC_ES_5yrloss_matched <- read_csv("Data/match_FY2009_reg_5yr_stayend.csv")
setDT(GFC_ES_5yrloss_matched)

prop_ES_5yr_matched <- GFC_5yrloss_earnings[GFC_ES_5yrloss_matched[t >= -5],on=.(t)]

prop_ES_5yr_matched <- prop_ES_5yr_matched[,":=" (prop_5yr = estimate/(-estimate + wageinc_treated),lci_5yr = conf.low/(-estimate + wageinc_treated),hci_5yr = conf.high/(-estimate + wageinc_treated))]

ggplot(prop_ES_5yr_matched,aes(x=t,y=prop_5yr*100)) + geom_point() +
  geom_errorbar(aes(ymin = lci_5yr*100,ymax = hci_5yr*100),width =0.2,colour = palette_e61(3)[1]) +
  geom_line() +
  labs_e61(title = "Global Financial Crisis Wage Scars","Matched sample*, re-employed within five years",y="(%)",x="Year from Job Loss",
           sources = c("ABS","e61"),
           footnotes = c("Control sample matched to treated units using 1:1 Propensity Score Matching. Propensities calculated using wage income in 2006 and 2007, age, gender, and the number of employees at the initial firm.")) +
  add_baseline() +
  scale_y_continuous_e61(limits = c(-30,10,by=10))

## Joint graph

dt_5yr <-prop_ES_5yr_matched[,.(t,prop = prop_5yr,lci = lci_5yr,hci = hci_5yr,variable = "yr5")]
dt_1yr <-prop_ES_1yr_matched[,.(t,prop = prop_1yr,lci = lci_1yr,hci = hci_1yr,variable = "yr1")]
dt_noyr <-prop_ES_matched[,.(t,prop,lci,hci,variable = "noyr")]

graph_dt1 <- rbind(dt_5yr,dt_1yr)

ggplot(graph_dt1,aes(x=t,y=prop*100,colour=variable,group=variable)) + geom_point() +
  geom_errorbar(aes(ymin = lci*100,ymax = hci*100),width =0.2,linetype="dashed") +
  geom_line() +
  labs_e61(title = "Global Financial Crisis Wage Scars","Matched sample*, re-employed within the period",y="(%)",x="Year from Job Loss",
           sources = c("ABS","e61"),
           footnotes = c("Control sample matched to treated units using 1:1 Propensity Score Matching. Propensities calculated using wage income in 2006 and 2007, age, gender, and the number of employees at the initial firm.")) +
  add_baseline() +
  scale_y_continuous_e61(limits = c(-30,10,by=10)) +
  scale_x_continuous_e61(limits = c(-5,10,1),hide_first_last = FALSE) +
  plab(c("One year","Five years"),x=c(3,3),y=c(-13,-18)) +
  geom_vline(xintercept = -1,linetype = "dashed")

save_e61("Wage_reemp.png",res=2)

graph_dt2 <- rbind(dt_5yr,dt_1yr,dt_noyr)

ggplot(graph_dt2,aes(x=t,y=prop*100,colour=variable,group=variable)) + geom_point() +
  geom_errorbar(aes(ymin = lci*100,ymax = hci*100),width =0.2,linetype="dashed") +
  geom_line() +
  labs_e61(#title = "Global Financial Crisis Wage Scars",
           subtitle = "Matched sample*",
           y="(%)",x="Year from Job Loss",
           sources = c("ABS","e61"),
           footnotes = c("Control sample matched to treated units using 1:1 Propensity Score Matching. Propensities calculated using wage income in 2006 and 2007, age, gender, and the number of employees at the initial firm.")) +
  add_baseline() +
  scale_y_continuous_e61(limits = c(-40,10,by=10)) +
  scale_x_continuous_e61(limits = c(-5,10,1),hide_first_last = FALSE) +
  plab(c("All individuals","One year","Five years"),x=c(3,3,3),y=c(-23,-28,-33)) +
  geom_vline(xintercept = -1,linetype = "dashed")

save_e61("Wage_reemp2.png",res=2)
save_e61("Wage_reemp2.svg")

