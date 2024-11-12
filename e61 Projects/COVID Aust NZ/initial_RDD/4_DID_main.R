# Script to pull together the time series graphs and simple DiD
# Author: Matt Nolan
# Date made: 12/11/2024
# Last update: 12/11/2024

library(tidyverse)
library(data.table)
library(theme61)
library(readxl)
library(fixest)

poly <- 1
bin_l <- 9
bin_r <- 36
treatment_number <- 1 # 1 for the March announcement, anything else for the September

if (treatment_number == 1){
  prop_JSP_matched_dt <- read.csv("JFR_by_group 1 .csv")  # Main results JSP
  setDT(prop_JSP_matched_dt)
  
  treated_actual <- as.IDate("2020-03-22")
  treated <- as.IDate("2020-03-19")
  start <- as.IDate("2020-01-16") 
  #end <- as.IDate("2020-07-21") # Before the announcement
  end <- as.IDate("2020-06-23") # Before it is announced that there would be an announcement
} else {
  prop_JSP_matched_dt <- read.csv("JFR_by_group 2 .csv")  # Main results JSP
  setDT(prop_JSP_matched_dt)
  
  # treated_actual <- as.IDate("2020-07-21")
  # treated <- as.IDate("2020-07-16")
  treated_actual <- as.IDate("2020-06-23")
  treated <- as.IDate("2020-06-18")
  
  start <- as.IDate("2020-04-02") 
  end <- as.IDate("2020-11-26") 
  
}

prop_JSP_matched_dt[,date := as.IDate(date)]

pre_announce_avg <- prop_JSP_matched_dt[date <= as.IDate("2020-03-19"),.(average = mean(prop)),by=.(nz)]
post_announce_avg <- prop_JSP_matched_dt[date > as.IDate("2020-03-19"),.(average = mean(prop)),by=.(nz)]

ggplot(prop_JSP_matched_dt,aes(x=date,y=prop,colour=as.factor(nz))) + geom_line() + 
  geom_vline(xintercept = as.IDate("2020-03-19"),linetype="dashed") +
  geom_vline(xintercept = as.IDate("2020-04-20"),linetype="dashed") +
  scale_y_continuous_e61(limits = c(0,0.16,0.04),labels = scales::percent_format()) +
  scale_colour_manual(values = c(palette_e61(2)[1],palette_e61(2)[2])) +
  labs_e61(title = "Job Finding Rates",subtitle = "By citizenship in Australia, matched") +
  plab(c("Announcement","Payment Increase"),x=c(as.IDate("2020-02-01"),as.IDate("2020-05-01")),y=c(0.05,0.13),colour=c("black","black")) +
  plab(c("Australian","New Zealander"),x=c(as.IDate("2020-02-01"),as.IDate("2020-02-01")),y=c(0.125,0.145),colour=c(palette_e61(2)[1],palette_e61(2)[2])) +
  geom_segment(data = pre_announce_avg, aes(x = start, xend = as.IDate("2020-03-19"), y = average, yend = average, colour = as.factor(nz)), linetype = "dashed") +
  geom_segment(data = post_announce_avg, aes(x = as.IDate("2020-03-19"), xend = end, y = average, yend = average, colour = as.factor(nz)), linetype = "dashed")

post_announce_avg_xJul <- prop_JSP_matched_dt[date > as.IDate("2020-03-19") & date < as.IDate("2020-07-01"),.(average = mean(prop)),by=.(nz)]

# As our second announcement is in July lets cut this back (that early July outlier is worth abstracting from - as it only increases our estimates)
ggplot(prop_JSP_matched_dt[date < as.IDate("2020-07-01")],aes(x=date,y=prop,colour=as.factor(nz))) + geom_line() + 
  geom_vline(xintercept = as.IDate("2020-03-19"),linetype="dashed") +
  #geom_vline(xintercept = as.IDate("2020-04-20"),linetype="dashed") +
  scale_y_continuous_e61(limits = c(0,0.16,0.04),labels = scales::percent_format()) +
  scale_colour_manual(values = c(palette_e61(2)[1],palette_e61(2)[2])) +
  labs_e61(title = "Job Finding Rates*",
           subtitle = "By citizenship in Australia, matched",y="",
           sources = c("ABS","e61"),
           footnotes = c("Proportion of those out of work finding employment, weekly")) +
  plab(c("Announcement"),x=c(as.IDate("2020-02-01")),y=c(0.05),colour=c("black")) +
  #plab(c("Announcement","Payment Increase"),x=c(as.IDate("2020-02-01"),as.IDate("2020-05-01")),y=c(0.05,0.13),colour=c("black","black")) +
  plab(c("Australian","New Zealander"),x=c(as.IDate("2020-02-01"),as.IDate("2020-02-01")),y=c(0.125,0.145),colour=c(palette_e61(2)[1],palette_e61(2)[2])) +
  geom_segment(data = pre_announce_avg, aes(x = start, xend = as.IDate("2020-03-19"), y = average, yend = average, colour = as.factor(nz)), linetype = "dashed",size = 1) +
  geom_segment(data = post_announce_avg_xJul, aes(x = as.IDate("2020-03-19"), xend = end, y = average, yend = average, colour = as.factor(nz)), linetype = "dashed",size = 1)

prop_JSP_matched_dt[,pre_post := fifelse(event_time <= 0,0,1)]

# Just use a regression to estimate the difference. In datalab will need to do a full TWFE estimate.
feols(data = prop_JSP_matched_dt[date < as.IDate("2020-07-01")],prop ~ nz + pre_post + nz:pre_post)


diff_data <- prop_JSP_matched_dt[nz == 0][prop_JSP_matched_dt[nz == 1],on=.(date)][,.(date,event_time,prop=prop - i.prop)]

ggplot(diff_data,aes(x=date,y=prop)) + geom_line() + 
  geom_vline(xintercept = as.IDate("2020-03-19"),linetype="dashed") +
  geom_vline(xintercept = as.IDate("2020-04-20"),linetype="dashed") +
  scale_y_continuous_e61(limits = c(-0.1,0.02,0.02),labels = scales::percent_format()) +
  plab(c("Announcement","Payment Increase"),x=c(as.IDate("2020-02-01"),as.IDate("2020-05-01")),y=c(-0.05,-0.01),colour=c("black","black")) +
  geom_hline(yintercept = 0)
  
