# Script to pull together the time series graphs and simple DiD for the VIC and non-VIC data.
# Author: Matt Nolan
# Date made: 30/12/2024
# Last update: 30/12/2024

library(tidyverse)
library(data.table)
library(theme61)
library(readxl)
library(fixest)

poly <- 1
bin_l <- 9
bin_r <- 36
treatment_number <- 2 # 1 for the March announcement, anything else for the September

if (treatment_number == 1){
  prop_JSP_matched_dt <- read.csv("VIC/VIC_JFR.csv")  # Main results JSP
  setDT(prop_JSP_matched_dt)
  
  treated_actual <- as.IDate("2020-03-22")
  treated <- as.IDate("2020-03-19")
  start <- as.IDate("2020-01-16") 
  #end <- as.IDate("2020-07-21") # Before the announcement
  end <- as.IDate("2020-06-23") # Before it is announced that there would be an announcement
} else {
  prop_JSP_matched_dt <- read.csv("VIC/VIC_July_JFR.csv")  # Main results JSP
  setDT(prop_JSP_matched_dt)
  
  # treated_actual <- as.IDate("2020-07-21")
  # treated <- as.IDate("2020-07-16")
  treated_actual <- as.IDate("2020-06-23")
  treated <- as.IDate("2020-06-18")
  
  start <- as.IDate("2020-04-02") 
  end <- as.IDate("2020-11-26") 
  
}

prop_JSP_matched_dt[,date := as.IDate(date)]
prop_JSP_matched_dt[,aus := fifelse(nz == 1,0,1)]

pre_announce_avg <- prop_JSP_matched_dt[date <= treated_actual,.(average = mean(prop)),by=.(nz)]
post_announce_avg <- prop_JSP_matched_dt[date > treated_actual,.(average = mean(prop)),by=.(nz)]

a <- ggplot(prop_JSP_matched_dt,aes(x=date,y=prop,colour=as.factor(nz))) + geom_line() + 
  geom_vline(xintercept = as.IDate("2020-06-25"),linetype="dashed") +
  #geom_vline(xintercept = as.IDate("2020-04-20"),linetype="dashed") +
  scale_y_continuous_e61(limits = c(0,0.16,0.04),labels = scales::percent_format()) +
  scale_colour_manual(values = c(palette_e61(2)[2],palette_e61(2)[1])) +
  labs_e61(title = "Victoria",
           subtitle = "By citizenship in Australia, matched",y="",
           sources = c("ABS","e61"),
           footnotes = c("Proportion of those out of work finding employment, weekly","Dotted lines refer to the average for the group, in the pre-period. Dashed is the post-period.")) +
  plab(c("Announcement"),x=c(as.IDate("2020-07-30")),y=c(0.025),colour=c("black")) +
  #plab(c("Announcement","Payment Increase"),x=c(as.IDate("2020-02-01"),as.IDate("2020-05-01")),y=c(0.05,0.13),colour=c("black","black")) +
  plab(c("Australian","New Zealand"),x=c(as.IDate("2020-04-01"),as.IDate("2020-04-01")),y=c(0.13,0.145),colour=c(palette_e61(2)[2],palette_e61(2)[1])) +
  geom_segment(data = pre_announce_avg, aes(x = start, xend = as.IDate("2020-06-25"), y = average, yend = average, colour = as.factor(nz)), linetype = "dotted",size = 1) +
  geom_segment(data = post_announce_avg, aes(x = as.IDate("2020-06-25"), xend = end, y = average, yend = average, colour = as.factor(nz)), linetype = "dashed",size = 1)

prop_JSP_matched_dt[,pre_post := fifelse(date <= as.IDate("2020-06-25"),0,1)]
feols(data = prop_JSP_matched_dt,prop ~ aus + pre_post + aus:pre_post)

if (treatment_number == 1){
  prop_JSP_matched_dt2 <- read.csv("VIC/nonVIC_JFR.csv")  # Main results JSP
  setDT(prop_JSP_matched_dt2)
  
  treated_actual <- as.IDate("2020-03-22")
  treated <- as.IDate("2020-03-19")
  start <- as.IDate("2020-01-16") 
  #end <- as.IDate("2020-07-21") # Before the announcement
  end <- as.IDate("2020-06-23") # Before it is announced that there would be an announcement
} else {
  prop_JSP_matched_dt2 <- read.csv("VIC/nonVIC_July_JFR.csv")  # Main results JSP
  setDT(prop_JSP_matched_dt2)
  
  # treated_actual <- as.IDate("2020-07-21")
  # treated <- as.IDate("2020-07-16")
  treated_actual <- as.IDate("2020-06-23")
  treated <- as.IDate("2020-06-18")
  
  start <- as.IDate("2020-04-02") 
  end <- as.IDate("2020-11-26") 
  
}

prop_JSP_matched_dt2[,date := as.IDate(date)]
prop_JSP_matched_dt2[,aus := fifelse(nz == 1,0,1)]

pre_announce_avg2 <- prop_JSP_matched_dt2[date <= treated_actual,.(average = mean(prop)),by=.(nz)]
post_announce_avg2 <- prop_JSP_matched_dt2[date > treated_actual,.(average = mean(prop)),by=.(nz)]

b <- ggplot(prop_JSP_matched_dt2,aes(x=date,y=prop,colour=as.factor(nz))) + geom_line() + 
  geom_vline(xintercept = as.IDate("2020-06-25"),linetype="dashed") +
  #geom_vline(xintercept = as.IDate("2020-04-20"),linetype="dashed") +
  scale_y_continuous_e61(limits = c(0,0.16,0.04),labels = scales::percent_format()) +
  scale_colour_manual(values = c(palette_e61(2)[2],palette_e61(2)[1])) +
  labs_e61(title = "Non-Victoria",
           subtitle = "By citizenship in Australia, matched",y="",
           sources = c("ABS","e61"),
           footnotes = c("Proportion of those out of work finding employment, weekly","Dotted lines refer to the average for the group, in the pre-period. Dashed is the post-period.")) +
  plab(c("Announcement"),x=c(as.IDate("2020-07-30")),y=c(0.025),colour=c("black")) +
  #plab(c("Announcement","Payment Increase"),x=c(as.IDate("2020-02-01"),as.IDate("2020-05-01")),y=c(0.05,0.13),colour=c("black","black")) +
  plab(c("Australian","New Zealand"),x=c(as.IDate("2020-04-01"),as.IDate("2020-04-01")),y=c(0.13,0.145),colour=c(palette_e61(2)[2],palette_e61(2)[1])) +
  geom_segment(data = pre_announce_avg2, aes(x = start, xend = as.IDate("2020-06-25"), y = average, yend = average, colour = as.factor(nz)), linetype = "dotted",size = 1) +
  geom_segment(data = post_announce_avg2, aes(x = as.IDate("2020-06-25"), xend = end, y = average, yend = average, colour = as.factor(nz)), linetype = "dashed",size = 1)

prop_JSP_matched_dt2[,pre_post := fifelse(date <= as.IDate("2020-06-25"),0,1)]
feols(data = prop_JSP_matched_dt2,prop ~ aus + pre_post + aus:pre_post)

a
b

save_e61(plotlist = list(a,b),"Victoria_JFR.pdf",res=2)
