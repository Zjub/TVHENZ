# Script to pull together initial RDD based on aggregate exit rates - doing for the second discontinuity
# Author: Matt Nolan
# Date made: 31/10/2024
# Last update: 31/10/2024


library(tidyverse)
library(data.table)
library(theme61)
library(rdrobust)
library(readxl)

rm(list=ls())

#prop_JSP_matched_dt <- read_excel("PSM_spec1.xlsx") # The "simplifed" matching model for placebo.
prop_JSP_matched_dt <- read_excel("PSM_spec2.xlsx") # Placebo including "non-personal" variables.
#prop_JSP_matched_dt <- insert main # Main results JSP
setDT(prop_JSP_matched_dt)


# treated_actual <- as.IDate("2020-07-21")
# treated <- as.IDate("2020-07-16")
treated_actual <- as.IDate("2020-06-23")
treated <- as.IDate("2020-06-18")


poly <- 1
bin_l <- 9
bin_r <- 36


start <- as.IDate("2020-04-02") 
end <- as.IDate("2020-11-26") 


prop_JSP_matched_dt[,date := as.IDate(date)]

rdplotting <- function(prop_JSP_matched, treatment_day, treatment_date, polynomial, bin_left, bin_right, periods_drop){
  # Construct Plot objects
  date_diff <- as.numeric(treatment_day - treatment_date)
  
  temp <- prop_JSP_matched
  
  if (periods_drop > 0){
    temp <- temp[event_time <= 0 | event_time > periods_drop*7]
  }
  
  rdplot_nz <- rdplot(temp[nz == 1]$prop,
                      temp[nz == 1]$event_time,
                      c = date_diff,
                      p = polynomial,
                      nbins = c(bin_left, bin_right - periods_drop),
                      title = "NZ Job Finding Rate RD")
  
  rdplot_aus <- rdplot(temp[nz == 0]$prop,
                       temp[nz == 0]$event_time,
                       c = treatment_day - treatment_date,
                       p = polynomial,
                       nbins = c(bin_left, bin_right - periods_drop),
                       title = "Aus Job Finding Rate RD")
  
  # Combine the elements
  rd_nz_plotline <- as.data.table(rdplot_nz$vars_poly)
  
  rd_nz_plotline[rdplot_x == date_diff,rdplot_y := NA]
  
  rd_nz_plotline[,nz_line := rdplot_y][,rdplot_y := NULL]
  
  rd_nz_plotdata <- as.data.table(rdplot_nz$vars_bins)[,.(rdplot_x = rdplot_mean_x, nz_dot = rdplot_mean_y)]
  nz_plot_data <- merge(rd_nz_plotdata, rd_nz_plotline, by="rdplot_x", all=TRUE)
  
  ggplot(nz_plot_data, aes(x=rdplot_x)) +
    geom_point(aes(y=nz_dot)) +
    geom_line(aes(y=nz_line)) +
    geom_vline(xintercept = date_diff, linetype = "dashed")
  
  rd_aus_plotline <- as.data.table(rdplot_aus$vars_poly)
  
  rd_aus_plotline[rdplot_x == date_diff,rdplot_y := NA]
  
  rd_aus_plotline[,aus_line := rdplot_y][,rdplot_y := NULL]
  
  rd_aus_plotdata <- as.data.table(rdplot_aus$vars_bins)[,.(rdplot_x = rdplot_mean_x, aus_dot = rdplot_mean_y)]
  aus_plot_data <- merge(rd_aus_plotdata, rd_aus_plotline, by="rdplot_x", all=TRUE)
  
  ggplot(aus_plot_data, aes(x=rdplot_x)) +
    geom_point(aes(y=aus_dot)) +
    geom_line(aes(y=aus_line)) +
    geom_vline(xintercept = date_diff, linetype = "dashed")
  
  all_data <- merge(aus_plot_data, nz_plot_data, by="rdplot_x", all=TRUE)
  
  plot_result <- ggplot(all_data, aes(x=rdplot_x)) +
    geom_point(aes(y=aus_dot, colour = "Aus")) +
    geom_point(aes(y=nz_dot, colour = "NZ")) +
    geom_line(aes(y=aus_line, colour = "Aus")) +
    geom_line(aes(y=nz_line, colour = "NZ")) +
    geom_vline(xintercept = date_diff, linetype = "dashed") +
    labs(title = "Job Finding Rates, optimal bandwidth")
  
  return(list(all_data, plot_result))
}

prop_JSP_matched_dt[,event_time := date - treated]
prop_JSP_matched_dt <- prop_JSP_matched_dt[date >= start & date <= end]


rd_res <- rdplotting(prop_JSP_matched = prop_JSP_matched_dt, treatment_day = treated_actual, treatment_date = treated, polynomial = poly, bin_left=bin_l, bin_right = bin_r, periods_drop = 0)
rd_res_drop <- rdplotting(prop_JSP_matched = prop_JSP_matched_dt, treatment_day = treated_actual, treatment_date = treated, polynomial = poly, bin_left=bin_l, bin_right = bin_r, periods_drop = 2)

rd_res
rd_res_drop

### Estimate model on differences

diff_data <- prop_JSP_matched_dt[nz == 0][prop_JSP_matched_dt[nz == 1],on=.(date)][,.(date,event_time,prop=prop - i.prop)]

generate_plot <- function(data, cutoff, treatment_day = treated_actual, treatment_date = treated, bin_left = bin_l, bin_right = bin_r, periods_drop = 0){
  date_diff <- as.numeric(treatment_day - treatment_date)
  
  temp <- data
  
  if (periods_drop > 0){
    temp <- temp[event_time <= 0 | event_time > periods_drop*7]
  }
  
  rdplot_all <- rdplot(temp$prop,
                       temp$event_time,
                       c = treatment_day - treatment_date,
                       p = 1,
                       nbins = c(bin_left, bin_right - periods_drop),
                       title = "Diff Job Finding Rate RD")
  
  rd_all_plotline <- as.data.table(rdplot_all$vars_poly)
  
  print(rd_all_plotline[rdplot_x >= -7 & rdplot_x <= 7])
  
  rd_all_plotline[rdplot_x == 1, rdplot_y := NA]
  
  rd_all_plotline[, all_line := rdplot_y][, rdplot_y := NULL]
  
  rd_all_plotdata <- as.data.table(rdplot_all$vars_bins)[, .(rdplot_x = rdplot_mean_x, all_dot = rdplot_mean_y)]
  all_plot_data <- merge(rd_all_plotdata, rd_all_plotline, by = "rdplot_x", all = TRUE)
  
  plot <- ggplot(all_plot_data, aes(x = rdplot_x)) +
    geom_point(aes(y = all_dot)) +
    geom_line(aes(y = all_line)) +
    geom_vline(xintercept = date_diff, linetype = "dashed") + labs_e61(title = "Difference in Job Finding Rates (Aussie - NZ)",y="") +
    scale_y_continuous_e61(labels=scales::percent_format(),limits=c(-0.1,0.02,by=0.02)) +
    plab("XXppt drop in relative Australian JFR",x=14,y=-0.01) + add_baseline()
  
  return(plot)
}

diff_rdd <- generate_plot(diff_data)

diff_rdd

prop_JSP_matched_dt[date < "2020-03-01" & nz == 0,.(mean(prop))]

# 7.3/8.7
# 6.9/8.7

diff_rdd_drop2 <- generate_plot(diff_data,periods_drop = 2)

diff_rdd_drop2
