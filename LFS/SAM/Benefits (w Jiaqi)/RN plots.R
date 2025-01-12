## Set up new plots for the RN on benefit changes using a SAM model
# Date: 6/01/2025
# Author: Matt Nolan
# Last edit: 7/01/2025

library(tidyverse)
library(data.table)
library(theme61)
library(readabs)
library(seasonal)
library(readxl)
library(readr)

benefit_50 <- read_csv("avgstats_b0.5.csv")
setDT(benefit_50)

benefit_50 <- benefit_50[1:6]

benefit_55 <- read_csv("avgstats_b0.55.csv")
setDT(benefit_55)

benefit_55 <- benefit_55[1:6]

benefit_60 <- read_csv("avgstats_b0.605.csv")
setDT(benefit_60)

benefit_60 <- benefit_60[1:6]

benefit_70 <- read_csv("avgstats_b0.7.csv")
setDT(benefit_70)

benefit_70 <- benefit_70[1:6]

benefit_77 <- read_csv("avgstats_b0.77.csv")
setDT(benefit_77)

benefit_77 <- benefit_77[1:6]

benefit_50[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)]
benefit_55[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)]
benefit_60[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)]
benefit_70[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)]
benefit_77[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)]

SAM_dt <- rbind(benefit_50[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)],
                benefit_55[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)],
                benefit_60[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)],
                benefit_70[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)],
                benefit_77[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)])

SAM_dt <- cbind(RR = c("50%","55%","60.5%","70%","77%"),SAM_dt)

plot_data <- melt(SAM_dt[,.(RR,`Job-Finding Rate`,`Separation Rate`)],id.vars = "RR")

ggplot(plot_data, aes(x = as.factor(RR), y = value, fill = as.factor(RR))) + 
  geom_col(position = "dodge") +
  facet_wrap(~ variable, scales = "free_y") +  
  labs(
    title = "Replacement Rate Distributions by Variable",
    x = "Replacement Rate + value of leisure",
    y = "Value"
  ) + scale_y_continuous_e61(labels = scales::percent_format())


plot_data_JFR <- melt(SAM_dt[,.(RR,`Job-Finding Rate`)],id.vars = "RR")

ggplot(plot_data, aes(x = as.factor(RR), y = value, fill = as.factor(RR))) + 
  geom_col(position = "dodge") +
  labs(
    title = "Job-Finding Rate for varying Replacement Rates",
    x = "Replacement Rate + value of leisure",
    y = "Value"
  ) + scale_y_continuous_e61(labels = scales::percent_format(),limits=c(0,0.5,0.1))



### Now lets do this for proper "replacement rate" changes - the above is hiking the value of leisure.

benefit_53 <- read_csv("avgstats_b0.53.csv")
setDT(benefit_53)

benefit_53 <- benefit_53[1:6]

benefit_56 <- read_csv("avgstats_b0.563.csv")
setDT(benefit_56)

benefit_56 <- benefit_56[1:6]

benefit_75 <- read_csv("avgstats_b0.75.csv")
setDT(benefit_75)

benefit_75 <- benefit_75[1:6]

benefit_53[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)]
benefit_56[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)]
benefit_75[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)]

SAM_dt2 <- rbind(benefit_50[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)],
                benefit_53[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)],
                benefit_56[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)],
                benefit_70[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)],
                benefit_75[Statistic == "Mean",.(Productivity,Unemployment,`Job-Finding Rate`,`Separation Rate`)])

SAM_dt2 <- cbind(RR = c("30%","33%","36.3%","50%","55%"),SAM_dt)

plot_data <- melt(SAM_dt2[,.(RR,`Job-Finding Rate`,`Separation Rate`,Unemployment)],id.vars = "RR")

ggplot(plot_data, aes(x = as.factor(RR), y = value, fill = as.factor(RR))) + 
  geom_col(position = "dodge") +
  facet_wrap(~ variable, scales = "free_y") +  
  labs(
    title = "Replacement Rate Distributions by Variable",
    x = "Replacement Rate",
    y = "Value"
  ) + scale_y_continuous_e61(labels = scales::percent_format())


plot_data_JFR <- melt(SAM_dt2[,.(RR,`Job-Finding Rate`)],id.vars = "RR")

ggplot(plot_data_JFR, aes(x = as.factor(RR), y = value, fill = as.factor(RR))) + 
  geom_col(position = "dodge") +
  labs_e61(
    #title = "Job-Finding Rate",
    subtitle = "Quarterly rate, average*",
    x = "Replacement Rate",
    y = "",
    footnotes = "Over the business cycle",
    sources = c("ABS","e61")
  ) + scale_y_continuous_e61(labels = scales::percent_format(),limits=c(0,0.5,0.1))

save_e61("JFR_SAM.pdf")

plot_data_JFR[1,3]/plot_data_JFR[2,3]-1 # Percentage drop in JFR from pre-COVID
plot_data_JFR[2,3]/plot_data_JFR[3,3]-1 # Percentage drop in JFR from now
plot_data_JFR[4,3]/plot_data_JFR[5,3]-1 # Percentage drop from US level

plot_data_SR <- melt(SAM_dt2[,.(RR,`Separation Rate`)],id.vars = "RR")

ggplot(plot_data_SR, aes(x = as.factor(RR), y = value, fill = as.factor(RR))) + 
  geom_col(position = "dodge") +
  labs_e61(
    #title = "Separation Rate",
    subtitle = "Quarterly rate, average*",
    x = "Replacement Rate",
    y = "",
    footnotes = "Over the business cycle",
    sources = c("ABS","e61")
  ) + scale_y_continuous_e61(labels = scales::percent_format(),limits=c(0,0.05,0.01))

save_e61("SR_SAM.pdf")

plot_data_SR[1,3]/plot_data_SR[2,3]-1 # Percentage drop in SR from pre-COVID
plot_data_SR[2,3]/plot_data_SR[3,3]-1 # Percentage drop in SR from now
plot_data_SR[4,3]/plot_data_SR[5,3]-1 # Percentage drop from US level


plot_data_UR <- melt(SAM_dt2[,.(RR,Unemployment)],id.vars = "RR")

ggplot(plot_data_UR, aes(x = as.factor(RR), y = value, fill = as.factor(RR))) + 
  geom_col(position = "dodge") +
  labs_e61(
    #title = "Unemployment Rate",
    subtitle = "Quarterly rate, average*",
    x = "Replacement Rate",
    y = "",
    footnotes = "Over the business cycle",
    sources = c("ABS","e61")
  ) + scale_y_continuous_e61(labels = scales::percent_format(),limits=c(0,0.12,0.02))

save_e61("UR_SAM.pdf")

plot_data_UR[1,3]
plot_data_UR[2,3]
plot_data_UR[3,3]

(plot_data_UR[2,3] - plot_data_UR[1,3])*100
(plot_data_UR[3,3] - plot_data_UR[2,3])*100
