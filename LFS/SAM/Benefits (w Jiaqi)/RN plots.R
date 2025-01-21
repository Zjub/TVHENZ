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

#### Add the longer form data

avgstats_b <- read_csv("avgstats_b.csv")
setDT(avgstats_b)

colnames(avgstats_b)

avgstats_b[,":=" (JFR_PC = `Job-Finding Rate`/shift(`Job-Finding Rate`) - 1,ben_change =(b-0.2)/(b-0.21) - 1)][,implied_elasticity := JFR_PC/ben_change]

ggplot(avgstats_b,aes(x=(b-0.2),y=`Job-Finding Rate`)) + geom_line() +
  labs_e61(title = "Job-Finding Rate",y="",x="Replacement Rates") +
  scale_y_continuous_e61(limits = c(0.3,0.42,0.02),labels=scales::percent_format())

save_e61("JFR_time_series.pdf",pad_width = 1)

ggplot(avgstats_b,aes(x=(b-0.2),y=`Separation Rate`)) + geom_line() +
  labs_e61(title = "Separation Rate",y="",x="Replacement Rates") +
  scale_y_continuous_e61(limits = c(0.00,0.04,0.01),labels=scales::percent_format())

save_e61("SR_time_series.pdf",pad_width = 1)

ggplot(avgstats_b,aes(x=(b-0.2),y=`Unemployment`)) + geom_line() +
  labs_e61(title = "Unemployment Rate",y="",x="Replacement Rates") +
  scale_y_continuous_e61(limits = c(0.00,0.1,0.02),labels=scales::percent_format())

save_e61("UR_time_series.pdf",pad_width = 1)

ggplot(avgstats_b,aes(x=(b-0.2),y=implied_elasticity*(-1))) + geom_line() +
  labs_e61(title = "Job-Finding Elasticity",subtitle = "% decline relative to benefit change",y="",x="Replacement Rates") +
  scale_y_continuous_e61(limits = c(0.00,0.8,0.1),labels=scales::percent_format())

save_e61("JFR_elasticity_time_series.pdf",pad_width = 1)

ggplot(avgstats_b,aes(x=(b-0.2),y=`J2J Transition Rate`)) + geom_line() +
  labs_e61(title = "Job-to-Job Rate",y="",x="Replacement Rates") +
  scale_y_continuous_e61(limits = c(0.00,0.04,0.01),labels=scales::percent_format())

save_e61("J2J_time_series.pdf",pad_width = 1)


a <- avgstats_b[b %in% c(0.5,0.53,0.61,0.7)]

ggplot(a, aes(x = as.factor((b - 0.2) * 100), y = `Job-Finding Rate`, fill = as.factor(b))) + 
  geom_col() +
  scale_x_discrete() +
  #labs_e61(title = "Job-Finding Rate", y = "") +
  labs_e61(subtitle = "Quarterly Rate", y = "") +
  scale_y_continuous_e61(labels = scales::percent_format(), limits = c(0, 0.5, 0.1)) +
  scale_fill_manual(values = c(palette_e61(4)[1], palette_e61(4)[2], palette_e61(4)[3], palette_e61(4)[4]))

save_e61("JFR_policy.pdf",pad_width = 1)


ggplot(a, aes(x = as.factor((b - 0.2) * 100), y = `Job-Finding Rate`, fill = as.factor(b))) + 
  geom_col() +
  scale_x_discrete() +
  labs_e61(title = "Job-Finding Rate", y = "",subtitle = "Quarterly Rate",x="Replacement Rate") +
  #labs_e61(subtitle = "Quarterly Rate", y = "") +
  scale_y_continuous_e61(labels = scales::percent_format(), limits = c(0, 0.5, 0.1)) +
  scale_fill_manual(values = c(palette_e61(4)[1], palette_e61(4)[2], palette_e61(4)[3], palette_e61(4)[4]))

save_e61("JFR_policy.png",res=2,pad_width = 1)





ggplot(a,aes(x=as.factor((b-0.2)*100),y=implied_elasticity*(-1),fill=as.factor(b))) + geom_col() +
  scale_x_discrete() +
  #labs_e61(title = "Job-Finding Rate Elasticity",subtitle = "% response relative to benefit change",y="") +
  labs_e61(subtitle = "% response relative to benefit change",y="") +
  scale_y_continuous_e61(limits=c(0,0.7,0.1)) +
  scale_fill_manual(values = c(palette_e61(4)[1], palette_e61(4)[2], palette_e61(4)[3], palette_e61(4)[4]))

save_e61("JFR_elasticity_policy.pdf",pad_width = 1)

ggplot(a,aes(x=as.factor((b-0.2)*100),y=`Separation Rate`,fill=as.factor(b))) + geom_col() +
  scale_x_discrete() +
  #labs_e61(title = "Separation Rate",y="") +
  labs_e61(subtitle = "Quarterly Rate",y="") +
  scale_y_continuous_e61(labels=scales::percent_format(),limits=c(0,0.03,0.005)) +
  scale_fill_manual(values = c(palette_e61(4)[1], palette_e61(4)[2], palette_e61(4)[3], palette_e61(4)[4]))

save_e61("SR_policy.pdf",pad_width = 1)

ggplot(a,aes(x=as.factor((b-0.2)*100),y=`Separation Rate`,fill=as.factor(b))) + geom_col() +
  scale_x_discrete() +
  labs_e61(title = "Separation Rate",y="",subtitle = "Quarterly Rate",x="Replacement Rate") +
  #labs_e61(subtitle = "Quarterly Rate",y="") +
  scale_y_continuous_e61(labels=scales::percent_format(),limits=c(0,0.03,0.005)) +
  scale_fill_manual(values = c(palette_e61(4)[1], palette_e61(4)[2], palette_e61(4)[3], palette_e61(4)[4]))

save_e61("SR_policy.png",pad_width = 1,res=2)


ggplot(a,aes(x=as.factor((b-0.2)*100),y=`Unemployment`,fill=as.factor(b))) + geom_col() +
  scale_x_discrete() +
  #labs_e61(title = "Unemployment Rate",y="") +
  labs_e61(subtitle = "Quarterly Rate",y="") +
  scale_y_continuous_e61(labels=scales::percent_format(),limits=c(0,0.07,0.01)) +
  scale_fill_manual(values = c(palette_e61(4)[1], palette_e61(4)[2], palette_e61(4)[3], palette_e61(4)[4]))

save_e61("UR_policy.pdf",pad_width = 1)

ggplot(a,aes(x=as.factor((b-0.2)*100),y=`J2J Transition Rate`,fill=as.factor(b))) + geom_col() +
  scale_x_discrete() +
  #labs_e61(title = "Job-to-Job Rate",y="") +
  labs_e61(subtitle = "Quarterly Rate",y="") +
  scale_y_continuous_e61(labels=scales::percent_format(),limits=c(0,0.04,0.005)) +
  scale_fill_manual(values = c(palette_e61(4)[1], palette_e61(4)[2], palette_e61(4)[3], palette_e61(4)[4]))

save_e61("J2J_policy.pdf",pad_width = 1)

0.3910432/0.3987658 -1
0.3685505/0.3987658 -1
0.3388891/0.3685505 - 1

50/41-1

0.04120393/0.04043685 -1


a

### Unrelated SEEK LM mismatch information

# Load required library
library(ggplot2)

# Create the data frame
Seek_data <- data.frame(
  Quarter = seq(as.Date("2016-10-01"), as.Date("2023-10-01"), by = "quarter"),
  Mismatch = c(
    26.5, 26.7, 26.9, 27.1, 27.3, 27.6, 27.9, 28.2, 28.5, 28.8, 29.2, 29.6, 30.0,
    29.4, 28.8, 28.2, 27.6, 27.0, 26.5, 26.2, 26.0, 26.5, 27.1, 27.8, 28.5, 29.2, 30.0, 30.9, 31.7
  )
)

# Plot the data
ggplot(Seek_data, aes(x = Quarter, y = Mismatch)) +
  geom_line() +
  labs_e61(
    title = "SEEK Labour Market Mismatch Indicator",
    subtitle = "Quarterly",
    y = "%",
    x = "",
    footnotes = "SEEK trend estimate.",
    sources = c("SEEK")
  ) +
  scale_y_continuous_e61(limits = c(25, 32,1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

save_e61("SEEK_mismatch.png",res=2)

