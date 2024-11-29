# Replotting the 2022 bunching work to be used in slides
# Author: Matt Nolan
# Date made: 28/11/2024
# Last update: 28/11/2024

library(tidyverse)
library(data.table)
library(theme61)
library(readxl)
library(fixest)
library(readr)

STP_2022 <- read_csv("STP2recipient2022.csv")
setDT(STP_2022)

Dom_full_2022 <- read_csv("DOMContincweekrecipient2022.csv")
setDT(Dom_full_2022)

Dom_regular_2022 <- read_csv("DOMFullincweekrecipient2022.csv")
setDT(Dom_regular_2022)

ggplot(melt(STP_2022, id.vars = "Weekly_Earnings"),aes(x=Weekly_Earnings,y=value,colour=variable)) + geom_line() + geom_vline(xintercept = 75,linetype = "dashed") +
  scale_y_continuous_e61(limits = c(0,12000,2000)) +
  labs_e61(subtitle = "Tax records of JSP recipients")

ggplot(melt(Dom_full_2022, id.vars = "Weekly_Earnings"),aes(x=Weekly_Earnings,y=value,colour=variable)) + geom_line() + geom_vline(xintercept = 75,linetype = "dashed") +
  scale_y_continuous_e61(limits = c(0,1200,200)) +
  labs_e61(subtitle = "DSS earning records of JSP recipients")

ggplot(melt(Dom_regular_2022, id.vars = "Weekly_Earnings"),aes(x=Weekly_Earnings,y=value,colour=variable)) + geom_line() + geom_vline(xintercept = 75,linetype = "dashed") +
  scale_y_continuous_e61(limits = c(0,60000,5000)) +
  labs_e61(subtitle = "DSS regular earning records of JSP recipients")


# The counterfactuals include the "adjustment" for round number bias - where we included a dummy to estimate it. We can back that out by just taking the average of the counterfactual at the other two points.
# We also remove the density close to zero in the published version for clarity, but not sure there is much point - cutting zero makes sense because it is a different margin!.

STP_data <- melt(STP_2022, id.vars = "Weekly_Earnings")
DOM_data <- melt(Dom_regular_2022, id.vars = "Weekly_Earnings")

# Thresholds: 45,99,147 (dummies for bunching at $50s)

STP_data[variable == "CF" & Weekly_Earnings == 45]$value <- STP_data[variable == "CF" & Weekly_Earnings %in% c(39,51),.(value = mean(value))]
STP_data[variable == "CF" & Weekly_Earnings == 99]$value <- STP_data[variable == "CF" & Weekly_Earnings %in% c(93,105),.(value = mean(value))]
STP_data[variable == "CF" & Weekly_Earnings == 147]$value <- STP_data[variable == "CF" & Weekly_Earnings %in% c(141,153),.(value = mean(value))]

DOM_data[variable == "CF" & Weekly_Earnings == 45]$value <- DOM_data[variable == "CF" & Weekly_Earnings %in% c(39,51),.(value = mean(value))]
DOM_data[variable == "CF" & Weekly_Earnings == 99]$value <- DOM_data[variable == "CF" & Weekly_Earnings %in% c(93,105),.(value = mean(value))]
DOM_data[variable == "CF" & Weekly_Earnings == 147]$value <- DOM_data[variable == "CF" & Weekly_Earnings %in% c(141,153),.(value = mean(value))]

a <- ggplot(STP_data,aes(x=Weekly_Earnings,y=value/1000,colour=variable)) + geom_line() + geom_vline(xintercept = 75,linetype = "dashed") +
  scale_y_continuous_e61(limits = c(0,12,2)) +
  labs_e61(subtitle = "Tax records of JSP recipients",y="(000s)",x="Weekly Earnings",sources = c("ABS","e61")) + plab(x=80,y=5,label="Abatement threshold",colour="black")

b <- ggplot(DOM_data,aes(x=Weekly_Earnings,y=value/1000,colour=variable)) + geom_line() + geom_vline(xintercept = 75,linetype = "dashed") +
  scale_y_continuous_e61(limits = c(0,60,10)) +
  labs_e61(subtitle = "Reported earnings of JSP recipients",y="(000s)",x="Weekly Earnings",sources = c("ABS","e61"))

save_e61(a,b,filename = "Bunching.pdf")
