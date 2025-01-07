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

SAM_dt <- cbind(RR = c(50,55,60.5,70,77),SAM_dt)



