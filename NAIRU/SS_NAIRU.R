# Purpose: Generate a state space model of inflation based on Cusbert 2017
# Last edit date: 26/06/2014
# Author: Matt Nolan -----

rm(list=ls())

library(readabs)
library(tidyverse)
library(data.table)
library(KFAS)
library(mFilter)
library(zoo)
library(Hmisc)
library(seasonal)

### Pull in three measured variables (CPI, ULC, Unemployment). ----

# CPI
cpi <- read_abs(cat_no = "6401.0")
setDT(cpi)

cpi_growth <- cpi[table_title == "TABLES 1 and 2. CPI: All Groups, Index Numbers and Percentage Changes" & unit == "Percent" & series == "Percentage Change from Previous Period ;  All groups CPI ;  Australia ;"][,.(date,cpi_value = value)]
setDT(cpi_growth)

# ULC
Nat_acc <- read_abs(cat_no = "5206.0") %>% filter(table_title == "Table 42. Unit Labour Costs") %>% filter(series_type == "Seasonally Adjusted") %>% filter(series == "Unit labour cost - Nominal ;")
setDT(Nat_acc)

#ggplot(Nat_acc,aes(x=date,y=value)) + geom_line()

Nat_acc <- Nat_acc[,.(date,value,lULC = log(value))]
Nat_acc[, lULC_growth := lULC - shift(lULC, 1, type = "lag")]

# UR
LS <- read_abs(cat_no = "6202.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original") %>% filter(series == "Employed total ;  Persons ;" | series == "Unemployed total ;  Persons ;"  | series == "Unemployment rate ;  Persons ;") %>% select("date","series","value")

LS2 <- pivot_wider(LS,id_cols = "date",names_from = "series",values_from = "value")
colnames(LS2) <- c("date","Employed","Unemployed","UR_ABS")

LS2 <- LS2 %>% mutate(UR_Derived = Unemployed/(Employed + Unemployed),QUnemp = frollmean(Unemployed,n=3,fill=NA,align="right"),QEmp =frollmean(Employed,n=3,fill=NA,align="right"),UR = UR_ABS) 

LSQ <- LS2[seq(5,nrow(LS2),3),] %>% select(date,QUnemp,QEmp,UR)
setDT(LSQ)

### Set up model ----

# Define initial NAIRU
N0 <- 0.068 

datestart <- as.Date("1989-01-01")
dateend <- as.Date("2019-12-01")

inflation <- cpi_growth[date >= datestart & date <= dateend]$cpi_value/100    
wage_growth <- Nat_acc[date >= datestart & date <= dateend]$lULC_growth
unemployment <- LSQ[date >= datestart & date <= dateend]$UR/100

# Combine data into a matrix for the state-space model
y <- cbind(inflation, wage_growth)

# Define the state-space model
model <- SSModel(
  y ~ -1 + 
    SSMcustom(
      Z = matrix(c(1, 0, 1, 1), 2, 2),
      T = matrix(c(1, 0, 0, 1), 2, 2),
      R = diag(2),
      Q = diag(c(NA, 0)),
      P1inf = diag(2),
      P1 = matrix(0, 2, 2),
      a1 = c(N0, 0)  # Set initial state mean
    ) + 
    SSMregression(~ unemployment, Q = matrix(0, 1, 1)),
  H = matrix(NA, 2, 2)
)

# Initial values for the variances
initial_values <- c(0.01, 0.01, 0.01, 0.01)

# Fit the state-space model with proper initial values and constraints
fit <- fitSSM(model, inits = initial_values, method = "BFGS")

# Extract the fitted model
fitted_model <- fit$model

# Apply the Kalman filter
kf_results <- KFS(fitted_model, smoothing = "state")

# Extract the estimated NAIRU
NAIRU_estimates <- kf_results$alphahat[, 1]

# Plot the NAIRU estimates
plot(NAIRU_estimates, type = "l", col = "blue", lwd = 2, 
     xlab = "Time", ylab = "NAIRU", main = "Estimated NAIRU over Time")