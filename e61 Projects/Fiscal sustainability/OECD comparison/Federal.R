# Topic: Making some high level Federal plots to motivate investigation
# Author: Matt Nolan
# Created: 8/7/2025
# Last edit: 8/7/2025
# Last editor: Matt Nolan

#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(readxl)
library(tidyverse)
library(data.table)
library(Hmisc)
library(tidysynth)

## Import data ----

Fed_spend <- Consolidated_Fiscal_Position <- read_excel("Consolidated Fiscal Position.xlsx", 
                                                        sheet = "long_exp")

setDT(Fed_spend)

Fed_spend_dt <- Fed_spend[, c(
  .SD[, lapply(.SD, function(x) x / .SD[[ncol(.SD)]]), .SDcols = 2:(ncol(Fed_spend))],
  .(Year = .SD[[1]])
)]

#Fed_spend_dt[,":=" (`Total expenses` = NULL,`Implied expenses` = NULL)]
Fed_spend_dt <- Fed_spend_dt[,.(`General public services`,`Defence`,`Public order and safety`,`Education`,`Health`,`Social security and welfare`,`Housing and community amenities`,`Recreation and culture`,`Econ Purposes`,Year
)]

Fed_spend_dt_long <- melt(Fed_spend_dt,id.vars = "Year",variable.name = "Category",value.name = "value")

ggplot(Fed_spend_dt_long[Year >= 2008],aes(x=Year,y=value*100,colour=Category)) + geom_line() +
  theme_e61(legend = "bottom") + geom_vline(xintercept = 2024) +
  labs_e61(title = "Share of Expenditure",
           y="%")

## Add Federal Revenue plots

cash_rev <- read_excel("PBO Historical fiscal data - 2025-26 Budget update.xlsx",sheet = "Table 6")
setDT(cash_rev)
NGDP <- read_excel("PBO Historical fiscal data - 2025-26 Budget update.xlsx",sheet = "Table 1")
setDT(NGDP)





