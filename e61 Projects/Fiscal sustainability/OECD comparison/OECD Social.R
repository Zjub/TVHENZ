####### Social Protection breakdown.
# Add time series, the State vs Federal breakdown - we will want subcategories but this will be done in a separate file with ABS data
# Author: Matt Nolan
# Created: 15/9/2025
# Last edit: 15/9/2025
# Last editor: Matt Nolan

rm(list=ls())

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
library(readabs)


Consol_toG <- read_excel("table22-consolidated-cofog-expenditure-spent-by-approach.xlsx", 
                         sheet = "COFOGexp_%oftotal", skip = 1)

Consol_toGDP <- read_excel("table22-consolidated-cofog-expenditure-spent-by-approach.xlsx", 
                           sheet = "COFOGexp_%GDP", skip = 1)

setDT(Consol_toG)
setDT(Consol_toGDP)

Aus_toGDP <- Consol_toGDP[Country == "Australia"][,':=' (Country = NULL,ISO = NULL, `COFOG Code` = NULL, `Government level code` = NULL,`1995` = NULL,`1996` = NULL,`1997` = NULL)]

Aus_toGDP <- melt(Aus_toGDP, id.vars = c(colnames(Aus_toGDP)[1],colnames(Aus_toGDP)[2]),variable.name = "Year",value.name = "value")

colnames(Aus_toGDP)[1] <- "COFOG_Area"
colnames(Aus_toGDP)[2] <- "Government_level"

Aus_toGDP[, Government_level := fifelse(Government_level %in% c("Local", "State"), 
                                        "Non-Federal", "Federal")]

Aus_toGDP <- Aus_toGDP[, .(value = sum(value,na.rm=TRUE)), 
                       by = .(COFOG_Area, Government_level, Year)]


ggplot(Aus_toGDP[COFOG_Area == "Social Protection"],aes(x=as.numeric(Year)+1997, y=value*100,fill = as.factor(Government_level))) + 
  geom_col() +
  theme_e61(legend = "bottom") + labs_e61(title = "Social Protection",y="% GDP") +
  scale_x_continuous_e61(limits=c(1997,2023,3),expand_left = 0.02,expand_right = 0.02,hide_first_last = FALSE) +
  plab(c("Federal","Non-Federal"),y=c(8.5,6),x=c(1998,1998))