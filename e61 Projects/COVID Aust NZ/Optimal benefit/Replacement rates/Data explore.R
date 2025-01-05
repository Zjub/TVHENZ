### Exploring SIH RR data (addition to Matt M's work).
## Data not stored centrally for security - will need a synthetic version for final paper.
# Author: Matt Nolan
# Date made: 31/12/2024
# Last update: 31/12/2024


library(tidyverse)
library(data.table)
library(theme61)
library(readxl)

# Replace with dataset
RR_dt <- read_csv("C:/Users/OEM/Downloads/RRs_csv.csv")
setDT(RR_dt)

RR_dt[,":=" (net_inc_ratio = hours0_net_income/current_net_income,grs_inc_ratio = hours0_gross_income/current_gross_income)]

# Here, if there is not capital income just do the straight RR. If gross_income is zero then there is no "taxable benefits" so also set to zero.
RR_dt[,":=" (grs_RR = (hours0_gross_income-NonWageIncome)/(current_gross_income-NonWageIncome),
             net_RR = fifelse(NonWageIncome == 0, hours0_net_income/current_net_income,
                              fifelse(hours0_gross_income == 0,0,
                         ((hours0_net_income-(NonWageIncome - NonWageIncome/hours0_gross_income*hours0_income_tax))/(current_net_income-(NonWageIncome - NonWageIncome/current_gross_income*current_income_tax))))))]

RR_dt_net <- RR_dt[,.(net_inc_ratio,net_RR)]
RR_dt_grs <- RR_dt[,.(grs_inc_ratio,grs_RR)]

ggplot(melt(RR_dt_net),aes(x=value,colour=variable)) + geom_density() + theme_e61(legend = "bottom")
ggplot(melt(RR_dt_grs),aes(x=value,colour=variable)) + geom_density() + theme_e61(legend = "bottom")

ggplot(melt(RR_dt_net), aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot() + 
  coord_flip() + 
  theme_e61(legend = "bottom")
