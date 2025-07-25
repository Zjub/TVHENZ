#install.packages("remotes")
#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

## Create ETR schedules for labour and capital income

# Load required libraries
library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(tidyverse)
library(data.table)
library(Hmisc)

# Options

levy = TRUE # Include the medicare levy
surcharge = FALSE # Include the medicare surcharge

## Define the tax function
# Income refers to taxable income

tax_function <- function(income, include_levy = FALSE) {

  # --- Medicare Levy phase-in and full ---
  medicare_levy <- if (include_levy) {
    ifelse(
      income <= 45000,
      # Phase-in below ~45k
      pmax(0, pmin((income - 27222) * 0.1, income * 0.02)),
      # Full 2% above ~45k
      income * 0.02
    )
  } else {
    0
  }
  # --- Base tax brackets ---
  base_tax <- case_when(
    income <= 18200 ~ 0,
    income <= 45000 ~ (income - 18200) * 0.16,
    income <= 135000 ~ (45000 - 18200) * 0.16 +
      (income - 45000) * 0.30,
    income <= 190000 ~ (45000 - 18200) * 0.16 +
      (135000 - 45000) * 0.30 +
      (income - 135000) * 0.37,
    TRUE             ~ (45000 - 18200) * 0.16 +
      (135000 - 45000) * 0.30 +
      (190000 - 135000) * 0.37 +
      (income - 190000) * 0.45
  )

  # --- Total tax ---
  total_tax <- base_tax + medicare_levy

  return(total_tax)
}

## Define effective tax rate function - non_income refers to income earned that is not taxed (i.e. CGT discount)
etr_function <- function(income,non_income) {
  tax_function(income, include_levy = levy) / (income + non_income)
}

# Create income sequence
incomes <- seq(1000, 200000, by = 1000)

long_data <- melt(data.table(incomes,ETR_labour = etr_function(incomes,0),ETR_CG =etr_function(incomes/2,incomes/2)),id.vars = "incomes")

ggplot(long_data,aes(x=incomes/1000,y=value*100,colour=variable)) + geom_line() +
  labs_e61(title = "Effective tax rates",y="%",x="$(000's)") +
  plab(c("Labour earnings","Long-term Capital Gains"),x=c(1,1),y=c(24,28))

save_e61("ETR_scenario.png",res=2)
save_e61("ETR_scenario.pdf")
