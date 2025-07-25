#install.packages("remotes")
#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

## Create ETR schedules for stable vs variable income

# Load required libraries
library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(tidyverse)
library(data.table)
library(Hmisc)

###
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
  fifelse(income != 0, tax_function(income, include_levy = levy) / (income + non_income),0)
}

# Create income sequence
income_target <- 100000
income_target_tag <- fifelse(income_target == 100000, "100k",
                             fifelse(income_target == 1000000,"1m",NA))
incomes_stable <- rep(income_target,times=60)
incomes_unstable <- rep(c(0,0,0,income_target*4),times=15)
time <- seq(1,60,by=1)

long_data <- melt(data.table(time,ETR_stable = etr_function(incomes_stable,0),ETR_unstable =etr_function(incomes_unstable,0),ETR_unstable_discount =etr_function(incomes_unstable/2,incomes_unstable/2)),id.vars = "time")

ggplot(long_data,aes(x=time,y=value*100,colour=variable)) + geom_line() +
  labs_e61(title = "Effective tax rates",y="%",x="$(000's)") +
  plab(c("Stable","Variable","Discounted Variable"),x=c(1,1,1),y=c(20,24,28))

long_tax_paid <- melt(data.table(time,`Stable` = tax_function(incomes_stable),`Volatile` =tax_function(incomes_unstable),`Volatile with discount` =tax_function(incomes_unstable/2)),id.vars = "time")

ggplot(long_tax_paid,aes(x=time,y=value,colour=variable)) + geom_line() +
  labs_e61(title = "Effective tax rates",y="%",x="$(000's)") +
  plab(c("Stable","Variable","Discounted Variable"),x=c(1,1,1),y=c(20,24,28))

ggplot(long_tax_paid[,.(value = sum(value)/1000000),by=.(variable)],aes(x=variable,y=value))+
  geom_col() +
  labs_e61(title = paste0("Hypothetical Tax Paid ($",as.character(income_target_tag)," earner)"),
       y= "$m",
       x="",
       sources = c("e61"),
       footnotes = c("These are actual tax paid sums, not PV values. The earnings occur over 60 years, with the stable earner receiving this amount each year. The volatile earner receives four times the annual amount every four years, and nothing in other years."))

save_e61(paste0("Volatile_tax_",income_target_tag,".pdf"))

## Example where there is 2.5% inflation and 2.5% real return over 10 years.

initial_income <- 1000
end_income <- initial_income*(1.05)^10
price_level <- 1*(1.025)^10

real_income <- end_income/price_level

real_income

(end_income-initial_income)*0.8

amount_tax_end <- (end_income-initial_income)*0.2

(1 + ((real_income - initial_income - amount_tax_end)/1000))^(1/10)

((1 + ((real_income - initial_income - amount_tax_end)/1000))^(1/10)-1)/0.025

(1000*(1.05)-1000)*0.1/(1000*(1.05)-1000)

