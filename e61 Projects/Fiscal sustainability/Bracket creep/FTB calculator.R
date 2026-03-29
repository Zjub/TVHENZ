## ETR calculator with option to add FBT
## LITO is included in this version

#install.packages("remotes")
#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

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
FBT_inc = TRUE
FBT_income = 100000

# Define the tax function

tax_function <- function(income, include_levy = levy, include_surcharge = surcharge,second_rate = 0.16) {

  # --- LITO (Low Income Tax Offset) ---
  # Applies to income tax payable (base tax), not Medicare levy / MLS.
  lito <- dplyr::case_when(
    income <= 37500 ~ 700,
    income <= 45000 ~ 700 - 0.05 * (income - 37500),
    income <= 66667 ~ 325 - 0.015 * (income - 45000),
    TRUE            ~ 0
  )

  # Guardrails (numerical safety)
  lito <- pmax(0, lito)

  # --- Medicare Levy phase-in and full ---
  medicare_levy <- if (include_levy) {
    ifelse(
      income <= 45000,
      pmax(0, pmin((income - 27222) * 0.1, income * 0.02)),
      income * 0.02
    )
  } else {
    0
  }

  # --- Medicare Levy Surcharge (MLS) --- Note: Surcharge income base is wrong, so do not use for now
  mls_rate <- if (include_surcharge) dplyr::case_when(
    income <= 101000 ~ 0,
    income <= 118000 ~ 0.01,
    income <= 158000 ~ 0.0125,
    TRUE             ~ 0.015
  ) else 0

  mls_surcharge <- income * mls_rate

  # --- Base tax brackets ---
  base_tax <- dplyr::case_when(
    income <= 18200 ~ 0,
    income <= 45000 ~ (income - 18200) * second_rate,
    income <= 135000 ~ (45000 - 18200) * second_rate +
      (income - 45000) * 0.30,
    income <= 190000 ~ (45000 - 18200) * second_rate +
      (135000 - 45000) * 0.30 +
      (income - 135000) * 0.37,
    TRUE             ~ (45000 - 18200) * second_rate +
      (135000 - 45000) * 0.30 +
      (190000 - 135000) * 0.37 +
      (income - 190000) * 0.45
  )

  # --- Apply LITO to base tax only (non-refundable) ---
  income_tax_after_offsets <- pmax(0, base_tax - lito)

  # --- Total tax ---
  total_tax <- income_tax_after_offsets + medicare_levy + mls_surcharge
  return(total_tax)
}

fbt_function <- function(FBT_income,second_rate=0.16) {
  #fbt_tax <- tax_function(income + FBT_income, include_levy = levy, include_surcharge = surcharge,second_rate=second_rate) - tax_function(total_income, include_levy = levy, include_surcharge = surcharge,second_rate=second_rate) # Use if want to look at putting FBT into base.
  fbt_tax <- FBT_income*0.47
  return(fbt_tax)
}


# Define effective tax rate function
etr_function <- function(total_income,second_rate=0.16) {
  tax_function(total_income, include_levy = levy, include_surcharge = surcharge,second_rate=second_rate) / total_income
}

# Create income sequence
non_FBT_incomes <- seq(1000, 200000, by = 500)

# Build comparison dataset
plot_dt <- bind_rows(
  tibble(
    scenario = "No FBT payment",
    non_FBT_income = non_FBT_incomes,
    FBT_income = 0,
    total_income = non_FBT_income + FBT_income,
    tax_paid = tax_function(non_FBT_income)
  ),
  tibble(
    scenario = "FBT payment received",
    non_FBT_income = non_FBT_incomes,
    FBT_income = FBT_income,
    total_income = non_FBT_income + FBT_income,
    tax_paid = tax_function(non_FBT_income) + fbt_function(FBT_income)
  )
)

setDT(plot_dt)

# Plot
ggplot(plot_dt[total_income <= max(non_FBT_incomes)], aes(x = total_income, y = tax_paid, linetype = scenario)) +
  geom_line(linewidth = 1) +
  labs(title = "Comparison of tax paid",
    x = "Total income (non-FBT income + FBT income)",
    y = "Tax paid",
    linetype = NULL
  ) +
  scale_x_continuous(labels = scales::dollar) + theme_e61(legend = "bottom")


