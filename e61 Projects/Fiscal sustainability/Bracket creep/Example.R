# Load required libraries
library(dplyr)
library(ggplot2)
#library(theme61)
library(readr)
library(tidyverse)
library(data.table)
library(Hmisc)

# Define the tax function
tax_function <- function(income) {
  tax <- case_when(
    income <= 18200 ~ 0,
    income <= 45000 ~ (income - 18200) * 0.16,
    income <= 135000 ~ (45000-18200)*0.16 + (income - 45000) * 0.30,
    income <= 190000 ~ (45000-18200)*0.16 + (135000 - 45000) * 0.30 + (income - 135000) * 0.37,
    TRUE ~ (45000-18200)*0.16 + (135000 - 45000) * 0.30 + (190000 - 135000) * 0.37 + (income - 190000) * 0.45
  )
  return(tax)
}

# Define effective tax rate function
etr_function <- function(income) {
  tax_function(income) / income
}

# Create income sequence
incomes <- seq(1000, 200000, by = 100)

# Define deflator (1 + 0.025)^10
deflator <- (1 + 0.025)^10

# Create main data frame
df <- tibble(
  income = incomes,
  etr_nominal = etr_function(income),
  etr_deflated = etr_function(income * deflator)
)

# Create reference points to annotate
annotate_points <- tibble(
  income = c(100000, 100000, 110000),
  etr = c(
    etr_function(100000),
    etr_function(100000 * deflator),
    etr_function(110000 * deflator)
  ),
  type = c("Nominal", "Deflated", "Deflated")
)

# Generate the plot
ggplot(df, aes(x = income)) +
  geom_line(aes(y = etr_nominal, color = "Nominal")) +
  geom_line(aes(y = etr_deflated, color = "Deflated")) +
  geom_vline(xintercept = c(100000, 110000), linetype = "dashed") +
  geom_point(data = annotate_points, aes(x = income, y = etr, shape = type), size = 2.5) +
  geom_text(data = annotate_points, aes(x = income, y = etr,
                                        label = paste0(round(etr * 100, 1), "%")),
            vjust = -1.2, size = 3.5) +
  scale_color_manual(values = c("Nominal" = "blue", "Deflated" = "red")) +
  scale_shape_manual(values = c("Nominal" = 16, "Deflated" = 17)) +
  labs(title = "Effective Tax Rates by Gross Income",
       subtitle = "Dashed lines at Avg Earnings in FY25 and FY35 (RHS); Deflated line assumes 2.5% inflation for 10 years",
       x = "Gross Income ($)", y = "Effective Tax Rate",
       color = "Tax Scale", shape = "Tax Scale") +
  theme_classic()
