# Load required libraries
library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(tidyverse)
library(data.table)
library(readxl)

# Clear environment
rm(list=ls())
gc()

# Read in tax data
tax_data <- read_excel("ts22individual16percentiledistributionontaxableincomebysexstate.xlsx", sheet = "Table 16A", skip = 2) #21/22
#tax_data <- read_excel("taxstats2013individual14percentiledistributionoftaxableindividuals.xlsx", sheet = "Individuals Table 14", skip = 4) # 12/13 - this is a different structure so doesn't work
setDT(tax_data)

# Summarize data by percentile
tax_dt <- tax_data[, .(#Individuals = sum(`Individuals`),
                       tax_income = sum(`Taxable income or loss`),
                       net_tax = sum(`Net tax`),
                       group = unique(`Ranged Taxable Income`)), 
                   by = .(Percentile)]

# Compute tax rate
tax_dt[, tax_rate := net_tax / tax_income]

# Plot tax rate by percentile
ggplot(tax_dt, aes(x = Percentile, y = tax_rate)) + 
  geom_line() + 
  labs(title = "Tax Rate by Percentile")

# Compute tax share and income share
tax_dt[, tax_share := net_tax / sum(net_tax)]
tax_dt[, tax_income_share := tax_income / sum(tax_income)]

max(tax_dt$tax_rate)
max(tax_dt$tax_share)
max(tax_dt$tax_income_share)



# Plot tax and income shares
ggplot(tax_dt, aes(x = Percentile, y = tax_share)) + 
  geom_col() + 
  labs_e61(title = "Tax Share by Percentile")

ggplot(tax_dt, aes(x = Percentile, y = tax_income_share)) + 
  geom_col() + 
  labs_e61(title = "Taxable Income Share by Percentile")

# Compute total tax and total income
total_tax <- sum(tax_dt$net_tax)
total_income <- sum(tax_dt$tax_income)

# Filter for the top 25% (percentile >= 75)
top_25_dt <- tax_dt[Percentile >= 75]

# Compute share of tax and income for the top 25%
top_25_tax_share <- sum(top_25_dt$net_tax) / total_tax
top_25_income_share <- sum(top_25_dt$tax_income) / total_income

# Compute average tax rate for the top 25%
top_25_tax_rate <- sum(top_25_dt$net_tax) / sum(top_25_dt$tax_income)

# Print results
cat("Top 25% share of tax:", round(top_25_tax_share * 100, 2), "%\n")
cat("Top 25% share of taxable income:", round(top_25_income_share * 100, 2), "%\n")
cat("Top 25% average tax rate:", round(top_25_tax_rate * 100, 2), "%\n")

# Visualization comparing the top 25% to the rest
tax_summary <- data.table(
  Group = c("Bottom 75%", "Top 25%"),
  Tax_Share = c(1 - top_25_tax_share, top_25_tax_share),
  Income_Share = c(1 - top_25_income_share, top_25_income_share)
)

ggplot(tax_summary, aes(x = Group, y = Tax_Share, fill = Group)) + 
  geom_col() + 
  labs_e61(title = "Share of Total Tax: Top 25% vs. Bottom 75%")

ggplot(tax_summary, aes(x = Group, y = Income_Share, fill = Group)) + 
  geom_col() + 
  labs_e61(title = "Share of Total Taxable Income: Top 25% vs. Bottom 75%")


##
