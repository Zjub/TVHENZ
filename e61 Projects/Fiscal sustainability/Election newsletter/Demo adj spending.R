## Last update:  28/04/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Basic demographic trends used to project spending under status quo

library(cli)
library(tidyverse)
library(data.table)
library(theme61)
library(tidyr)
library(readxl)
library(gghighlight)
library(readabs)
library(OECD)
library(jsonlite)
library(httr)
library(Synth)
library(mFilter)

rm(list=ls())
gc()

## Import PBO Budget data

Expenses <- read_excel("PBO Historical fiscal data - 2025-26 Budget update.xlsx",
                                                               sheet = "Table 7", skip = 2)

setDT(Expenses)

Expenses <- Expenses[!is.na(Units)]

colnames(Expenses)[1] <- "Series"
colnames(Expenses)[4] <- "Empty"

Expense_data <- Expenses %>%
  filter(Series %in% c("Total education","Total health","Total social security and welfare","Total expenses"))

cols_numeric <- names(Expense_data)[5:ncol(Expense_data)]

Expense_data[, (cols_numeric) := lapply(.SD, as.numeric), .SDcols = cols_numeric]

value_cols <- names(Expense_data)[5:ncol(Expense_data)]

expenses_ex_row <- Expense_data[Series == "Total expenses", ..value_cols] -
  Expense_data[Series == "Total education", ..value_cols] -
  Expense_data[Series == "Total health", ..value_cols] -
  Expense_data[Series == "Total social security and welfare", ..value_cols]

manual_cols <- data.table(
  Series = "Other expenses",
  Units = "$m",
  Note = NA,
  Empty = NA
)

expenses_ex_row_full <- cbind(manual_cols, expenses_ex_row)
Expense_data <- rbind(Expense_data, expenses_ex_row_full, fill = TRUE)

Expense_data_long <- Expense_data %>%
  pivot_longer(
    cols = `1964-65`:`2028-29`,   # all your year columns
    names_to = "year",
    values_to = "value"
  )

setDT(Expense_data_long)


Expense_data_dt <- Expense_data_long[Series != "Total expenses"]

## Now get the population information and the age shares.

# pop_proj <- read_abs(cat_no = "3222.0") # Table issues break the import, will pull in manually

pop <- read_abs(cat_no = "3101.0")

setDT(pop)

unique(pop$series)

pop <- pop[grepl("Estimated Resident Population ;  Persons ;", series) & grepl("Australia", table_title)  & frequency == "Annual"]

total_pop <- pop[,.(total = sum(value)),by=.(date)]

pop[, age := fifelse(
  grepl("100 and over", series),
  100,
  as.integer(sub(".*;\\s*(\\d+)\\s*;.*", "\\1", series))
)]

pop[, age_group := fcase(
  age >= 0 & age <= 14, "0-14",
  age >= 15 & age <= 24, "15-24",
  age >= 25 & age <= 39, "25-39",
  age >= 40 & age <= 54, "40-54",
  age >= 55 & age <= 64, "55-64",
  age >= 65 & age <= 74, "65-74",
  age >= 75, "75+"
)]

age_pop <- pop[,.(number = sum(value)),by=.(date,age_group)][order(date,age_group)]


pop_shares <- total_pop[age_pop,on=.(date)][, prop := number/total]

## Combine for projections - start with levels

pop_shares[, year := format(date, "%Y")]
pop_wide <- dcast(pop_shares, year ~ age_group, value.var = "number")
expense_wide <- dcast(Expense_data_dt, year ~ Series, value.var = "value")
expense_wide[, year := substr(year, 1, 4)]

final_data <- merge(expense_wide, pop_wide, by = "year", all.x = TRUE)
final_data <- final_data[!is.na(`55-64`)]


health_model <- lm(`Total health` ~ `0-14` + `15-24` + `25-39` + `40-54` + `55-64` + `65-74` + `75+`, data = final_data)
summary(health_model)


# Step 5: Predict
final_data[, health_predicted := predict(health_model, newdata = final_data)]


