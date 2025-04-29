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

Expense_data_long[Series == "Total expenses"] # The adjustment of accounting year (1999) is broken in this - best to focus on only accrual years in any case

Expense_data_dt <- Expense_data_long[Series != "Total expenses"]
Expense_data_dt[, end_year := as.numeric(substr(year, 1, 4))]

cpi <- read_abs(cat_no = "6401.0")
setDT(cpi)

cpi2 <- cpi[table_title == "TABLES 1 and 2. CPI: All Groups, Index Numbers and Percentage Changes" & unit == "Index Numbers" & series == "Index Numbers ;  All groups CPI ;  Australia ;"][,.(date,cpi_value = value)]

cpi2[, end_year := as.numeric(year(date))]
cpi2[, month := month(date)]

cpi2[month >= 7, end_year := end_year + 1]

cpi_annual <- cpi2[, .(cpi_yearly = mean(cpi_value, na.rm = TRUE)), by = end_year]

Expense_data_dt <- cpi_annual[Expense_data_dt,on=.(end_year)][,value := value*100/cpi_yearly]



###### Now get the population information and the age shares.

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

pop_shares[year %in% c(2024,2019,2014) & age_group == "75+"]

## Combine for projections - start with levels
#
# pop_shares[, year := format(date, "%Y")]
#
# pop_wide <- dcast(pop_shares, year + total ~ age_group, value.var = "prop")
#
# expense_wide <- dcast(Expense_data_dt, year ~ Series, value.var = "value")
# expense_wide[, year := substr(year, 1, 4)]
#
# final_data <- merge(expense_wide, pop_wide, by = "year", all.x = TRUE)
# final_data[, year := as.numeric(year)]
# final_data <- final_data[!is.na(total)]
#
# final_data[, education_pc := `Total education` / total]
# final_data[, health_pc := `Total health` / total]
# final_data[, social_pc := `Total social security and welfare` / total]
# final_data[, other_pc := `Other expenses` / total]
#
# # Set up an hpfilter to get the trend values
# final_data[, education_hp := hpfilter(education_pc, freq = 100)$trend]
# final_data[, health_hp := hpfilter(health_pc, freq = 100)$trend]
# final_data[, social_hp := hpfilter(social_pc, freq = 100)$trend]
# final_data[, other_hp := hpfilter(other_pc, freq = 100)$trend]
#
# education_model <- lm(education_hp ~ `0-14` + `15-24` + `25-39` + `40-54` + `55-64` + `65-74` + `75+`, data = final_data)
# health_model    <- lm(health_hp ~ `0-14` + `15-24` + `25-39` + `40-54` + `55-64` + `65-74` + `75+`, data = final_data)
# social_model    <- lm(social_hp ~ `0-14` + `15-24` + `25-39` + `40-54` + `55-64` + `65-74` + `75+`, data = final_data)
# other_model     <- lm(other_hp ~ `0-14` + `15-24` + `25-39` + `40-54` + `55-64` + `65-74` + `75+`, data = final_data)
#
# # Predict per capita spending
# final_data[, education_pred_pc := predict(education_model, newdata = final_data)]
# final_data[, health_pred_pc    := predict(health_model, newdata = final_data)]
# final_data[, social_pred_pc    := predict(social_model, newdata = final_data)]
# final_data[, other_pred_pc     := predict(other_model, newdata = final_data)]
#
# final_data[, education_pred_total := education_pred_pc * total]
# final_data[, health_pred_total    := health_pred_pc * total]
# final_data[, social_pred_total    := social_pred_pc * total]
# final_data[, other_pred_total     := other_pred_pc * total]
#
# # Aggregate total predicted spending
# final_data[, total_spending_predicted := education_pred_total + health_pred_total + social_pred_total + other_pred_total]
#
# final_output <- final_data[, .(year, total_spending_predicted,
#                                education_pred_total, health_pred_total,
#                                social_pred_total, other_pred_total)]
#
# final_output
#
# final_data[, actual_total_spending := `Total education` + `Total health` + `Total social security and welfare` + `Other expenses`]
#
# plot_data <- final_data[, .(year,
#                             actual_total_spending = actual_total_spending / 1e9,
#                             predicted_total_spending = total_spending_predicted / 1e9)]
#
# plot_data_long <- melt(plot_data, id.vars = "year",
#                        variable.name = "type",
#                        value.name = "spending_billion")
#
# # Step 4: Plot
# ggplot(plot_data_long, aes(x = year, y = spending_billion, color = type)) +
#   geom_line(size = 1) +
#   labs(title = "Actual vs Predicted Total Government Spending",
#        x = "Year",
#        y = "Spending ($ Billion)",
#        color = "Legend") +
#   theme_minimal()

## Logs

pop_shares[, year := format(date, "%Y")]

# Reshape pop_shares wide: one row per year, each age_group as a column
pop_wide <- dcast(pop_shares, year + total ~ age_group, value.var = "prop")

# Step 2: Prepare Expense_data_dt
setDT(Expense_data_dt)

# Keep only relevant spending series
Expense_filtered <- Expense_data_dt[
  Series %in% c("Total education", "Total health", "Total social security and welfare", "Other expenses")
]

# Reshape expenses wide: one row per year, columns for each series
expense_wide <- dcast(Expense_filtered, year ~ Series, value.var = "value")

# Clean year variable: keep only first four digits
expense_wide[, year := substr(year, 1, 4)]

# Step 3: Merge population shares and expenses
final_data <- merge(expense_wide, pop_wide, by = "year", all.x = TRUE)
final_data <- final_data[!is.na(total)]
final_data[, year := as.numeric(year)]


keep_cash <- TRUE

if(keep_cash == FALSE){
  final_data <- final_data[year > 1999] # Dropping the cash accounting years
} else {
  final_data <- final_data[year != 1999] # Dropping the transition year
  final_data[, cash_dum := fifelse(year < 2000,1,0)]
}

# Step 4: Calculate spending per capita
final_data[, education_pc := `Total education` / total]
final_data[, health_pc    := `Total health` / total]
final_data[, social_pc    := `Total social security and welfare` / total]
final_data[, other_pc     := `Other expenses` / total]

# Step 5: Take log of per capita spending

training_year <- 2014

final_data[, education_log := log(education_pc*1000000000)]
final_data[, health_log    := log(health_pc*1000000000)]
final_data[, social_log    := log(social_pc*1000000000)]
final_data[, other_log     := log(other_pc*1000000000)]
final_data[, train := year <= training_year]

# Step 6: Apply HP filter to the log series (trend in logs = trend in growth rates)
final_data[, education_log_hp := hpfilter(education_log, freq = 100)$trend]
final_data[, health_log_hp    := hpfilter(health_log, freq = 100)$trend]
final_data[, social_log_hp    := hpfilter(social_log, freq = 100)$trend]
final_data[, other_log_hp     := hpfilter(other_log, freq = 100)$trend]

# Step 7: Estimate regressions ONLY on training data (up to 2019)
education_model <- lm(education_log_hp ~ `0-14` + `15-24` + `25-39` + `40-54` + `55-64` + `65-74` + `75+` + cash_dum + year,
                      data = final_data[train == TRUE])

health_model    <- lm(health_log_hp ~ `0-14` + `15-24` + `25-39` + `40-54` + `55-64` + `65-74` + `75+` + cash_dum + year,
                      data = final_data[train == TRUE])

social_model    <- lm(social_log_hp ~ `0-14` + `15-24` + `25-39` + `40-54` + `55-64` + `65-74` + `75+` + cash_dum + year,
                      data = final_data[train == TRUE])

other_model     <- lm(other_log_hp ~ `0-14` + `15-24` + `25-39` + `40-54` + `55-64` + `65-74` + `75+` + cash_dum + year,
                      data = final_data[train == TRUE])

# Step 8: Predict log(per capita spending) for ALL years (in-sample and out-of-sample)
final_data[, education_log_pred := predict(education_model, newdata = final_data)]
final_data[, health_log_pred    := predict(health_model, newdata = final_data)]
final_data[, social_log_pred    := predict(social_model, newdata = final_data)]
final_data[, other_log_pred     := predict(other_model, newdata = final_data)]

# Step 9: Convert predicted logs back to levels
final_data[, education_pred_total := exp(education_log_pred) * total]
final_data[, health_pred_total    := exp(health_log_pred) * total]
final_data[, social_pred_total    := exp(social_log_pred) * total]
final_data[, other_pred_total     := exp(other_log_pred) * total]

# Step 10: Aggregate predicted total spending
final_data[, total_spending_predicted := education_pred_total + health_pred_total + social_pred_total + other_pred_total]

# Step 11: Calculate actual total spending
final_data[, actual_total_spending := (`Total education` + `Total health` + `Total social security and welfare` + `Other expenses`)*1000000000]

# Total HP trend (add up HP-filtered components)
final_data[, total_spending_hp := exp(education_log_hp) * total +
             exp(health_log_hp) * total +
             exp(social_log_hp) * total +
             exp(other_log_hp) * total]

# Now prepare plot data
plot_nominal <- final_data[, .(year,
                               actual_total_spending = actual_total_spending,
                               predicted_total_spending = total_spending_predicted,
                               hp_trend_total_spending = total_spending_hp)]

# Melt to long format for ggplot
plot_nominal_long <- melt(plot_nominal, id.vars = "year",
                          variable.name = "type",
                          value.name = "spending_dollars")

# Step 13: Prepare data for plotting (Log total spending per capita)
final_data[, actual_total_log_pc := log((actual_total_spending / total))]
final_data[, predicted_total_log_pc := log((total_spending_predicted / total))]
final_data[, hp_trend_log_pc := log(total_spending_hp / total)]

plot_log <- final_data[, .(year,
                           actual_total_log_pc,
                           predicted_total_log_pc,
                           hp_trend_log_pc)]

plot_log_long <- melt(plot_log, id.vars = "year",
                      variable.name = "type",
                      value.name = "log_spending_pc")

# Step 14: Plot Nominal levels
ggplot(plot_nominal_long, aes(x = year, y = spending_dollars / 1e9, color = type)) +
  geom_line(size = 1) +
  geom_vline(xintercept = training_year + 0.5, linetype = "dashed") +
  labs(title = "Actual vs Predicted vs HP Trend: Total Government Spending (Nominal $)",
       subtitle = paste0("Dashed line shows ", training_year, " - model estimated on data before"),
       x = "Year",
       y = "Spending ($ Billion)",
       color = "Legend") +
  theme_minimal()

# Step 15: Plot Log(per capita)
ggplot(plot_log_long, aes(x = year, y = log_spending_pc, color = type)) +
  geom_line(size = 1) +
  geom_vline(xintercept = training_year + 0.5, linetype = "dashed") +
  labs(title = "Actual vs Predicted vs HP Trend: Log Total Spending per Capita",
       subtitle = paste0("Dashed line shows ", training_year, " - model estimated on data before"),
       x = "Year",
       y = "Log Spending per Capita",
       color = "Legend") +
  theme_minimal()

save_e61("demo.png",res=2)

final_data[year %in% c(2019, 2024)]

ggplot(final_data,aes(x=year,y=`75+`)) + geom_line()
ggplot(final_data,aes(x=year,y=`40-54`)) + geom_line()

# Create a tidy table with actual and predicted spending for each subcategory

subcategories_plot <- final_data[, .(year,
                                     education_actual = `Total education` * 1e9,
                                     health_actual = `Total health` * 1e9,
                                     social_actual = `Total social security and welfare` * 1e9,
                                     other_actual = `Other expenses` * 1e9,
                                     education_predicted = education_pred_total,
                                     health_predicted = health_pred_total,
                                     social_predicted = social_pred_total,
                                     other_predicted = other_pred_total)]

# Melt to long format for easy ggplot
subcategories_plot_long <- melt(subcategories_plot,
                                id.vars = "year",
                                variable.name = "series",
                                value.name = "spending_dollars")

# Create new columns for 'category' and 'type' (actual or predicted)
subcategories_plot_long[, category := fifelse(grepl("education", series), "Education",
                                              fifelse(grepl("health", series), "Health",
                                                      fifelse(grepl("social", series), "Social Security & Welfare", "Other")))]

subcategories_plot_long[, type := fifelse(grepl("actual", series), "Actual", "Predicted")]

# Plot each subcategory (Actual vs Predicted)
ggplot(subcategories_plot_long, aes(x = year, y = spending_dollars / 1e9, color = type)) +
  geom_line(size = 1) +
  facet_wrap(~category, scales = "free_y") +
  geom_vline(xintercept = training_year + 0.5, linetype = "dashed") +
  labs(title = "Actual vs Predicted Spending by Category",
       subtitle = paste0("Dashed line shows ", training_year, " - model estimated on data before"),
       x = "Year",
       y = "Spending ($ Billion)",
       color = "Legend") +
  theme_minimal()

# Check population shares
ggplot(pop_shares, aes(x = date, y = prop, color = age_group)) +
  geom_line() +
  labs(title = "Population Shares by Age Group",
       x = "Year",
       y = "Proportion of Total Population",
       color = "Age Group") +
  theme_minimal()

### Decomposition of role of changing population shares.

# -----------------------------------------------
# Full Decomposition Script (Baseline = 2000)
# -----------------------------------------------

library(data.table)
library(ggplot2)

# Step 1: Setup

# Set baseline year
baseline_year <- 2000

# Define age groups
age_groups <- c("0-14", "15-24", "25-39", "40-54", "55-64", "65-74", "75+")

# Step 2: Clean coefficient names properly
education_coefs_raw <- coef(education_model)
health_coefs_raw    <- coef(health_model)
social_coefs_raw    <- coef(social_model)
other_coefs_raw     <- coef(other_model)

names(education_coefs_raw) <- gsub("`", "", names(education_coefs_raw))
names(health_coefs_raw)    <- gsub("`", "", names(health_coefs_raw))
names(social_coefs_raw)    <- gsub("`", "", names(social_coefs_raw))
names(other_coefs_raw)     <- gsub("`", "", names(other_coefs_raw))

# Step 3: Extract only the age group coefficients
education_coefs_age <- education_coefs_raw[age_groups]
health_coefs_age    <- health_coefs_raw[age_groups]
social_coefs_age    <- social_coefs_raw[age_groups]
other_coefs_age     <- other_coefs_raw[age_groups]

# Step 4: Calculate delta (change in pop shares from baseline)

# Calculate delta for each age group
for (ag in age_groups) {
  baseline_value <- final_data[year == baseline_year, get(ag)]
  final_data[, paste0("delta_", ag) := get(ag) - baseline_value]
}

# Step 5: Calculate contribution = coefficient × delta share

# Education
for (ag in age_groups) {
  final_data[, paste0("education_contrib_", ag) := education_coefs_age[[ag]] * get(paste0("delta_", ag))]
}

# Health
for (ag in age_groups) {
  final_data[, paste0("health_contrib_", ag) := health_coefs_age[[ag]] * get(paste0("delta_", ag))]
}

# Social
for (ag in age_groups) {
  final_data[, paste0("social_contrib_", ag) := social_coefs_age[[ag]] * get(paste0("delta_", ag))]
}

# Other
for (ag in age_groups) {
  final_data[, paste0("other_contrib_", ag) := other_coefs_age[[ag]] * get(paste0("delta_", ag))]
}

# Step 6: Aggregate total contributions

for (ag in age_groups) {
  final_data[, paste0("total_contrib_", ag) :=
               get(paste0("education_contrib_", ag)) +
               get(paste0("health_contrib_", ag)) +
               get(paste0("social_contrib_", ag)) +
               get(paste0("other_contrib_", ag))]
}

# Step 7: Stack into long format

# Education
education_long <- melt(final_data, id.vars = "year",
                       measure.vars = patterns("^education_contrib_"),
                       variable.name = "age_group",
                       value.name = "contribution")
education_long[, age_group := gsub("education_contrib_", "", age_group)]
education_long[, category := "Education"]

# Health
health_long <- melt(final_data, id.vars = "year",
                    measure.vars = patterns("^health_contrib_"),
                    variable.name = "age_group",
                    value.name = "contribution")
health_long[, age_group := gsub("health_contrib_", "", age_group)]
health_long[, category := "Health"]

# Social
social_long <- melt(final_data, id.vars = "year",
                    measure.vars = patterns("^social_contrib_"),
                    variable.name = "age_group",
                    value.name = "contribution")
social_long[, age_group := gsub("social_contrib_", "", age_group)]
social_long[, category := "Social Security & Welfare"]

# Other
other_long <- melt(final_data, id.vars = "year",
                   measure.vars = patterns("^other_contrib_"),
                   variable.name = "age_group",
                   value.name = "contribution")
other_long[, age_group := gsub("other_contrib_", "", age_group)]
other_long[, category := "Other Expenses"]

# Total
total_long <- melt(final_data, id.vars = "year",
                   measure.vars = patterns("^total_contrib_"),
                   variable.name = "age_group",
                   value.name = "contribution")
total_long[, age_group := gsub("total_contrib_", "", age_group)]
total_long[, category := "Total Spending"]

# Step 8: Combine all
all_contributions <- rbindlist(list(education_long, health_long, social_long, other_long, total_long))

# Step 9: Plot Decomposition

ggplot(all_contributions, aes(x = year, y = contribution, fill = age_group)) +
  geom_area(alpha = 0.85) +
  geom_vline(xintercept = training_year + 0.5, linetype = "dashed") +
  facet_wrap(~category, scales = "free_y", ncol = 2) +
  labs(title = "Change in Predicted Log Spending per Capita by Age Group (Baseline = 2000)",
       subtitle = paste0("Dashed line shows ", training_year, " - model estimated on data before"),
       x = "Year",
       y = "Contribution to Change in Log Spending",
       fill = "Age Group") +
  theme_minimal() +
  theme(legend.position = "bottom")


### Bits to clear

setDT(plot_log_long)

# real spending comparison plot
ggplot(plot_log_long[!type %in% "actual_total_log_pc"], aes(x = year, y = log_spending_pc, colour = type)) +
  geom_line() +
  geom_vline(xintercept = training_year + 0.5 + 5, linetype = "dashed") +
  labs(title = "Total Government Spending",
       subtitle = "Real value, 2012 prices (in logs)",
       footnotes = c(paste0("Model estimates trend government spending on subcategory as function of time and demographic structure. Model estimated on data before ",training_year),"Trend estimate based on an HP filter."),
       x = "Year",
       y = "") +
  scale_y_continuous_e61(limits = c(15,17,0.5)) +
  plab(c("Demographically adjusted trend","Trend spending"),x=c(1975,1975),y=c(15.3,16.7))

save_e61("Dem_adj_spend.png",res=2)
