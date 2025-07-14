# Topic: Grab some of the distributional information about STIK to talk about the distribution of benefits from core spending
# Author: Matt Nolan
# Created: 10/7/2025
# Last edit: 10/7/2025
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



## Import data ----
# Note there are a set of rows called "all households" - only keeping to act as a cross-check on the base data, as all should have the same values.

STIK_dt <- read_excel("Expenditure plots.xlsx", 
                                sheet = "STIK")
setDT(STIK_dt)

STIK_dt

pop_dt <- STIK_dt[Measure == "Number"]
STIK_no_number <- STIK_dt[Measure != "Number"]

value_cols <- setdiff(names(STIK_no_number), c("Year", "Measure"))
pop_cols <- paste0(value_cols, "_pop")

pop_dt[, (value_cols) := lapply(.SD, function(x) x / 1000000), .SDcols = value_cols]

STIK_per_person <- merge(STIK_no_number, pop_dt, by = "Year", suffixes = c("", "_pop"))
STIK_per_person[, (value_cols) := Map(`/`, .SD[, value_cols, with=FALSE], .SD[, pop_cols, with=FALSE])]
STIK_per_person[, (pop_cols) := NULL]

STIK_per_person

## Create graphs

### Age ----
age_dt <- STIK_no_number[,.(Year,Measure,`15-24`,`25-34`,`35-44`, `45-54`, `55-64`, `65 and over`)]

age_dt_long <- melt(age_dt,id.vars = c("Year","Measure"))

ggplot(age_dt_long[Measure == "Health"],aes(x=Year,y=value,fill=variable)) + geom_col(position = "dodge") + 
  coord_flip() + theme_e61(legend = "bottom")


age_perperson_dt <- STIK_per_person[,.(Year,Measure,`15-24`,`25-34`,`35-44`, `45-54`, `55-64`, `65 and over`)]

age_perperson_dt_long <- melt(age_perperson_dt,id.vars = c("Year","Measure"))

ggplot(age_perperson_dt_long[Measure == "Health"],aes(x=Year,y=value,fill=variable)) + geom_col(position = "dodge") + 
  coord_flip() + theme_e61(legend = "bottom")

# Analysis

health_total <- age_dt_long[Measure == "Health" & variable == "65 and over"]
health_perperson <- age_perperson_dt_long[Measure == "Health" & variable == "65 and over"]

# Approximate population: total spend / per person spend
health_total[, pop_estimate := value / health_perperson$value]

# Growth rates relative to first year
health_total[, `:=`(
  total_growth = (value / first(value)) - 1,
  per_person_growth = (health_perperson$value / first(health_perperson$value)) - 1,
  pop_growth = (pop_estimate / first(pop_estimate)) - 1
)]

# --- Step 2: Share of total spending (65+) ---
# Total spending across all age groups
total_spending_all <- age_dt_long[Measure == "Health", .(total = sum(value)), by = Year]

# Merge with 65+ spending
health_share_65plus <- merge(
  health_total[, .(Year, spend_65plus = value)],
  total_spending_all,
  by = "Year"
)[, share := spend_65plus / total]

# Plot share of total spending
p1 <- ggplot(health_share_65plus, aes(x = Year, y = share*100)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  scale_y_continuous_e61() +
  labs(title = "Share of Health Spending: 65+",
       y = "Health Social Transfers in Kind, % share", x = "Year",
       sources = c("ABS","e61"))

p1

save_e61("STIK_health.png",res=2)

# --- Step 3: Per person spend relative to average ---
pop_estimates <- merge(
  age_dt_long[Measure == "Health"],
  age_perperson_dt_long[Measure == "Health"],
  by = c("Year", "Measure", "variable"),
  suffixes = c("_total", "_perperson")
)[, .(Year, variable, population = value_total / value_perperson, total_spend = value_total)]

# Total population each year
total_population <- pop_estimates[, .(total_pop = sum(population)), by = Year]

# Average per person spend each year
avg_perperson_spend <- merge(
  total_spending_all, total_population, by = "Year"
)[, avg_spend := total / total_pop]

# Relative per person spend for 65+ group
health_perperson_corrected <- merge(
  health_perperson[, .(Year, value)], 
  avg_perperson_spend[, .(Year, avg_spend)], 
  by = "Year"
)[, share_perperson := value / avg_spend]

# Plot per person spending share
p2 <- ggplot(health_perperson_corrected, aes(x = Year, y = share_perperson)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  scale_y_continuous_e61(limits = c(1.4,1.7,0.05)) +
  labs(title = "Per Person Health Spending: 65+",
       y = "Relative Per Person Spend", x = "Year",
       sources = c("ABS","e61"))

p2

save_e61("health_relative_age.png",res=2)



### Income ----

colnames(STIK_per_person)

income_dt <-STIK_per_person[,.(Year,Measure,`Lowest (EDHI)`,`Second (EDHI)`,`Third (EDHI)`,`Fourth (EDHI)`,`Highest (EDHI)`)]

income_dt_long <- melt(income_dt,id.vars = c("Year","Measure"))

ggplot(income_dt_long[Measure == "Health"],aes(x=Year,y=value,fill=variable)) + geom_col(position = "dodge") + 
  coord_flip() + theme_e61(legend = "bottom")

prop_income_dt <- income_dt_long[, .(
  total_spend = sum(value, na.rm=TRUE),
  highest_spend = sum(value[variable == "Highest (EDHI)"], na.rm=TRUE),
  lowest_spend = sum(value[variable == "Lowest (EDHI)"], na.rm=TRUE)
), by = .(Year, Measure)]

prop_income_dt[, top_income_share := highest_spend / total_spend]
prop_income_dt[, bottom_income_share := lowest_spend / total_spend]

ggplot(prop_income_dt[Measure == "Health"],aes(x=Year,y=top_income_share)) + geom_col(position = "dodge") + 
  coord_flip()

ggplot(prop_income_dt[Measure == "Education"],aes(x=Year,y=top_income_share)) + geom_col(position = "dodge") + 
  coord_flip()

ggplot(prop_income_dt[Measure == "Other"],aes(x=Year,y=top_income_share)) + geom_col(position = "dodge") + 
  coord_flip()

prop_income_dt$Measure <- factor(prop_income_dt$Measure, levels = c("Gross Disposable Income","Private Final Consumption","Other","Education", "Health"))

ggplot(prop_income_dt[Measure %in% c("Health","Education","Other")],aes(x=Year,y=top_income_share,fill=Measure)) + geom_col() + 
  coord_flip()


prop_income_dt_full <- income_dt_long[!Measure %in% c("Gross Disposable Income","Private Final Consumption")][, .(
  total_spend = sum(value, na.rm=TRUE),
  highest_spend = sum(value[variable == "Highest (EDHI)"], na.rm=TRUE),
  lowest_spend = sum(value[variable == "Lowest (EDHI)"], na.rm=TRUE)
), by = .(Year, Measure)]

prop_income_dt_full[, top_income_share := highest_spend / total_spend]
prop_income_dt_full[, bottom_income_share := lowest_spend / total_spend]

ggplot(prop_income_dt_full,aes(x=Year,y=top_income_share)) + geom_col(position = "dodge")

ggplot(prop_income_dt_full, aes(x = Year, y = top_income_share)) +
  geom_col(position = "dodge") +
  coord_cartesian(ylim = c(0.1, 0.2)) +  # Show y-axis from 10% to 100%
  scale_y_continuous(labels = scales::percent) +
  theme_e61()

ggplot(melt(prop_income_dt_full[Measure == "Health",.(Year,Measure,top_income_share,bottom_income_share)],id.vars = c("Year","Measure")),aes(x=Year,y=value*100,fill=variable)) +
  geom_col(position = "dodge") +
  coord_cartesian(ylim = c(10, 22)) +  # Show y-axis from 10% to 100%
  scale_y_continuous() +
  scale_x_continuous(breaks = seq(min(prop_income_dt_full$Year), 
                                  max(prop_income_dt_full$Year), 
                                  by = 4)) +
  plab(c("Highest Quintile","Lowest Quintile"),x=c(2004,2012),y=c(21,21)) +
  labs_e61(title = "Public Health Spending on an Income Quintile",
           subtitle = "% of total Public Health Spending",
           x="")

save_e61("STIK_health_income.png",res=2,auto_scale = FALSE)
