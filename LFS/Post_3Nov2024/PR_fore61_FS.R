## Participation rate shift-share for Fiscal e61 project.
# Initial creation: 10/11/2024
# Last edit: 10/11/2024

library(tidyverse)
library(data.table)
library(theme61)
library(readabs)
library(seasonal)

rm(list = ls())

LFS <- read_abs(cat_no = "6202.0") 
setDT(LFS)

## Participation rate shift-share: gender and age
# Initial creation: 10/11/2024
# Last edit: 11/09/2025 - update for theme61

library(tidyverse)
library(data.table)
library(theme61)
library(readabs)
library(seasonal)

rm(list = ls())

## End date

end_date <- "2024-06-01"


## Set up data

LFS <- read_abs(cat_no = "6202.0") 
setDT(LFS)


## Gender

LFS_gender <- LFS %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original")  %>% filter(series == "Participation rate ;  Persons ;" | series == "Participation rate ;  > Males ;"  | series == "Participation rate ;  > Females ;") %>% select("date","series","value")

avg_gender_1990 <- LFS_gender[date >= as.Date("1990-01-01") & date <= as.Date("1990-12-31"),
                              .(avg_value = mean(value/100)), by = series]

ggplot(LFS_gender[date >= as.Date("1990-01-01")],aes(x=date,y=value/100,colour=series)) + geom_line() + 
  scale_y_continuous_e61(limits=c(0.40,0.85,0.10),labels=scales::percent_format()) + 
  labs_e61(title="Labour Force Participation",subtitle="By reported gender",y="",x="") + 
  plot_label("Male",y=0.78,x="1993-03-01",colour = palette_e61(3)[2]) + 
  plot_label("Total",y=0.68,x="1993-03-01",colour = palette_e61(3)[3]) + 
  plot_label("Female",y=0.58,x="1993-03-01",colour = palette_e61(3)[1]) +
  geom_hline(data = avg_gender_1990[series == "Participation rate ;  > Males ;"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(3)[2]) +
  geom_hline(data = avg_gender_1990[series == "Participation rate ;  Persons ;"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(3)[3]) +
  geom_hline(data = avg_gender_1990[series == "Participation rate ;  > Females ;"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(3)[1])

save_e61("LFP_gender.png",res=2,pad_width = 1,auto_scale = FALSE)

ggplot(LFS_gender[date >= as.Date("1990-01-01") & series == "Participation rate ;  Persons ;"],aes(x=date,y=value/100,colour=series)) + geom_line() + 
  scale_y_continuous_e61(limits=c(0.40,0.85,0.10),labels=scales::percent_format()) + 
  labs_e61(title="Labour Force Participation",y="",x="",sources="ABS") +
  geom_hline(data = avg_gender_1990[series == "Participation rate ;  Persons ;"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(1)[1])

save_e61("LFP.png",res=2,pad_width = 1,auto_scale = FALSE)

## Shift share

LFS_gender_share <- LFS %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original") %>% filter(series == "Labour force total ;  Persons ;" | series == "Labour force total ;  > Males ;"  | series == "Labour force total ;  > Females ;" | series == "Civilian population aged 15 years and over ;  Persons ;" | series == "Civilian population aged 15 years and over ;  > Males ;" | series == "Civilian population aged 15 years and over ;  > Females ;")

unique(LFS_gender_share[series_type == "Seasonally Adjusted"]$series)

LFS_gender_share <- LFS_gender_share[series_type == "Original"][,.(date,series,value)]

LFS_gender_share[, measure := ifelse(grepl("Labour force total", series), "Labour_Force", 
                                     ifelse(grepl("Civilian population", series), "Population", NA))]

LFS_gender_share[, gender := ifelse(grepl("Persons", series), "Total", 
                                    ifelse(grepl("Males", series), "Male", 
                                           ifelse(grepl("Females", series), "Female", NA)))]


list_of_adjusted_gender <- list()

combo_dt <- unique(LFS_gender_share[, .(measure, gender)])

# Loop over each unique combination of measure and gender to apply seasonal adjustment
for (i in 1:nrow(combo_dt)) {
  combo <- combo_dt[i]
  subset_data <- LFS_gender_share[measure == combo$measure & gender == combo$gender, .(date, value)]
  subset_data <- subset_data[order(date)]
  
  ts_data <- ts(subset_data$value, start = c(year(min(subset_data$date)), month(min(subset_data$date))), frequency = 12)
  seas_result <- try(seas(ts_data), silent = TRUE)
  
  # Check if seasonal adjustment succeeded
  if (!inherits(seas_result, "try-error") && !is.null(seas_result$series$s11)) {
    subset_data[, value_sa := as.numeric(seas_result$series$s11)]
  } else {
    subset_data[, value_sa := NA]
    warning(paste("Seasonal adjustment failed for:", combo$measure, combo$gender))
  }
  
  subset_data[, measure := combo$measure]
  subset_data[, gender := combo$gender]
  
  # Store the adjusted data in the list
  list_of_adjusted_gender[[paste(combo$measure, combo$gender, sep = "_")]] <- subset_data
}

LFS_gender_adjusted <- rbindlist(list_of_adjusted_gender)

LFS_gender_wide <- dcast(LFS_gender_adjusted, date + gender ~ measure, value.var = "value_sa")
LFS_gender_wide[, participation_rate := Labour_Force / Population]

# Dates used for discussing the shift-shares/give example plots - based on the midpoint of the years discussed in post
reference_dates <- as.Date(c("1990-06-01", "2000-06-01", "2008-06-01", "2014-06-01", "2019-06-01"))

baseline_values <- LFS_gender_wide[date %in% reference_dates & gender != "Total", .(
  participation_rate_baseline = participation_rate,
  population_baseline = Population
), by = .(date, gender)][,population_share_baseline := population_baseline/(sum(population_baseline)),by=date]

setnames(baseline_values, "date", "date_baseline")

# Add in the overall participation rate, as we are about to fiddle with shares so this is the "base total rate"
LFS_gender_wide[, total_participation_rate := sum(participation_rate * (Population / sum(Population))), by = date]

# Merge baseline participation rates and population shares for each reference date with the main data
LFS_hypothetical <- merge(LFS_gender_wide, baseline_values, by = "gender", allow.cartesian = TRUE, suffixes = c("", "_baseline"))

# Calculate the hypothetical total participation rate if subgroup participation rates are fixed at baseline values
LFS_hypothetical[, hypothetical_rate_fixed_pr := sum(participation_rate_baseline * (Population / sum(Population))), by = .(date, date_baseline)]

# Calculate the hypothetical total participation rate if subgroup population shares are fixed at baseline values
LFS_hypothetical[, hypothetical_rate_fixed_share := sum(participation_rate * population_share_baseline), by = .(date, date_baseline)]

LFS_plot_data <- LFS_hypothetical[, .(
  date, date_baseline, total_participation_rate, hypothetical_rate_fixed_pr, hypothetical_rate_fixed_share
)]
LFS_plot_data <- melt(LFS_plot_data, id.vars = c("date", "date_baseline"), 
                      variable.name = "scenario", value.name = "participation_rate")


# Plot actual vs. hypothetical total participation rates for each baseline scenario
ggplot(LFS_plot_data[date >= as.Date("1990-01-01")], aes(x = date, y = participation_rate, colour = scenario)) +
  geom_line() +
  facet_wrap(~ date_baseline, scales = "free_y") +
  labs(title = "Total Participation Rate Scenarios",
       subtitle = "Actual vs Hypothetical Scenarios with Fixed Subgroup Participation Rates or Population Shares",
       x = "Date",
       y = "",
       colour = "Scenario") + scale_y_continuous_e61(labels=scales::percent_format(),limits = c(0.5,0.8,0.1),sec_axis = FALSE) + theme_e61(legend = "bottom")

save_e61("rate_vs_pop_gender.png",res=2,pad_width = 3,auto_scale = FALSE,sources="ABS")

## Single changes in PR
# Here we are using the fact that PR = sum(sg_PR*sg_share)
# Need to adjust shares - at current they can be more or less than 1

# Fixed Female Participation Rate - allow Male PR and population shares to vary
LFS_hypothetical[, hypothetical_rate_fixed_female := 
                   sum(ifelse(gender == "Female", participation_rate_baseline, participation_rate) * (Population / sum(Population))),
                 by = .(date, date_baseline)]

# Fixed Male Participation Rate - allow Female PR and population shares to vary
LFS_hypothetical[, hypothetical_rate_fixed_male := 
                   sum(ifelse(gender == "Male", participation_rate_baseline, participation_rate) * (Population / sum(Population))),
                 by = .(date, date_baseline)]

LFS_plot_data2 <- LFS_hypothetical[, .(
  date, date_baseline, total_participation_rate, hypothetical_rate_fixed_female, hypothetical_rate_fixed_male
)]

LFS_plot_data2 <- melt(LFS_plot_data2, id.vars = c("date", "date_baseline"), 
                       variable.name = "scenario", value.name = "participation_rate")


ggplot(LFS_plot_data2[date >= as.Date("1990-01-01")], aes(x = date, y = participation_rate, color = scenario)) +
  geom_line() +
  facet_wrap(~ date_baseline, scales = "free_y") +
  labs_e61(title = "Total Participation Rate Scenarios with Fixed Female or Male PR",
           subtitle = "Actual vs Hypothetical Scenarios with Fixed Female or Male Participation Rates",
           x = "Date",
           y = "",
           colour = "Scenario") + scale_y_continuous_e61(labels=scales::percent_format(),limits = c(0.5,0.8,0.1),sec_axis = FALSE) + theme_e61(legend = "bottom")

save_e61("rate_by_gender.png",res=2,pad_width = 3,auto_scale = FALSE,sources="ABS")


## Age 
# 
# unique(LFS$table_no)
# #unique(LFS[table_no > 6202014 & table_no < 6202019,.(series,table_no)])
# a <- LFS[table_no == "6202022"]

LFS_detailed <- read_abs(cat_no = "6291.0.55.001") %>% filter(table_title == "Table 01. Labour force status by Age, Social marital status, and Sex")
setDT(LFS_detailed)

unique(LFS_detailed$series)

age_groups <- c("15-24", "25-34", "35-44", "45-54", "55-64","65 years and over")

list_of_tables <- list()

for (age_group in age_groups) {
  
  if (age_group != "65 years and over"){
    dt <- LFS_detailed[series == paste0("> ", age_group, " years ;  Participation rate ;  Persons ;")][, .(date, value)]
  } else {
    dt <- LFS_detailed[series == paste0(age_group, " ;  Participation rate ;  Persons ;")][, .(date, value)]
  }
  
  dt <- dt[order(date)]
  
  # Check if there are any NA values in 'value' and exclude them (interpolating might be better, check to see the gaps manually)
  if (any(is.na(dt$value))) {
    dt <- dt[!is.na(value)]
  }
  
  value_ts <- ts(dt$value, start = c(year(min(dt$date)), month(min(dt$date))), frequency = 12)
  seas_result <- try(seas(value_ts), silent = TRUE)
  
  # Check if seasonal adjustment succeeded and if '$series$s11' is available from that objecvt
  if (!inherits(seas_result, "try-error") && !is.null(seas_result$series$s11)) {
    value_sa <- seas_result$series$s11
    dt[, value_sa := as.numeric(value_sa)]
  } else {
    dt[, value_sa := NA]  # Set to NA if seasonal adjustment fails
    warning(paste("Seasonal adjustment failed for age group:", age_group))
  }
  
  list_of_tables[[age_group]] <- dt
}

age_PR_dt <- rbindlist(list_of_tables, idcol = "age_group")

avg_age_1990 <- age_PR_dt[date >= as.Date("1990-01-01") & date <= as.Date("1990-12-31"),
                          .(avg_value = mean(value_sa)), by = age_group]

ggplot(age_PR_dt[date >= as.Date("1990-01-01")],aes(x=date,y=value_sa,colour=age_group)) + geom_line() +
  labs_e61(title = "Labour Force Participation by Age",y="",source = "ABS") +
  scale_y_continuous_e61(limits=c(0,100,by=20)) +
  plab(label = c("15-24","25-34","35-44","45-54","55-64","65+"),x = rep(as.Date("2010-01-01"),times=6),y=c(47,42,37,32,27,22)) +
  geom_hline(data = avg_age_1990[age_group == "15-24"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(6)[1]) +
  geom_hline(data = avg_age_1990[age_group == "25-34"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(6)[2]) +
  geom_hline(data = avg_age_1990[age_group == "35-44"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(6)[3]) +
  geom_hline(data = avg_age_1990[age_group == "45-54"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(6)[4]) +
  geom_hline(data = avg_age_1990[age_group == "55-64"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(6)[5]) +
  geom_hline(data = avg_age_1990[age_group == "65 years and over"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(6)[6])

save_e61("LFP_age.png",res=2,pad_width = 1,auto_scale = FALSE)

#unique(LFS_detailed$series)

## Construct age shift shares

list_of_population_tables <- list()
list_of_lf_tables <- list()

# Construct population counts
for (age_group in age_groups) {
  
  if (age_group != "65 years and over"){
    dt <- LFS_detailed[series == paste0("> ", age_group, " years ;  Civilian population aged 15 years and over ;  Persons ;")][, .(date, value)]
  } else {
    dt <- LFS_detailed[series == paste0(age_group, " ;  Civilian population aged 15 years and over ;  Persons ;")][, .(date, value)]
  }
  
  dt <- dt[order(date)]
  
  # Check if there are any NA values in 'value' and exclude them (interpolating might be better, check to see the gaps manually)
  if (any(is.na(dt$value))) {
    dt <- dt[!is.na(value)]
  }
  
  value_ts <- ts(dt$value, start = c(year(min(dt$date)), month(min(dt$date))), frequency = 12)
  seas_result <- try(seas(value_ts), silent = TRUE)
  
  # Check if seasonal adjustment succeeded and if '$series$s11' is available from that objecvt
  if (!inherits(seas_result, "try-error") && !is.null(seas_result$series$s11)) {
    value_sa <- seas_result$series$s11
    dt[, value_sa := as.numeric(value_sa)]
  } else {
    dt[, value_sa := NA]  # Set to NA if seasonal adjustment fails
    warning(paste("Seasonal adjustment failed for age group:", age_group))
  }
  
  list_of_population_tables[[age_group]] <- dt
}

# Construction labour force counts
for (age_group in age_groups) {
  
  if (age_group != "65 years and over"){
    dt <- LFS_detailed[series == paste0("> ", age_group, " years ;  Labour force total ;  Persons ;")][, .(date, value)]
  } else {
    dt <- LFS_detailed[series == paste0(age_group, " ;  Labour force total ;  Persons ;")][, .(date, value)]
  }
  
  dt <- dt[order(date)]
  
  # Check if there are any NA values in 'value' and exclude them (interpolating might be better, check to see the gaps manually)
  if (any(is.na(dt$value))) {
    dt <- dt[!is.na(value)]
  }
  
  value_ts <- ts(dt$value, start = c(year(min(dt$date)), month(min(dt$date))), frequency = 12)
  seas_result <- try(seas(value_ts), silent = TRUE)
  
  # Check if seasonal adjustment succeeded and if '$series$s11' is available from that objecvt
  if (!inherits(seas_result, "try-error") && !is.null(seas_result$series$s11)) {
    value_sa <- seas_result$series$s11
    dt[, value_sa := as.numeric(value_sa)]
  } else {
    dt[, value_sa := NA]  # Set to NA if seasonal adjustment fails
    warning(paste("Seasonal adjustment failed for age group:", age_group))
  }
  
  list_of_lf_tables[[age_group]] <- dt
}

############ Now generate plots

# For clarity we will cut the number of dates on this a bit
reference_dates <- as.Date(c("1990-06-01", "2000-06-01", "2008-06-01", "2019-06-01"))

# Set up population data
age_pop_dt <- rbindlist(list_of_population_tables, idcol = "age_group")

# Note: SA failed for the 65+ group, so use actuals.
age_pop_dt[age_group == "65 years and over", value_sa := value]

# Set up labour force data
age_LF_dt <- rbindlist(list_of_lf_tables, idcol = "age_group")

merged_age_data <- merge(age_LF_dt, age_pop_dt, by = c("date", "age_group"), suffixes = c("_lf", "_pop"))
merged_age_data[, participation_rate := value_sa_lf / value_sa_pop]

# Calculate baseline participation rates and population shares at each reference date for each age group
baseline_values_age <- merged_age_data[date %in% reference_dates & age_group != "Total", .(
  participation_rate_baseline = participation_rate,
  population_baseline = value_sa_pop
), by = .(date, age_group)][, population_share_baseline := population_baseline / sum(population_baseline), by = date]

setnames(baseline_values_age, "date", "baseline_date")

hypothetical_data_age <- merge(merged_age_data, baseline_values_age, by = "age_group", allow.cartesian = TRUE)

# Hypothetical total participation rate if each age group’s participation rate is fixed at baseline values for all
hypothetical_data_age[, hypothetical_rate_fixed_agePR := 
                        sum(participation_rate_baseline * (value_sa_pop / sum(value_sa_pop))), 
                      by = .(date, baseline_date)]

# Hypothetical total participation rate if population shares are fixed at baseline values for all
hypothetical_data_age[, hypothetical_rate_fixed_pop_share := 
                        sum(participation_rate * population_share_baseline), 
                      by = .(date, baseline_date)]

unique(hypothetical_data_age[,sum(population_share_baseline),by=.(date)]$V1)

ggplot(hypothetical_data_age,aes(x=date,y=hypothetical_rate_fixed_agePR,colour = as.factor(baseline_date))) + geom_line()+
  theme_e61(legend = "bottom")

# Add in the total rate for graph comparisons
total_data <- merged_age_data[, .(
  total_participation_rate = sum(value_sa_lf, na.rm = TRUE) / sum(value_sa_pop, na.rm = TRUE)
), by = date]

# Merge the total participation rate with the hypothetical data for plotting
plot_data_age <- merge(hypothetical_data_age, total_data, by = c("date"), all.x = TRUE)

# Reshape data for plotting
plot_data_age <- plot_data_age[, .(
  date, baseline_date, total_participation_rate, hypothetical_rate_fixed_agePR, hypothetical_rate_fixed_pop_share
)]
plot_data_age <- melt(plot_data_age, id.vars = c("date", "baseline_date"), 
                      variable.name = "scenario", value.name = "participation_rate")

# Plot the scenarios
ggplot(plot_data_age[date >= as.Date("1990-01-01")], aes(x = date, y = participation_rate, color = scenario)) +
  geom_line() +
  facet_wrap(~ baseline_date, scales = "free_y") +
  labs_e61(title = "Total Participation Rate Scenarios by Age Group",
           subtitle = "Actual vs Hypothetical Scenarios with Fixed Age Group Participation Rates or Population Shares",
           x = "Date",
           y = "",
           colour = "Scenario",
           sources = "ABS") +
  scale_y_continuous_e61(labels = scales::percent_format(),limits = c(0.5,0.8,0.1),sec_axis = FALSE) + theme_e61(legend = "bottom")


save_e61("age_total.png",res=2,pad_width = 2,auto_scale = FALSE)

## Now by age group

merged_age_data[, total_participation_rate := sum(participation_rate * (value_sa_pop / sum(value_sa_pop))), by = date] 
age_hypothetical <- merge(merged_age_data, baseline_values_age, by = "age_group", allow.cartesian = TRUE)

for (age in unique(age_hypothetical$age_group)) {
  
  # Create a column for each age group scenario, fixing only that age group's participation rate
  age_hypothetical[, paste0("hypothetical_rate_fixed_", age) := 
                     sum(ifelse(age_group == age, participation_rate_baseline, participation_rate) * 
                           (value_sa_pop / sum(value_sa_pop))), 
                   by = .(date, baseline_date)]
}



plot_data_age2 <- age_hypothetical[, c("date", "baseline_date", "total_participation_rate", 
                                       grep("hypothetical_rate_fixed_", names(age_hypothetical), value = TRUE)), with = FALSE]

# Reshape for plotting
plot_data_age2 <- melt(plot_data_age2, id.vars = c("date", "baseline_date"), 
                       variable.name = "scenario", value.name = "participation_rate")

ggplot(plot_data_age2[date >= as.Date("1990-01-01") & baseline_date %in% c(as.Date("1990-06-01"))], aes(x = date, y = participation_rate, colour = scenario,size=scenario)) +
  geom_line() +
  facet_wrap(~ baseline_date, scales = "free_y") +
  labs_e61(title = "Total Participation Rate Scenarios by Age Group",
           subtitle = "Actual vs Hypothetical Scenarios with Fixed Age Group Participation Rates",
           x = "Date",
           y = "",
           colour = "Scenario",
           sources = "ABS") +
  scale_y_continuous_e61(labels=scales::percent_format(),limits = c(0.55,0.7,0.05)) + 
  scale_size_manual(values = c(1,rep(0.5,times=6))) +
  plab(c("Total","15-24","25-34","35-44","45-54","55-64","65+"),x=rep(as.Date("1995-01-01"),times=7),y=c(0.68,0.67,0.66,0.59,0.58,0.57,0.56))
#scale_linetype_manual(values = c("solid",rep("solid",times=6)))

save_e61("Fixed_agegroup_PR.png",res=2,pad_width = 1,auto_scale = FALSE)

############# Finally, a bar plot with the "contribution shares for the change since 1990 and another since 2019.

baseline_1990 <- merged_age_data[date == as.Date("1990-06-01")]
current_2024 <- merged_age_data[date == as.Date(end_date)]

actual_1990 <- merged_age_data[date == as.Date("1990-06-01"), sum(participation_rate * (value_sa_pop / sum(value_sa_pop)))]
actual_2024 <- merged_age_data[date == as.Date(end_date), sum(participation_rate * (value_sa_pop / sum(value_sa_pop)))]

total_change <- actual_2024 - actual_1990 # This is the PR change that forms the base of the contribution

# Extract columns with hypothetical scenarios
hypothetical_columns <- grep("hypothetical_rate_fixed_", names(age_hypothetical), value = TRUE)

# Initialize a data.table to store contributions
contribution_data <- data.table(age_group = gsub("hypothetical_rate_fixed_", "", hypothetical_columns))

# Calculate the contribution of each age group’s changes in participation rate or population share
contribution_data[, Participation := sapply(hypothetical_columns, function(col) {
  hypothetical_rate <- unique(age_hypothetical[date == as.Date(end_date) & baseline_date == as.Date("1990-06-01"), get(col)])
  actual_2024 - hypothetical_rate  # Difference from actual total participation rate in 2024
})]

# That is the contributions from the rate changes, we also need the share changes

age_hypothetical_share <- merge(merged_age_data, baseline_values_age, by = "age_group", allow.cartesian = TRUE)
age_hypothetical_share[, pop_share := value_sa_pop / sum(value_sa_pop), by = .(date, baseline_date)]

# Rescaling the population shares for hypotheticals - this is not working right now, instead need to rescale the code below
# for (age in unique(age_hypothetical_share$age_group)) {
#   age_hypothetical_share[, paste0("hypothetical_rate_fixed_", age) := {
#     # Get baseline share for the fixed age group
#     fixed_share <- population_share_baseline[age_group == age]
#     
#     # For each date and baseline combination:
#     # If this is the fixed age group, use its baseline share
#     # If not, scale other shares proportionally to sum to (1 - fixed_share)
#     ifelse(age_group == age,
#            fixed_share,
#            # Scale other shares proportionally
#            pop_share * ((1 - fixed_share) / sum(pop_share[age_group != age]))
#     ) * participation_rate
#   }, by = .(date, baseline_date)]
# }


# age_hypothetical_share <- merge(merged_age_data, baseline_values_age, by = "age_group", allow.cartesian = TRUE)
# 
# age_hypothetical_share[,pop_share := value_sa_pop / sum(value_sa_pop),by=.(date,baseline_date)]

age_hypothetical_share[date == as.Date(end_date) & baseline_date == as.Date("1990-06-01")]

# Note: keeping the "rate" name the same is confusing - but this is population share.
for (age in unique(age_hypothetical_share$age_group)) {
  
  # Calculate the baseline share for the age group being fixed
  baseline_share <- age_hypothetical_share[age_group == age & baseline_date == as.Date("1990-06-01"), population_share_baseline]
  
  # Calculate the hypothetical total participation rate with fixed population share for this age group
  age_hypothetical_share[, paste0("hypothetical_rate_fixed_", age) := 
                           sum(ifelse(age_group == age, 
                                      # Keep baseline share for the fixed age group
                                      baseline_share, 
                                      # Scale down the other shares proportionally
                                      (1 - baseline_share) * (value_sa_pop / sum(value_sa_pop[-which(age_group == age)]))
                           ) * participation_rate),
                         by = .(date, baseline_date)]
}

age_hypothetical_share[date == as.Date(end_date) & baseline_date == as.Date("1990-06-01")]


contribution_data[, Population := sapply(hypothetical_columns, function(col) {
  hypothetical_rate <- unique(age_hypothetical_share[date == as.Date(end_date) & baseline_date == as.Date("1990-06-01"), get(col)])
  actual_2024 - hypothetical_rate  # Difference from actual total participation rate in 2024
})]

# Prepare data for plotting
plot_data <- melt(contribution_data, id.vars = "age_group", 
                  variable.name = "component", value.name = "contribution")

ggplot(plot_data, aes(x = age_group, y = contribution*100, fill = component)) +
  geom_bar(stat = "identity",position="dodge") +
  labs(title = "Decomposition of Change in PR by Age Group (June 1990 to June 2025)",
       subtitle = "",
       x = "Age Group",
       y = "PPT",
       fill = "Component") + theme_e61(legend = "bottom") +
  scale_y_continuous_e61(limits = c(-5,5,1)) + coord_flip()

save_e61("Share_decomp_2025.png",res=2,pad_width = 1,auto_scale = FALSE)

#### As well as this above plot, we want to also use the new PRs and the 2065 population shares to map out the change in participation if group participation rates don't keep rising.


