## Last update:  1/04/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan 
# A simulation of poverty rates using our median income lines only (BHC and AHC)

library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(tidyverse)
library(data.table)
library(readxl)
library(Hmisc)


rm(list=ls())
gc()

### Assumptions

liquid_thresh = 13 # Only applied at the bottom for now - check to apply more widely

housing_cost_inflator = 1.12 # 1 represents keeping prices at their SIH 2020 level

IR_house_single <- 0 # A random figure for "imputed rent" at household level
IR_house_couple <- 0 # A random figure for "imputed rent" at household level
IR <- 0 # A random figure for "imputed rent" at equivalised level

## Use the dataset with flags from Matthew M
Rep_rates_df <- read_csv("C:/Users/MattNolan/Downloads/RRs_csv 3.csv") # Work version
#Rep_rates_df <- read_csv("C:/Users/OEM/Downloads/RRs_csv 3.csv") # Home version

setDT(Rep_rates_df)

min(Rep_rates_df$hours)

hour_limit <- 30 # Two versions used - 30 for FT, 5 for the general.

#### Data setup ----

# Bug in the tax calc where negative incomes returned NAs on HECS replayments. FiX! 

Rep_rates_df$current_HECS_payment <- ifelse(is.na(Rep_rates_df$current_HECS_payment), 0,
                                            Rep_rates_df$current_HECS_payment )
Rep_rates_df$hours0_HECS_payment <- ifelse(is.na(Rep_rates_df$hours0_HECS_payment), 0,
                                           Rep_rates_df$hours0_HECS_payment )


Rep_rates_df$current_IT <- Rep_rates_df$current_income_tax + Rep_rates_df$current_HECS_payment
Rep_rates_df$hours0_IT <- Rep_rates_df$hours0_income_tax + Rep_rates_df$hours0_HECS_payment


Rep_rates_df$current_net_income <- ifelse(is.na(Rep_rates_df$current_net_income), 
                                          Rep_rates_df$current_gross_income + Rep_rates_df$current_net_fam_a + 
                                            Rep_rates_df$current_net_fam_b + Rep_rates_df$current_RA + 
                                            Rep_rates_df$current_ES  - Rep_rates_df$current_IT, 
                                          Rep_rates_df$current_net_income)

Rep_rates_df[is.na(current_net_income_partner)] # These are partners with negative income

Rep_rates_df <- Rep_rates_df[!is.na(current_net_income_partner)]

Rep_rates_df$hours0_net_income <- ifelse(is.na(Rep_rates_df$hours0_net_income), 
                                         Rep_rates_df$hours0_gross_income + Rep_rates_df$hours0_net_fam_a + 
                                           Rep_rates_df$hours0_net_fam_b + Rep_rates_df$hours0_RA + 
                                           Rep_rates_df$hours0_ES - Rep_rates_df$hours0_IT, 
                                         Rep_rates_df$hours0_net_income)

Rep_rates_df[,IU_agg := fifelse(partnered == 1 & Numb_dep == 0,"Couple",
                                fifelse(partnered == 1 & Numb_dep > 0, "Couple + Dep",
                                        fifelse(partnered == 0 & Numb_dep == 0, "Single", "Single + Dep")))]

## Calculate Replacement Rates! 

Rep_rates_df[,":=" (net_inc_ratio = hours0_net_income/current_net_income,grs_inc_ratio = hours0_gross_income/current_gross_income)]


# Here, if there is not capital income just do the straight RR. If gross_income is zero then there is no "taxable benefits" so also set to zero.
Rep_rates_df[,":=" (grs_RR = (hours0_gross_income-NonWageIncome)/(current_gross_income-NonWageIncome),
                    net_RR = fifelse(NonWageIncome == 0, hours0_net_income/current_net_income,
                                     fifelse(hours0_gross_income == 0,0,
                                             ((hours0_net_income-(NonWageIncome - NonWageIncome/hours0_gross_income*hours0_IT))/(current_net_income-(NonWageIncome - NonWageIncome/current_gross_income*current_IT))))))]

Rep_rates_df$hours0_wk_partner_earnings <- Rep_rates_df$hours0_net_income_partner 

Rep_rates_df$current_wk_partner_earnings <- Rep_rates_df$current_net_income_partner 

Rep_rates_df[,":=" (grs_RR_household = (hours0_gross_income + hours0_wk_partner_earnings -NonWageIncome)/(current_gross_income + current_wk_partner_earnings -NonWageIncome),
                    net_RR_household = fifelse(NonWageIncome == 0, (hours0_net_income + hours0_wk_partner_earnings) / (current_net_income + current_wk_partner_earnings),
                                               fifelse(hours0_gross_income + hours0_wk_partner_earnings == 0,0,
                                                       ((hours0_net_income + hours0_wk_partner_earnings -(NonWageIncome - NonWageIncome/hours0_gross_income*hours0_IT))/(current_net_income + current_wk_partner_earnings -(NonWageIncome - NonWageIncome/current_gross_income*current_IT))))))]


# Clean the Data - This should already be done for some of this in the tax Calc stuff
Rep_rates_df <- subset(Rep_rates_df, wage > 15)

# Check age (restrictions applied later - checking they aren't in the base dataset) - the population here is 22 to 54. So calculations are based on the median income for that age group. That is ok but just make this clear in the write-up (as OECD standard is full population which should reduce this line: https://www.oecd.org/en/data/indicators/poverty-rate.html)  XXXX
min(Rep_rates_df$AGEEC)
max(Rep_rates_df$AGEEC)


# Drop those with Negative incomes 

Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$current_net_income >= 0)
Rep_rates_df <- subset(Rep_rates_df , Rep_rates_df$hours0_net_income >= 0)
Rep_rates_df <- subset(Rep_rates_df , Rep_rates_df$NonWageIncome >= 0)

setDT(Rep_rates_df)

## Calculate medians - this is for the calculation below
# Add adjustment to weights due to "double counting" certain individuals. Would be helpful to have hhld_id and person_id to improve this. XXXX
Rep_rates_df[,hhld_wgt := fifelse(partnered == 1 & current_work_income_partner > 20*hour_limit,SIHPSWT/2,SIHPSWT)]

Rep_rates_df$hours0_wk_partner_earnings <- ifelse(is.na(Rep_rates_df$hours0_wk_partner_earnings), Rep_rates_df$hours0_gross_income_partner - Rep_rates_df$hours0_income_tax_partner + Rep_rates_df$hours0_RA_partner + Rep_rates_df$hours0_ES_partner, Rep_rates_df$hours0_wk_partner_earnings )

Rep_rates_df$hhincome <- Rep_rates_df$hours0_net_income + Rep_rates_df$NonWageIncome + Rep_rates_df$hours0_wk_partner_earnings

Rep_rates_df$hhincome <- ifelse(Rep_rates_df$hhincome < 0, 0, Rep_rates_df$hhincome)

Rep_rates_df[,":=" (eq_scale = 1 + 0.5*partnered + 0.3*Numb_dep)][,eq_hhinc := hhincome/eq_scale][,hhld_size := 1 + partnered + Numb_dep]

# Calculate household pre-income.
Rep_rates_df[,hhincome_pre := current_net_income + NonWageIncome + current_wk_partner_earnings] 

Rep_rates_df$hhincome_pre <- ifelse(Rep_rates_df$hhincome_pre < 0, 0, Rep_rates_df$hhincome_pre)

Rep_rates_df[,eq_hhinc_pre := hhincome_pre/eq_scale]

Rep_rates_df[, weighted_median_50 := wtd.quantile(eq_hhinc_pre, weights = hhld_size * hhld_wgt, probs = 0.5, na.rm = TRUE)*0.5] 
Rep_rates_df[, weighted_median_60 := wtd.quantile(eq_hhinc_pre, weights = hhld_size * hhld_wgt, probs = 0.5, na.rm = TRUE)*0.6] 

## Before subsetting, create AHC measures
# First generate the AHC incomes - by removing CRA, and subtracting housing costs. HCs are defined as rent or mortgage payments - so rates and insurance payments currently excluded.

Rep_rates_df[,Rent := fifelse(is.na(Rent),0,Rent)]

Rep_rates_df[,current_AHC := hhincome_pre - Rent*housing_cost_inflator - Weekly_Mortgage_Repayments*housing_cost_inflator]# - current_RA - current_RA_partner: rent assistance is in the income where appropriate, so already net of rent
Rep_rates_df[,hours0_AHC := hhincome - Rent*housing_cost_inflator - Weekly_Mortgage_Repayments*housing_cost_inflator]#  - hours0_RA - hours0_RA_partner: So changes in RA are implicitly captured in changes in the net rent paid here for the poverty line.

# AHC_check <- melt(Rep_rates_df_subset[,.(id = seq(1:nrow(Rep_rates_df_subset)),IU_agg,hhincome,hours0_AHC)],id.vars = c("IU_agg", "id"))
# 
# ggplot(AHC_check,aes(x=log(value),colour=variable)) + geom_density() + theme_e61(legend = "bottom")
# 
# AHC_check[variable == "hours0_AHC" & value < 0,.N]/AHC_check[variable == "hours0_AHC",.N] # 11% go negative in AHC terms after job loss
# 
# AHC_check2 <- melt(Rep_rates_df_subset[,.(id = seq(1:nrow(Rep_rates_df_subset)),IU_agg,hhincome_pre,current_AHC)],id.vars = c("IU_agg", "id")) # Check current AHC income
# 
# ggplot(AHC_check2,aes(x=log(value),colour=variable)) + geom_density() + theme_e61(legend = "bottom")
# 
# AHC_check2[variable == "current_AHC" & value < 0,.N]/AHC_check2[variable == "current_AHC",.N] # 0.4% are negative in a AHC sense prior to job loss.

# Now calculate the median income line for this income metric. First, the new scale.

Rep_rates_df[,AHC_eq_scale := 1 + 0.724*partnered + 0.35*Numb_dep] # New scale based on this (https://www.stats.govt.nz/assets/Uploads/Methods/Measuring-child-poverty-Equivalence-scale/measuring-child-poverty-equivalence-scale.pdf) due to the lower economies of scale in non-housing expenditures.

Rep_rates_df[,":=" (eq_current_AHC = current_AHC/AHC_eq_scale,eq_hours0_AHC = hours0_AHC/AHC_eq_scale)]

# Now calculate 50% of the median of eq_current_AHC

Rep_rates_df[, AHC_weighted_median_50 := wtd.quantile(eq_current_AHC, weights = hhld_size * hhld_wgt, probs = 0.5, na.rm = TRUE)*0.5] 

Rep_rates_df[,wm_50_AHC := eq_hours0_AHC - AHC_weighted_median_50]
Rep_rates_df[,actual_gap_AHC := (eq_hours0_AHC - AHC_weighted_median_50)*AHC_eq_scale]
Rep_rates_df[,pre_wm_50_AHC := eq_current_AHC - AHC_weighted_median_50]


### Subset data to remove those working less than the fixed hours. Also do age restrictions here ----
Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$AGEEC > 21)

Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$AGEEC < 55)

Rep_rates_df_subset <- subset(Rep_rates_df, Rep_rates_df$hours > hour_limit)

## Create liquid asset categories

Rep_rates_df_subset[, Liquid_Assets_Person_group := fifelse(Liquid_Assets_Person < 1000, "< 1000",
                                                            fifelse(Liquid_Assets_Person >= 1000 & Liquid_Assets_Person < 10000, "1,000 - 10,000",
                                                                    fifelse(Liquid_Assets_Person >= 10000 & Liquid_Assets_Person < 50000,
                                                                            "10,000 - 50,000",
                                                                            "50,000+")))]

Rep_rates_df_subset$weeks_of_liquid_assets <- Rep_rates_df_subset$Liquid_Assets_Person / Rep_rates_df_subset$current_work_income

# Replace weeks_of_liquid_assets with 0 if numerator or denominator are negative
Rep_rates_df_subset$weeks_of_liquid_assets[Rep_rates_df_subset$Liquid_Assets_Person < 0 | Rep_rates_df_subset$current_work_income < 0] <- 0

# Replace weeks_of_liquid_assets with 104 if it exceeds 104
Rep_rates_df_subset$weeks_of_liquid_assets[Rep_rates_df_subset$weeks_of_liquid_assets > 104] <- 104

## Add benefit eligibility status

Rep_rates_df_subset[, eligibility_status := fifelse(
  hours0_taxable_benefit > 0 & (hours0_net_fam_a_income + hours0_net_fam_b_income > 0), 
  "Benefit + FTB Eligible", fifelse(
    hours0_taxable_benefit > 0, 
    "Benefit Eligible", fifelse(
      (hours0_net_fam_a_income + hours0_net_fam_b_income > 0), 
      "FTB Eligible", 
      "Ineligible"
    )
  )
)]

Rep_rates_df_subset[, eligibility_status := factor(eligibility_status, 
                                                   levels = c("Ineligible", "Benefit Eligible", 
                                                              "FTB Eligible", "Benefit + FTB Eligible"))]

#### Poverty lines ----
# Only add median lines for this code - AHC constructed earlier

Rep_rates_df_subset[, normalized_weight_total := SIHPSWT / sum(SIHPSWT)] # Broadest normalised weights - construct other "by group" weightings when needed

Rep_rates_df_subset[,wm_50_BHC := fifelse(Home_owner == 1,eq_hhinc - weighted_median_50 + IR,eq_hhinc - weighted_median_50)]
Rep_rates_df_subset[,actual_gap_BHC := (eq_hhinc - weighted_median_50)*eq_scale]
Rep_rates_df_subset[,wm_60_BHC := fifelse(Home_owner == 1,eq_hhinc - weighted_median_60 + IR,eq_hhinc - weighted_median_60)]
Rep_rates_df_subset[,pre_wm_50_BHC := fifelse(Home_owner == 1,eq_hhinc_pre - weighted_median_50 + IR,eq_hhinc_pre - weighted_median_50)]
Rep_rates_df_subset[,pre_wm_60_BHC := fifelse(Home_owner == 1,eq_hhinc_pre - weighted_median_60 + IR,eq_hhinc_pre - weighted_median_60)]

ggplot(Rep_rates_df_subset, aes(x = wm_50_BHC, weight = hhld_size * hhld_wgt)) +
  geom_density(alpha = 0.1) +  
  labs(
    title = "Distribution of Poverty Gap by Weeks of Liquid Assets",
    x = "Poverty Gap",
    y = "Weighted Count"
  ) + xlim(-1500,1500) + theme_e61(legend = "bottom")+ geom_vline(xintercept = -250,linetype = "dashed") + geom_vline(xintercept = 0,linetype = "dashed")

ggplot(Rep_rates_df_subset, aes(x = wm_50_AHC, weight = hhld_size * hhld_wgt)) +
  geom_density(alpha = 0.1) +  
  labs(
    title = "Distribution of Poverty Gap by Weeks of Liquid Assets",
    x = "Poverty Gap",
    y = "Weighted Count"
  ) + xlim(-1500,1500) + theme_e61(legend = "bottom") + geom_vline(xintercept = -250,linetype = "dashed") + geom_vline(xintercept = 0,linetype = "dashed")

# For wm_50_BHC
ggplot(Rep_rates_df_subset, aes(x = wm_50_BHC, weight = hhld_size * hhld_wgt)) +
  stat_ecdf(geom = "step") +
  labs(
    title = "Cumulative Distribution of Poverty Gap by Weeks of Liquid Assets (BHC)",
    x = "Poverty Gap",
    y = "Cumulative Probability"
  ) +
  xlim(-1500, 1500) +
  theme_e61(legend = "bottom") +
  geom_vline(xintercept = -250, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")

# For wm_50_AHC
ggplot(Rep_rates_df_subset, aes(x = wm_50_AHC, weight = hhld_size * hhld_wgt)) +
  stat_ecdf(geom = "step") +
  labs(
    title = "Cumulative Distribution of Poverty Gap by Weeks of Liquid Assets (AHC)",
    x = "Poverty Gap",
    y = "Cumulative Probability"
  ) +
  xlim(-1500, 1500) +
  theme_e61(legend = "bottom") +
  geom_vline(xintercept = -250, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")


# Reshape to long format
dt_long <- melt(
  Rep_rates_df_subset,
  #measure.vars = c("wm_50_BHC", "wm_50_AHC"),
  measure.vars = c("actual_gap_BHC","actual_gap_AHC"),
  variable.name = "Measure",
  value.name = "PovertyGap"
)

# Plot combined CDF
ggplot(dt_long, aes(x = PovertyGap, color = Measure, weight = hhld_size * hhld_wgt)) +
  stat_ecdf(geom = "step") +
  labs_e61(
    title = "Cumulative Distribution of Poverty Gap Income Concept",
    subtitle = "Equivalised Poverty Gap",
    x = "Poverty Gap",
    y = "",
    color = "Measure"
  ) +
  coord_cartesian(xlim = c(-500, 500)) +
  scale_y_continuous_e61(labels=scales::percent_format(),y_top = TRUE) +
  geom_vline(xintercept = -340, linetype = "dashed",colour = palette_e61(2)[2]) +
  geom_vline(xintercept = -110, linetype = "dashed", colour = palette_e61(2)[1]) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0.2, linetype = "dashed", colour = "red") +
  plab(label = c("20% in poverty","AHC = $340 increase","BHC = $110 increase"),x=c(100,-400,-200),y=c(0.1,0.9,0.7),colour=c("red",palette_e61(2)[2],palette_e61(2)[1])) 

dt_long_single <- melt(
  Rep_rates_df_subset[partnered == 0],
  #measure.vars = c("wm_50_BHC", "wm_50_AHC"),
  measure.vars = c("actual_gap_BHC","actual_gap_AHC"),
  variable.name = "Measure",
  value.name = "PovertyGap"
)

# Plot combined CDF
ggplot(dt_long_single, aes(x = PovertyGap, color = Measure, weight = hhld_size * hhld_wgt)) +
  stat_ecdf(geom = "step") +
  labs_e61(
    title = "Cumulative Distribution of Single Poverty Gap",
    subtitle = "Equivalised Poverty Gap",
    x = "Poverty Gap",
    y = "",
    color = "Measure"
  ) +
  coord_cartesian(xlim = c(-500, 500)) +
  scale_y_continuous_e61(labels=scales::percent_format(),y_top = TRUE) +
  geom_vline(xintercept = -340, linetype = "dashed",colour = palette_e61(2)[2]) +
  geom_vline(xintercept = -110, linetype = "dashed", colour = palette_e61(2)[1]) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0.2, linetype = "dashed", colour = "red") +
  plab(label = c("20% in poverty","AHC = $340 increase","BHC = $110 increase"),x=c(100,-400,-200),y=c(0.1,0.9,0.7),colour=c("red",palette_e61(2)[2],palette_e61(2)[1])) 


# Reshape to long format
dt_long_couple <- melt(
  Rep_rates_df_subset[partnered == 1],
  #measure.vars = c("wm_50_BHC", "wm_50_AHC"),
  measure.vars = c("actual_gap_BHC","actual_gap_AHC"),
  variable.name = "Measure",
  value.name = "PovertyGap"
)

# Plot combined CDF
ggplot(dt_long_couple, aes(x = PovertyGap, color = Measure, weight = hhld_size * hhld_wgt)) +
  stat_ecdf(geom = "step") +
  labs_e61(
    title = "Cumulative Distribution of Couple Poverty Gap",
    subtitle = "Equivalised Poverty Gap",
    x = "Poverty Gap",
    y = "",
    color = "Measure"
  ) +
  coord_cartesian(xlim = c(-500, 500)) +
  scale_y_continuous_e61(labels=scales::percent_format(),y_top = TRUE) +
  geom_vline(xintercept = -340, linetype = "dashed",colour = palette_e61(2)[2]) +
  geom_vline(xintercept = -110, linetype = "dashed", colour = palette_e61(2)[1]) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0.2, linetype = "dashed", colour = "red") +
  plab(label = c("20% in poverty","AHC = $340 increase","BHC = $110 increase"),x=c(100,-400,-200),y=c(0.1,0.9,0.7),colour=c("red",palette_e61(2)[2],palette_e61(2)[1])) 


# Summary stats
Rep_rates_df_subset[, {
  w <- hhld_size * hhld_wgt  # Individual weights
  prop_weighted <- sum(w * (wm_50_BHC < 0), na.rm = TRUE) / sum(w, na.rm = TRUE)
  .(weighted_proportion = prop_weighted)
}]

Rep_rates_df_subset[, {
  w <- hhld_size * hhld_wgt  # Individual weights
  prop_weighted <- sum(w * (wm_60_BHC < 0), na.rm = TRUE) / sum(w, na.rm = TRUE)
  .(weighted_proportion = prop_weighted)
}]

### Poverty sim -----

summary_med_AHC <- Rep_rates_df_subset[,.(Home_owner,liquid = weeks_of_liquid_assets > liquid_thresh,net_RR,poverty_med = wm_50_AHC <= 0,eligible = eligibility_status != "Ineligible",IU_agg,weight = hhld_wgt*hhld_size)]

summary_med_AHC

compute_results <- function(subset_data, income = "AHC",loop = 700) {
  # Determine variable suffix based on income type
  suffix <- if (income == "AHC") "_AHC" else ""
  suffix2 <- if (income == "AHC") "_AHC" else "_BHC"
  
  # Construct dynamic variable names
  eq_hours0_var <- fifelse(income == "AHC","eq_hours0_AHC", "eq_hhinc")
  wm_50_var <- paste0("wm_50", suffix)
  actual_var <- paste0("actual_gap", suffix2)
  median_var <- if (income == "AHC") "AHC_weighted_median_50" else "weighted_median_50"
  eq_var <- if (income == "AHC") "AHC_eq_scale" else "eq_scale"
  
  # Construct poverty gap
  #subset_data[, poverty_gap := get(eq_hours0_var) - get(median_var)]
  subset_data[, poverty_gap := get(actual_var)]
  
  
  # Define flags and groups
  subset_data[, ":=" (
    liquid = weeks_of_liquid_assets > liquid_thresh,
    #eligible = eligibility_status != "Ineligible" # To include FTB only in eligible
    eligible = eligibility_status %in% c("Benefit Eligible","Benefit + FTB Eligible") # Excludes those on FTB only
  )][, group := fifelse(!eligible, "Ineligible",
                        fifelse(Home_owner & liquid, "Eligible: Homeowner & Liquid",
                                fifelse(Home_owner & !liquid, "Eligible: Homeowner Only",
                                        fifelse(!Home_owner & liquid, "Eligible: Liquid Only",
                                                "Eligible: Neither"))))]
  
  # Normalize weights
  subset_data[, normalized_weight_hhld := hhld_wgt / sum(hhld_wgt, na.rm = TRUE)]
  
  # Prepare results table
  results <- data.table(
    i = integer(),
    group = character(),
    poverty_rate = numeric()
  )
  
  # Loop over benefit adjustments
  for (i in 0:loop) {
    adjusted_data <- copy(subset_data)
    #adjusted_data[, adjusted_poverty_gap := ifelse(hours0_taxable_benefit > 0, poverty_gap + i/get(eq_var), poverty_gap)] # Adjusting so that the increase is $1, which is a lower equivalised amount for larger families
    adjusted_data[, adjusted_poverty_gap := ifelse(hours0_taxable_benefit + hours0_net_fam_a_income + hours0_net_fam_b_income > 0, poverty_gap + i, poverty_gap)]
    
    group_results <- adjusted_data[, .(
      poverty_rate = sum((adjusted_poverty_gap < 0) * normalized_weight_hhld, na.rm = TRUE)
    ), by = .(group)][, i := i]
    
    results <- rbind(results, group_results, fill = TRUE)
  }
  
  # Order groups
  results[, group := factor(group, levels = c(
    "Ineligible",
    "Eligible: Homeowner & Liquid",
    "Eligible: Homeowner Only",
    "Eligible: Liquid Only",
    "Eligible: Neither"
  ))]
  
  return(results)
}

### The issue here is that these are "equivalized dollars" Nedd to change everyone back to actual dollars to make sense of this exercise XXX

AHC_poverty <- compute_results(Rep_rates_df_subset)

AHC_poverty[,.(poverty_rate = sum(poverty_rate)),by=.(i)]

ggplot(AHC_poverty, aes(x = i, y = poverty_rate * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("AHC Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE) +
  plab(label = c(
    "Ineligible",
    "Eligible: Homeowner & Liquid",
    "Eligible: Homeowner Only",
    "Eligible: Liquid Only",
    "Eligible: Neither"
  ), y = c(95,89,83,77,71),x=rep(150,times=5))

BHC_poverty <- compute_results(Rep_rates_df_subset,income="BHC")

BHC_poverty[,.(poverty_rate = sum(poverty_rate)),by=.(i)]

ggplot(BHC_poverty, aes(x = i, y = poverty_rate * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("BHC Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)+
  plab(label = c(
    "Ineligible",
    "Eligible: Homeowner & Liquid",
    "Eligible: Homeowner Only",
    "Eligible: Liquid Only",
    "Eligible: Neither"
  ), y = c(95,89,83,77,71),x=rep(150,times=5))

Single_BHC_poverty <- compute_results(Rep_rates_df_subset[partnered == 0],income="BHC")

ggplot(Single_BHC_poverty, aes(x = i, y = poverty_rate * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("Single BHC Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)+
  plab(label = c(
    "Ineligible",
    "Eligible: Homeowner & Liquid",
    "Eligible: Homeowner Only",
    "Eligible: Liquid Only",
    "Eligible: Neither"
  ), y = c(95,89,83,77,71),x=rep(150,times=5))

Coupled_BHC_poverty <- compute_results(Rep_rates_df_subset[partnered == 1],income="BHC")

ggplot(Coupled_BHC_poverty, aes(x = i, y = poverty_rate * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("Coupled BHC Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)+
  plab(label = c(
    "Ineligible",
    "Eligible: Homeowner & Liquid",
    "Eligible: Homeowner Only",
    "Eligible: Liquid Only",
    "Eligible: Neither"
  ), y = c(95,89,83,77,71),x=rep(150,times=5))

Child_BHC_poverty <- compute_results(Rep_rates_df_subset[Numb_dep > 0],income="BHC")

ggplot(Child_BHC_poverty, aes(x = i, y = poverty_rate * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("Child BHC Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)+
  plab(label = c(
    "Ineligible",
    "Eligible: Homeowner & Liquid",
    "Eligible: Homeowner Only",
    "Eligible: Liquid Only",
    "Eligible: Neither"
  ), y = c(95,89,83,77,71),x=rep(150,times=5))


Child_sole_BHC_poverty <- compute_results(Rep_rates_df_subset[Numb_dep > 0 & partnered == 0],income="BHC")

ggplot(Child_sole_BHC_poverty, aes(x = i, y = poverty_rate * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("Sole parent BHC Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)+
  plab(label = c(
    "Ineligible",
    "Eligible: Homeowner & Liquid",
    "Eligible: Homeowner Only",
    "Eligible: Liquid Only",
    "Eligible: Neither"
  ), y = c(95,89,83,77,71),x=rep(150,times=5))

Child_sole_AHC_poverty <- compute_results(Rep_rates_df_subset[Numb_dep > 0 & partnered == 0],income="AHC")

ggplot(Child_sole_AHC_poverty, aes(x = i, y = poverty_rate * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("Sole parent AHC Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)

NoChild_sole_BHC_poverty <- compute_results(Rep_rates_df_subset[Numb_dep == 0 & partnered == 0],income="BHC")

ggplot(NoChild_sole_BHC_poverty, aes(x = i, y = poverty_rate * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("Single no dep BHC Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)+
  plab(label = c(
    "Ineligible",
    "Eligible: Homeowner & Liquid",
    "Eligible: Homeowner Only",
    "Eligible: Liquid Only",
    "Eligible: Neither"
  ), y = c(95,89,83,77,71),x=rep(150,times=5))

NoChild_sole_AHC_poverty <- compute_results(Rep_rates_df_subset[Numb_dep == 0 & partnered == 0],income="AHC")

ggplot(NoChild_sole_AHC_poverty, aes(x = i, y = poverty_rate * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("Single no dep AHC Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)+
  plab(label = c(
    "Ineligible",
    "Eligible: Homeowner & Liquid",
    "Eligible: Homeowner Only",
    "Eligible: Liquid Only",
    "Eligible: Neither"
  ), y = c(95,89,83,77,71),x=rep(150,times=5))

# Check rent assistance

NoChild_sole_BHC_poverty_RAreceipt <- compute_results(Rep_rates_df_subset[Numb_dep == 0 & partnered == 0 & hours0_RA > 0],income="BHC")

ggplot(NoChild_sole_BHC_poverty_RAreceipt, aes(x = i, y = poverty_rate * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("Single no dep BHC Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)+
  plab(label = c(
    "Ineligible",
    "Eligible: Homeowner & Liquid",
    "Eligible: Homeowner Only",
    "Eligible: Liquid Only",
    "Eligible: Neither"
  ), y = c(95,89,83,77,71),x=rep(150,times=5))

NoChild_sole_BHC_poverty_noRAreceipt <- compute_results(Rep_rates_df_subset[Numb_dep == 0 & partnered == 0 & hours0_RA == 0],income="BHC")

ggplot(NoChild_sole_BHC_poverty_noRAreceipt, aes(x = i, y = poverty_rate * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("Single no dep BHC Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)+
  plab(label = c(
    "Ineligible",
    "Eligible: Homeowner & Liquid",
    "Eligible: Homeowner Only",
    "Eligible: Liquid Only",
    "Eligible: Neither"
  ), y = c(95,89,83,77,71),x=rep(150,times=5))


## Checking for the individuals with deep poverty

Rep_rates_df_subset[Numb_dep == 0 & partnered == 0 & actual_gap_BHC < -300 & group != "Ineligible"]

Rep_rates_df_subset[Numb_dep > 0 & partnered == 0 & actual_gap_BHC < -600 & group != "Ineligible"]

# For abstract

NoChild_sole_BHC_poverty_illiquid <- compute_results(Rep_rates_df_subset[Numb_dep == 0 & partnered == 0 & hours0_RA > 0 & group == "Eligible: Neither"],income="BHC",loop=300)

NoChild_sole_BHC_poverty_illiquid[poverty_rate >0.40]
NoChild_sole_BHC_poverty_illiquid[89:95]

Total_BHC_poverty_illiquid <- compute_results(Rep_rates_df_subset[group != "Ineligible"],income="BHC",loop=300)
Total_BHC_poverty_illiquid[i %in% seq(89,95,by=1)][,.(poverty = sum(poverty_rate)),by="i"]

BHC_poverty_illiquid_RA <- compute_results(Rep_rates_df_subset[hours0_RA > 0 & group == "Eligible: Neither"],income="BHC",loop=300)

BHC_poverty_illiquid_RA[i %in% seq(89,95,by=1)][,.(poverty = sum(poverty_rate)),by="i"]

AHC_poverty_illiquid_RA <- compute_results(Rep_rates_df_subset[hours0_RA > 0 & group == "Eligible: Neither"],income="AHC",loop=500)

target <- AHC_poverty_illiquid_RA[i==0]$poverty_rate - BHC_poverty_illiquid_RA[i==0]$poverty_rate + BHC_poverty_illiquid_RA[i==114]$poverty_rate

AHC_poverty_illiquid_RA[poverty_rate <= target]

91/389

114/389

328/389

### Construct distribution of replacement rates for submission as well.



