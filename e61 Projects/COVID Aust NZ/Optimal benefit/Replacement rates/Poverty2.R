## Last update:  12/03/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan 
# Focused code on alternative poverty estimates
# These were the graphs made at home - while I kept working on other parts at work. So there was a version error.

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

liquid_thresh = 5 # Only applied at the bottom for now - check to apply more widely

IR_house_single <- 0 # A random figure for "imputed rent" at household level
IR_house_couple <- 0 # A random figure for "imputed rent" at household level
IR <- 0 # A random figure for "imputed rent" at equivalised level

## Use the dataset with flags from Matthew M
#Rep_rates_df <- read_csv("C:/Users/MattNolan/Downloads/RRs_csv 3.csv") # Work version
Rep_rates_df <- read_csv("C:/Users/OEM/Downloads/RRs_csv 3.csv") # Home version


setDT(Rep_rates_df)

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

## Before subsetting, create AHC measures ----
# First generate the AHC incomes - by removing CRA, and subtracting housing costs. HCs are defined as rent or mortgage payments - so rates and insurance payments currently excluded.

Rep_rates_df[,Rent := fifelse(is.na(Rent),0,Rent)]

Rep_rates_df[,current_AHC := hhincome_pre - Rent - Weekly_Mortgage_Repayments]# - current_RA - current_RA_partner: rent assistance is in the income where appropriate, so already net of rent
Rep_rates_df[,hours0_AHC := hhincome - Rent - Weekly_Mortgage_Repayments]#  - hours0_RA - hours0_RA_partner: So changes in RA are implicitly captured in changes in the net rent paid here for the poverty line.

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

Rep_rates_df_subset[, normalized_weight_total := SIHPSWT / sum(SIHPSWT)] # Broadest normalised weights - construct other "by group" weightings when needed

colnames(Rep_rates_df_subset)


### Henderson Poverty lines ----

henderson_poverty_lines <- data.frame(
  Income_Unit = c(
    "Couple", "Couple plus 1", "Couple plus 2", "Couple plus 3", "Couple plus 4",
    "Single person", "Single parent plus 1", "Single parent plus 2",
    "Single parent plus 3", "Single parent plus 4"
  ),
  Including_Housing = c(
    819.31, 984.85, 1150.39, 1315.93, 1481.47,
    612.47, 786.29, 951.72, 1117.26, 1282.80
  ),
  Other_than_Housing = c(
    599.24, 744.88, 890.52, 1036.16, 1180.18,
    412.18, 566.11, 711.75, 857.38, 1003.02
  )
)
## Adjust below to non-work line from MI: https://melbourneinstitute.unimelb.edu.au/__data/assets/pdf_file/0006/4961229/Poverty-Lines-Australia-March-Quarter-2024.pdf. Generally only used for those who are retired.
henderson_poverty_lines_nonwork <- data.frame(
  Income_Unit = c(
    "Couple", "Couple plus 1", "Couple plus 2", "Couple plus 3", "Couple plus 4",
    "Single person", "Single parent plus 1", "Single parent plus 2",
    "Single parent plus 3", "Single parent plus 4"
  ),
  Including_Housing_nonwork = c(
    703.46, 869.00, 1034.54, 1200.09, 1365.63,
    496.62, 670.33, 835.87, 1001.41, 1166.95
  ),
  Other_than_Housing_nonwork = c(
    483.28, 629.03, 774.67, 920.31, 1064.34,
    296.34, 450.26, 595.90, 741.54, 887.18
  )
)
setDT(henderson_poverty_lines)


liquid_assets_thresholds <- c(-Inf, 1, 20, 40, 60, Inf)

Rep_rates_df_subset <- Rep_rates_df_subset %>%
  mutate(
    Income_Unit = case_when(
      partnered == 1 & Numb_dep == 0 ~ "Couple",
      partnered == 1 & Numb_dep == 1 ~ "Couple plus 1",
      partnered == 1 & Numb_dep == 2 ~ "Couple plus 2",
      partnered == 1 & Numb_dep == 3 ~ "Couple plus 3",
      partnered == 1 & Numb_dep == 4 ~ "Couple plus 4",
      partnered == 0 & Numb_dep == 0 ~ "Single person",
      partnered == 0 & Numb_dep == 1 ~ "Single parent plus 1",
      partnered == 0 & Numb_dep == 2 ~ "Single parent plus 2",
      partnered == 0 & Numb_dep == 3 ~ "Single parent plus 3",
      partnered == 0 & Numb_dep == 4 ~ "Single parent plus 4",
      partnered == 0 & Numb_dep == 5 ~ "Single parent plus 4",
      TRUE ~ NA_character_  # Handle cases beyond 4 dependents or invalid values
    )
  )

# Join with chosen poverty line
Rep_rates_df_subset <- henderson_poverty_lines[Rep_rates_df_subset,on=.(Income_Unit)]



#Rep_rates_df_subset[,poverty_gap := fifelse(Home_owner == 1,hhincome - min(Weekly_Mortgage_Repayments*0.5, (Including_Housing - Other_than_Housing)) - Other_than_Housing,hhincome - Including_Housing)]

Rep_rates_df_subset[,henderson_BHC := fifelse(Home_owner == 1,
                                              fifelse(partnered == 1, hhincome - Including_Housing + IR_house_couple,
                                                      hhincome - Including_Housing + IR_house_single),hhincome - Including_Housing)] # Negative means below the poverty line

Rep_rates_df_subset[, liquid_assets_category := cut(weeks_of_liquid_assets,
                                                    breaks = liquid_assets_thresholds,
                                                    include.lowest = TRUE,
                                                    labels = c("<1", "1-20", "21-40", "40-60", "61+"))]



# Normalize weights within each category of weeks_of_liquid_assets
Rep_rates_df_subset[, normalized_weight_hhld_la := hhld_wgt / sum(hhld_wgt), by = .(liquid_assets_category)]
Rep_rates_df_subset[, normalized_weight_eqind_la := hhld_wgt*hhld_size / sum(hhld_wgt*hhld_size), by = .(liquid_assets_category)]

nrow(Rep_rates_df_subset)

subset <- Rep_rates_df_subset[!is.na(henderson_BHC)]

nrow(Rep_rates_df_subset)

# Plot the distribution
ggplot(subset, aes(x = henderson_BHC, weight = normalized_weight_eqind_la, col = as.factor(liquid_assets_category))) +
  geom_density(alpha = 0.1) +  
  labs(
    title = "Distribution of Poverty Gap by Weeks of Liquid Assets",
    x = "Poverty Gap",
    y = "Weighted Count"
  ) + xlim(-1500,1500) + theme_e61(legend = "bottom")


### Median income Poverty lines ----

## Calculate equivalised income - using modified OECD scale

colnames(Rep_rates_df_subset)

# This gives INDIVIDUAL equivalised income at the HOUSEHOLD level for household weights.  So if we want to talk about household level poverty we would need to multiply this by the number of household members.  If we want to do individuals in poverty then we keep this measure but our weight is the household weight multiplied by the number of individuals in the household (1 + partnered + Numb_dep).

## Individual level.
# Calculate the weighted cut-offs in our data

#Rep_rates_df_subset[, weighted_median_50 := wtd.quantile(eq_hhinc, weights = hhld_size * hhld_wgt, probs = 0.5, na.rm = TRUE)] # This is the distribution of post-income - so this will be wrong, it just gives us the distribution around the median of our sample
#Rep_rates_df_subset[, weighted_median_60 := wtd.quantile(eq_hhinc, weights = hhld_size * hhld_wgt, probs = 0.6, na.rm = TRUE)] # This is the distribution of post-income - so this will be wrong, it just gives us the distribution around the median of our sample

Rep_rates_df_subset[,wm_50_BHC := fifelse(Home_owner == 1,eq_hhinc - weighted_median_50 + IR,eq_hhinc - weighted_median_50)]
Rep_rates_df_subset[,wm_60_BHC := fifelse(Home_owner == 1,eq_hhinc - weighted_median_60 + IR,eq_hhinc - weighted_median_60)]
Rep_rates_df_subset[,pre_wm_50_BHC := fifelse(Home_owner == 1,eq_hhinc_pre - weighted_median_50 + IR,eq_hhinc_pre - weighted_median_50)]
Rep_rates_df_subset[,pre_wm_60_BHC := fifelse(Home_owner == 1,eq_hhinc_pre - weighted_median_60 + IR,eq_hhinc_pre - weighted_median_60)]

ggplot(Rep_rates_df_subset, aes(x = wm_50_BHC, weight = hhld_size * hhld_wgt)) +
  geom_density(alpha = 0.1) +  
  labs(
    title = "Distribution of Poverty Gap by Weeks of Liquid Assets",
    x = "Poverty Gap",
    y = "Weighted Count"
  ) + xlim(-1500,1500) + theme_e61(legend = "bottom")

ggplot(Rep_rates_df_subset, aes(x = wm_60_BHC, weight = hhld_size * hhld_wgt)) +
  geom_density(alpha = 0.1) +  
  labs(
    title = "Distribution of Poverty Gap by Weeks of Liquid Assets",
    x = "Poverty Gap",
    y = "Weighted Count"
  ) + xlim(-1500,1500) + theme_e61(legend = "bottom")

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

# Now look at the distribution by liquid asset group
Rep_rates_df_subset[, normalized_weight_eqind_la := hhld_wgt*hhld_size / sum(hhld_wgt*hhld_size), by = .(liquid_assets_category)]

ggplot(Rep_rates_df_subset, aes(x = wm_50_BHC, weight = normalized_weight_eqind_la,colour = liquid_assets_category)) +
  geom_density(alpha = 0.1) +  
  labs(
    title = "Distribution of Poverty Gap by Weeks of Liquid Assets",
    subtitle = "Median income (50%) line, equivalised individual income",
    x = "Poverty Gap",
    y = "Weighted Count"
  ) + xlim(-1500,1500) + theme_e61(legend = "bottom") + geom_vline(xintercept = 0,linetype = "dashed")

# Now look at the distribution by family type


#Rep_rates_df_subset[, normalized_weight_eqind_fam := hhld_wgt*hhld_size / sum(hhld_wgt*hhld_size), by = .(Income_Unit)]
Rep_rates_df_subset[, normalized_weight_eqind_fam := hhld_wgt*hhld_size / sum(hhld_wgt*hhld_size), by = .(IU_agg)]

ggplot(Rep_rates_df_subset, aes(x = wm_50_BHC, weight = normalized_weight_eqind_fam,colour = IU_agg)) +
  geom_density(alpha = 0.1) +  
  labs(
    title = "Distribution of Poverty Gap by Family Type",
    subtitle = "Median income (50%) line, equivalised individual income",
    x = "Poverty Gap",
    y = "Weighted Count"
  ) + xlim(-1500,1500) + theme_e61(legend = "bottom") + geom_vline(xintercept = 0,linetype = "dashed")

ggplot(Rep_rates_df_subset, aes(x = pre_wm_50_BHC, weight = normalized_weight_eqind_fam,colour = IU_agg)) +
  geom_density(alpha = 0.1) +  
  labs(
    title = "Distribution of Poverty Gap for employed by Family Type",
    subtitle = "Median income (50%) line, equivalised individual income",
    x = "Poverty Gap",
    y = "Weighted Count"
  ) + xlim(-1500,1500) + theme_e61(legend = "bottom") + geom_vline(xintercept = 0,linetype = "dashed")


# Now by eligibility
Rep_rates_df_subset[, normalized_weight_eqind_elig := hhld_wgt*hhld_size / sum(hhld_wgt*hhld_size), by = .(eligibility_status)]

ggplot(Rep_rates_df_subset, aes(x = wm_50_BHC, weight = normalized_weight_eqind_elig,colour = eligibility_status)) +
  geom_density(alpha = 0.1) +  
  labs(
    title = "Distribution of Poverty Gap by Family Type",
    subtitle = "Median income (50%) line, equivalised individual income",
    x = "Poverty Gap",
    y = "Weighted Count"
  ) + xlim(-1500,1500) + theme_e61(legend = "bottom") + geom_vline(xintercept = 0,linetype = "dashed")


### Budget standards (June 2016 value) [then gross up with beneficiary CPI from ABS]

single_amt <- 433.68
single_child_amt <- 675.18 - 433.68
couple_amt <- 660.25
couple_child_amt <- (940.37 - 660.25)/2 # Averaged the two child amounts, and will just add this per child - as it isn't clear why the child cost increases for the second child in the piece.

single_amt + single_child_amt

couple_amt + couple_child_amt

# Q4 2024 estimates from EIAC. Use core + renting line at this stage - the discretionary spending and purchase amounts are not of interest, and as we don't differentiate owners and renters in other lines we will leave as is here (following standard OECD lines). Also don't define by gender at this stage.

single_amt_24 <- 657
single_child_amt_24 <- 1137.5 - 958
couple_amt_24 <- 839
couple_child_amt_24 <- (1341 - 1164)/2

single_amt_24 + single_child_amt_24

couple_amt_24 + couple_child_amt_24


# Absolute line basis
ben_inflate <- 139.2/108 # From https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/selected-living-cost-indexes-australia/latest-release#data-downloads

# Relative line basis
ben_inflate_relative <- 818.92/704.67 # Use 2016 v 2024 Henderson lines for their inflation [not this is the couple not child only]

612.18 /526.77 # Single
785.92/676.27 # Single 1 kid.  Looks like the line increases proportionally.

Rep_rates_df_subset[,BS_line := fifelse(partnered == 0,(single_amt + single_child_amt*Numb_dep)*ben_inflate,
                                        (couple_amt + couple_child_amt*Numb_dep)*ben_inflate)]

Rep_rates_df_subset[,BS_line_rel := fifelse(partnered == 0,(single_amt + single_child_amt*Numb_dep)*ben_inflate_relative,
                                            (couple_amt + couple_child_amt*Numb_dep)*ben_inflate_relative)]

Rep_rates_df_subset[,BS_line_24 := fifelse(partnered == 0,(single_amt_24 + single_child_amt_24*Numb_dep),
                                           (couple_amt_24 + couple_child_amt_24*Numb_dep))]


Rep_rates_df_subset[IU_agg == "Single + Dep"]

Rep_rates_df_subset[,BS_BHC := fifelse(Home_owner == 1,
                                       fifelse(partnered == 1,hhincome - BS_line + IR_house_couple,
                                               hhincome - BS_line + IR_house_single)
                                       ,hhincome -BS_line)] # Negative means below the poverty line
Rep_rates_df_subset[,BS_BHC_rel := fifelse(Home_owner == 1,
                                           fifelse(partnered == 1,hhincome - BS_line_rel + IR_house_couple,
                                                   hhincome - BS_line_rel + IR_house_single)
                                           ,hhincome -BS_line_rel)]
Rep_rates_df_subset[,BS_BHC_24 := fifelse(Home_owner == 1,
                                          fifelse(partnered == 1,hhincome - BS_line_24 + IR_house_couple,
                                                  hhincome - BS_line_24 + IR_house_single)
                                          ,hhincome -BS_line_24)]

ggplot(Rep_rates_df_subset, aes(x = BS_BHC, weight = normalized_weight_eqind_fam,colour = IU_agg)) +
  geom_density(alpha = 0.1) +  
  labs(
    title = "Distribution of Poverty Gap for employed by Family Type",
    subtitle = "Budget Standard line, hhld income individual level",
    x = "Poverty Gap",
    y = "Weighted Count"
  ) + xlim(-1500,1500) + theme_e61(legend = "bottom") + geom_vline(xintercept = 0,linetype = "dashed")

single_child_amt/(single_amt)

melt(Rep_rates_df_subset[,.(Including_Housing,BS_line,id = seq(1:nrow(Rep_rates_df_subset)))],id.vars = "id")

ggplot(melt(Rep_rates_df_subset[,.(Including_Housing,BS_line,id = seq(1:nrow(Rep_rates_df_subset)))],id.vars = "id"),aes(y=value,x=id,colour=variable)) + geom_point()

Rep_rates_df_subset[,diff_HL_BSL := Including_Housing - BS_line]
Rep_rates_df_subset[Numb_dep == 1,.(hhld_size,Numb_dep,diff_HL_BSL,BS_line,Including_Housing,Income_Unit)][order(diff_HL_BSL)]

ggplot(Rep_rates_df_subset[,.(diff = Including_Housing - BS_line,id = seq(1:nrow(Rep_rates_df_subset)),IU_agg)],aes(y=diff,x=id,colour=IU_agg)) +geom_point() +
  scale_y_continuous_e61(limits = c(-1500,max(Rep_rates_df_subset$diff_HL_BSL),500)) +
  theme_e61(legend = "bottom") + add_baseline()

ggplot(Rep_rates_df_subset, aes(x = BS_BHC, weight = normalized_weight_eqind_fam,colour = IU_agg)) +
  geom_density(alpha = 0.1) +  
  labs(
    title = "Distribution of Poverty Gap for employed by Family Type",
    subtitle = "Budget Standard line, household income (ind weighting)",
    x = "Poverty Gap",
    y = "Weighted Count"
  ) + xlim(-1500,1500) + theme_e61(legend = "bottom") + geom_vline(xintercept = 0,linetype = "dashed")

group_count <- Rep_rates_df_subset[,.(total = sum(hhld_wgt*hhld_size)),by=.(IU_agg)]
hend_count <- Rep_rates_df_subset[henderson_BHC <= 0,.(N = sum(hhld_wgt*hhld_size)),by=.(IU_agg)]
med_count <- Rep_rates_df_subset[wm_50_BHC <= 0,.(N = sum(hhld_wgt*hhld_size)),by=.(IU_agg)]
BS_count <- Rep_rates_df_subset[BS_BHC <= 0,.(N = sum(hhld_wgt*hhld_size)),by=.(IU_agg)]
BS_rel_count <- Rep_rates_df_subset[BS_BHC_rel <= 0,.(N = sum(hhld_wgt*hhld_size)),by=.(IU_agg)]
BS_24_count <- Rep_rates_df_subset[BS_BHC_24 <= 0,.(N = sum(hhld_wgt*hhld_size)),by=.(IU_agg)]

hend_count <- group_count[hend_count,on=.(IU_agg)][,PR := N/total][order(IU_agg)]
med_count <- group_count[med_count,on=.(IU_agg)][,PR := N/total][order(IU_agg)]
BS_count <- group_count[BS_count,on=.(IU_agg)][,PR := N/total][order(IU_agg)]
BS_rel_count <- group_count[BS_rel_count,on=.(IU_agg)][,PR := N/total][order(IU_agg)]
BS_24_count <- group_count[BS_24_count,on=.(IU_agg)][,PR := N/total][order(IU_agg)]

PR <- melt(data.table(cat = hend_count$IU_agg, hend = hend_count$PR, med = med_count$PR, BS = BS_count$PR,BS_rel = BS_rel_count$PR,BS_24 = BS_24_count$PR),id.vars = "cat")

ggplot(PR,aes(x=cat,y=value,fill=variable)) + geom_col(position = "dodge") +
  theme_e61(legend = "bottom") +
  labs_e61("Job loss Poverty Rates via different lines",y="") +
  scale_y_continuous_e61(limits = c(0,1,0.2))

unique(Rep_rates_df_subset[,.(IU_agg,Including_Housing,weighted_median_50,BS_line,Numb_dep)])

ggplot(melt(unique(Rep_rates_df_subset[Numb_dep < 2,.(IU_agg,Henderson = Including_Housing,Med = weighted_median_50*eq_scale,BS_2016 = BS_line,BS_rel_2016 = BS_line_rel,BS_2024 = BS_line_24)]),id.var = "IU_agg"),aes(x=IU_agg,y=value,fill=variable)) + geom_col(position = "dodge") + theme_e61(legend = "bottom") +
  labs(title = "Household poverty level ($weekly)",subtitle = "Dependents = 1 child",y="") +
  scale_y_continuous_e61(limits = c(0,1200,200))

Rep_rates_df_subset$eq_scale

Rep_rates_df_subset[IU_agg == "Single + Dep"]

### Now lets pull some information about the individuals in poverty
# We are going to focus on two - median income and budget standards. Key distinction is that median income gives the highest required income for couples, while budget standards has the highest standard for single individuals.

## Budget standard description (BS_line_24, BS_BHC_24)

summary_BS <- Rep_rates_df_subset[,.(Home_owner,liquid = weeks_of_liquid_assets > liquid_thresh,net_RR,poverty_BS = BS_BHC_24 <= 0,eligible = eligibility_status != "Ineligible",IU_agg,weight = hhld_wgt*hhld_size)]

ggplot(summary_BS,aes(x=net_RR,y=poverty_BS,colour = IU_agg)) + geom_point() + theme_e61(legend = "bottom")

RR_pov_BS <- summary_BS[, .(
  Poverty_Proportion = weighted.mean(poverty_BS, weight),
  net_RR_Ineligible = weighted.mean(net_RR, weight)), by = .(IU_agg)]

ggplot(melt(RR_pov_BS,id.vars = "IU_agg"),aes(x=IU_agg,y=value,fill=variable)) + geom_col(position = "dodge")

ggplot(summary_BS, aes(x = IU_agg, y = net_RR, fill = IU_agg)) + 
  geom_boxplot(outlier.shape = NA) +  # Hides extreme outliers for better visibility
  geom_jitter(aes(color = IU_agg), width = 0.2, alpha = 0.3) +  # Adds scatter for individual points
  labs(title = "Distribution of net_RR by IU_agg",
       x = "IU_agg",
       y = "Net RR") +
  theme_e61(legend = "bottom")


# More analysis

summary_BS[, .(
  Poverty_Proportion = weighted.mean(poverty_BS, weight),
  Home_Owner_Proportion_In_Poverty = weighted.mean(Home_owner, weight, na.rm = TRUE),
  Liquid_Proportion_In_Poverty = weighted.mean(liquid, weight, na.rm = TRUE),
  Home_Owner_And_Liquid_Proportion_In_Poverty = weighted.mean(Home_owner & liquid, weight, na.rm = TRUE),
  Ineligible_Proportion_In_Poverty = weighted.mean(!eligible, weight, na.rm = TRUE)
), by = .(IU_agg)]

poverty_summary_BS <- summary_BS[, .(
  Total_Poverty_Proportion = weighted.mean(poverty_BS, weight), # Total poverty proportion in the population
  Ineligible_Poverty = weighted.mean(!eligible & poverty_BS, weight),
  Eligible_HomeOwner_Liquid = weighted.mean(eligible & Home_owner & liquid & poverty_BS, weight),
  Eligible_HomeOwner_NotLiquid = weighted.mean(eligible & Home_owner & !liquid & poverty_BS, weight),
  Eligible_NotHomeOwner_Liquid = weighted.mean(eligible & !Home_owner & liquid & poverty_BS, weight),
  Eligible_NotHomeOwner_NotLiquid = weighted.mean(eligible & !Home_owner & !liquid & poverty_BS, weight)
), by = .(IU_agg)]

poverty_long_BS <- melt(poverty_summary_BS, 
                        id.vars = c("IU_agg", "Total_Poverty_Proportion"), 
                        variable.name = "Category", 
                        value.name = "Proportion")

ggplot(poverty_long_BS, aes(x = IU_agg, y = Proportion*100, fill = Category)) +
  geom_bar(stat = "identity") +
  #geom_hline(aes(yintercept = Total_Poverty_Proportion), linetype = "dashed", color = "black") +
  labs_e61(title = "Composition of Poverty by Family Type - Budget Standard line",
           x = "",
           y = "%",
           fill = "Poverty Category") + coord_flip() +
  scale_y_continuous_e61(limits = c(0,100,25)) +
  #theme_e61(legend = "bottom") + 
  format_flip() +
  plab(label = c("Ineligible","House & Liquid","Home Owner","Liquid renter","Illiquid renter"),x = c(2,1.5,1,2,1.5),y= c(30,30,30,55,55),colour = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))

net_RR_summary_BS <- summary_BS[, .(
  net_RR_Ineligible = weighted.mean(net_RR[!eligible & poverty_BS], weight[!eligible & poverty_BS], na.rm = TRUE),
  net_RR_HomeOwner_Liquid = weighted.mean(net_RR[eligible & Home_owner & liquid & poverty_BS], 
                                          weight[eligible & Home_owner & liquid & poverty_BS], na.rm = TRUE),
  net_RR_HomeOwner_NotLiquid = weighted.mean(net_RR[eligible & Home_owner & !liquid & poverty_BS], 
                                             weight[eligible & Home_owner & !liquid & poverty_BS], na.rm = TRUE),
  net_RR_NotHomeOwner_Liquid = weighted.mean(net_RR[eligible & !Home_owner & liquid & poverty_BS], 
                                             weight[eligible & !Home_owner & liquid & poverty_BS], na.rm = TRUE),
  net_RR_NotHomeOwner_NotLiquid = weighted.mean(net_RR[eligible & !Home_owner & !liquid & poverty_BS], 
                                                weight[eligible & !Home_owner & !liquid & poverty_BS], na.rm = TRUE),
  net_RR_NotInPoverty = weighted.mean(net_RR[poverty_BS == FALSE], weight[poverty_BS == FALSE], na.rm = TRUE)
), by = .(IU_agg)]

net_RR_summary_BS[,net_RR_Ineligible := NULL] # Delete after checking this is zero

net_RR_long_BS <- melt(net_RR_summary_BS, 
                       id.vars = c("IU_agg"), 
                       variable.name = "Category", 
                       value.name = "Proportion")

ggplot(net_RR_long_BS,aes(x=IU_agg,y=Proportion,fill=Category)) + geom_col(position = "dodge") +
  theme_e61(legend = "bottom") + coord_flip() +
  scale_y_continuous_e61(limits=c(0,0.8,0.2))

## Median income line (BS_line_24, BS_BHC_24)

summary_med <- Rep_rates_df_subset[,.(Home_owner,liquid = weeks_of_liquid_assets > liquid_thresh,net_RR,poverty_med = wm_50_BHC <= 0,eligible = eligibility_status != "Ineligible",IU_agg,weight = hhld_wgt*hhld_size)]

summary_med[, .(
  Poverty_Proportion = weighted.mean(poverty_med, weight),
  Home_Owner_Proportion_In_Poverty = weighted.mean(Home_owner, weight, na.rm = TRUE),
  Liquid_Proportion_In_Poverty = weighted.mean(liquid, weight, na.rm = TRUE),
  Home_Owner_And_Liquid_Proportion_In_Poverty = weighted.mean(Home_owner & liquid, weight, na.rm = TRUE),
  Ineligible_Proportion_In_Poverty = weighted.mean(!eligible, weight, na.rm = TRUE)
), by = .(IU_agg)]

poverty_summary_med <- summary_med[, .(
  Total_Poverty_Proportion = weighted.mean(poverty_med, weight), # Total poverty proportion in the population
  Ineligible_Poverty = weighted.mean(!eligible & poverty_med, weight),
  Eligible_HomeOwner_Liquid = weighted.mean(eligible & Home_owner & liquid & poverty_med, weight),
  Eligible_HomeOwner_NotLiquid = weighted.mean(eligible & Home_owner & !liquid & poverty_med, weight),
  Eligible_NotHomeOwner_Liquid = weighted.mean(eligible & !Home_owner & liquid & poverty_med, weight),
  Eligible_NotHomeOwner_NotLiquid = weighted.mean(eligible & !Home_owner & !liquid & poverty_med, weight)
), by = .(IU_agg)]

poverty_long_med <- melt(poverty_summary_med, 
                         id.vars = c("IU_agg", "Total_Poverty_Proportion"), 
                         variable.name = "Category", 
                         value.name = "Proportion")

ggplot(poverty_long_med, aes(x = IU_agg, y = Proportion*100, fill = Category)) +
  geom_bar(stat = "identity") +
  #geom_hline(aes(yintercept = Total_Poverty_Proportion), linetype = "dashed", color = "black") +
  labs_e61(title = "Composition of Poverty by Family Type - Median Income line",
           subtitle = "Before Housing Costs",
           x = "",
           y = "%",
           fill = "Poverty Category") + coord_flip() +
  scale_y_continuous_e61(limits = c(0,100,25)) +
  #theme_e61(legend = "bottom") + 
  format_flip() +
  plab(label = c("Ineligible","House & Liquid","Home Owner","Liquid renter","Illiquid renter"),x = c(2,1.7,1.4,1.1,0.8),y= c(55,55,55,55,55),colour = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))

save_e61("Med_poverty_BHC.pdf",pad_width = 1)


ggplot(poverty_long_med, aes(x = IU_agg, y = Proportion*100, fill = Category)) +
  geom_bar(stat = "identity") +
  #geom_hline(aes(yintercept = Total_Poverty_Proportion), linetype = "dashed", color = "black") +
  labs_e61(title = "Composition of Poverty by Family Type - Median Income line",
           subtitle = "Before Housing Costs",
           x = "",
           y = "%",
           fill = "Poverty Category") + coord_flip() +
  scale_y_continuous_e61(limits = c(0,100,25)) +
  #theme_e61(legend = "bottom") + 
  format_flip() +
  plab(label = c("Ineligible","House & Liquid","Home Owner","Liquid renter","Illiquid renter"),x = c(2,1.7,1.4,1.1,0.8),y= c(55,55,55,55,55),colour = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))

save_e61("Med_poverty_BHC.png",pad_width = 1,res=2)


###### Create AHC rates ----
# Just need to complete generating AHC measures above.

Rep_rates_df_subset[,wm_50_AHC := eq_hours0_AHC - AHC_weighted_median_50]
Rep_rates_df_subset[,pre_wm_50_AHC := eq_current_AHC - AHC_weighted_median_50]
# 
# ggplot(Rep_rates_df_subset, aes(x = wm_50_AHC, weight = hhld_size * hhld_wgt)) +
#   geom_density(alpha = 0.1) +  
#   labs(
#     title = "Distribution of Poverty Gap (AHC)",
#     x = "Poverty Gap",
#     y = "Weighted Count"
#   ) + xlim(-1500,1500) + theme_e61(legend = "bottom")


#BHC poverty
Rep_rates_df_subset[, {
  w <- hhld_size * hhld_wgt  # Individual weights
  prop_weighted <- sum(w * (wm_50_BHC < 0), na.rm = TRUE) / sum(w, na.rm = TRUE)
  .(weighted_proportion = prop_weighted)
}]

Rep_rates_df_subset[, {
  w <- hhld_size * hhld_wgt  # Individual weights
  prop_weighted <- sum(w * (pre_wm_50_BHC < 0), na.rm = TRUE) / sum(w, na.rm = TRUE)
  .(weighted_proportion = prop_weighted)
}]


#AHC poverty
Rep_rates_df_subset[, {
  w <- hhld_size * hhld_wgt  # Individual weights
  prop_weighted <- sum(w * (wm_50_AHC < 0), na.rm = TRUE) / sum(w, na.rm = TRUE)
  .(weighted_proportion = prop_weighted)
}]

Rep_rates_df_subset[, {
  w <- hhld_size * hhld_wgt  # Individual weights
  prop_weighted <- sum(w * (pre_wm_50_AHC < 0), na.rm = TRUE) / sum(w, na.rm = TRUE)
  .(weighted_proportion = prop_weighted)
}]


# ggplot(Rep_rates_df_subset, aes(x = wm_50_AHC, weight = normalized_weight_eqind_fam,colour = IU_agg)) +
#   geom_density(alpha = 0.1) +  
#   labs(
#     title = "Distribution of Poverty Gap by Family Type (AHC)",
#     subtitle = "Median income (50%) line, equivalised individual income",
#     x = "Poverty Gap",
#     y = "Weighted Count"
#   ) + xlim(-1500,1500) + theme_e61(legend = "bottom") + geom_vline(xintercept = 0,linetype = "dashed")
# 
# ggplot(Rep_rates_df_subset, aes(x = pre_wm_50_AHC, weight = normalized_weight_eqind_fam,colour = IU_agg)) +
#   geom_density(alpha = 0.1) +  
#   labs(
#     title = "Distribution of Poverty Gap for employed by Family Type (AHC)",
#     subtitle = "Median income (50%) line, equivalised individual income",
#     x = "Poverty Gap",
#     y = "Weighted Count"
#   ) + xlim(-1500,1500) + theme_e61(legend = "bottom") + geom_vline(xintercept = 0,linetype = "dashed")

# The AHC medians
summary_med_AHC <- Rep_rates_df_subset[,.(Home_owner,liquid = weeks_of_liquid_assets > liquid_thresh,net_RR,poverty_med = wm_50_AHC <= 0,eligible = eligibility_status != "Ineligible",IU_agg,weight = hhld_wgt*hhld_size)]

summary_med_AHC[, .(
  Poverty_Proportion = weighted.mean(poverty_med, weight),
  Home_Owner_Proportion_In_Poverty = weighted.mean(Home_owner, weight, na.rm = TRUE),
  Liquid_Proportion_In_Poverty = weighted.mean(liquid, weight, na.rm = TRUE),
  Home_Owner_And_Liquid_Proportion_In_Poverty = weighted.mean(Home_owner & liquid, weight, na.rm = TRUE),
  Ineligible_Proportion_In_Poverty = weighted.mean(!eligible, weight, na.rm = TRUE)
), by = .(IU_agg)]

poverty_summary_med_AHC <- summary_med_AHC[, .(
  Total_Poverty_Proportion = weighted.mean(poverty_med, weight), # Total poverty proportion in the population
  Ineligible_Poverty = weighted.mean(!eligible & poverty_med, weight),
  Eligible_HomeOwner_Liquid = weighted.mean(eligible & Home_owner & liquid & poverty_med, weight),
  Eligible_HomeOwner_NotLiquid = weighted.mean(eligible & Home_owner & !liquid & poverty_med, weight),
  Eligible_NotHomeOwner_Liquid = weighted.mean(eligible & !Home_owner & liquid & poverty_med, weight),
  Eligible_NotHomeOwner_NotLiquid = weighted.mean(eligible & !Home_owner & !liquid & poverty_med, weight)
), by = .(IU_agg)]

poverty_long_med_AHC <- melt(poverty_summary_med_AHC, 
                             id.vars = c("IU_agg", "Total_Poverty_Proportion"), 
                             variable.name = "Category", 
                             value.name = "Proportion")

ggplot(poverty_long_med_AHC, aes(x = IU_agg, y = Proportion*100, fill = Category)) +
  geom_bar(stat = "identity") +
  #geom_hline(aes(yintercept = Total_Poverty_Proportion), linetype = "dashed", color = "black") +
  labs_e61(#title = "Composition of Poverty by Family Type - Median Income line (AHC)",
    subtitle = "After Housing Costs",
    x = "",
    y = "%",
    fill = "Poverty Category") + coord_flip() +
  scale_y_continuous_e61(limits = c(0,100,25)) +
  #theme_e61(legend = "bottom") + 
  format_flip() +
  plab(label = c("Ineligible","House & Liquid","Home Owner","Liquid renter","Illiquid renter"),x = c(2,1.7,1.4,1.1,0.8),y= c(58,58,58,58,58),colour = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))

save_e61("Med_poverty_AHC.pdf",pad_width = 1)

ggplot(poverty_long_med_AHC, aes(x = IU_agg, y = Proportion*100, fill = Category)) +
  geom_bar(stat = "identity") +
  #geom_hline(aes(yintercept = Total_Poverty_Proportion), linetype = "dashed", color = "black") +
  labs_e61(title = "Composition of Poverty by Family Type - Median Income line (AHC)",
           subtitle = "After Housing Costs",
           x = "",
           y = "%",
           fill = "Poverty Category") + coord_flip() +
  scale_y_continuous_e61(limits = c(0,100,25)) +
  #theme_e61(legend = "bottom") + 
  format_flip() +
  plab(label = c("Ineligible","House & Liquid","Home Owner","Liquid renter","Illiquid renter"),x = c(2,1.7,1.4,1.1,0.8),y= c(58,58,58,58,58),colour = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))

save_e61("Med_poverty_AHC.png",pad_width = 1,res=2)
