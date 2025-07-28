## Last update:  17/06/2025
## Last update person:  Matt Nolan 
# Making the alternative liquidity scenarios in a separate script

library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(tidyverse)
library(data.table)
library(readxl)
library(Hmisc)
library(gganimate)

work_home <- "work"

if (work_home == "work"){
  Rep_rates_df <- read_csv("C:/Users/MattNolan/Downloads/RRs_csv 3.csv") # Work version original
} else {
  Rep_rates_df <- read_csv("C:/Users/OEM/Downloads/RRs_csv 3.csv") # Home version original
}


filter_group <- "JSP" # Three potential groups:  JSP, pos_RR, and ALL.  JSP is only those with taxable benefits, pos_RR is all individuals with a positive replacement rate, ALL is everyone including zeros.

setDT(Rep_rates_df)

hour_limit <- 30

liquid_type <- "hhld" # Two option: "ind" and "hhld" depending on the threshold we want to use (in terms of income and liquid assets). For replacement rates ind makes the most sense, for poverty it is hhld.

#liquid_thresh <- 13

### Data cleaning ----

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

Rep_rates_df$hours0_net_income <- ifelse(is.na(Rep_rates_df$hours0_net_income), 
                                         Rep_rates_df$hours0_gross_income + Rep_rates_df$hours0_net_fam_a + 
                                           Rep_rates_df$hours0_net_fam_b + Rep_rates_df$hours0_RA + 
                                           Rep_rates_df$hours0_ES - Rep_rates_df$hours0_IT, 
                                         Rep_rates_df$hours0_net_income)


#############################################################################################3

# Generate household weights based on whether people show up.

Rep_rates_df[,hhld_wgt := fifelse(partnered == 1 & current_work_income_partner > 20*hour_limit,SIHPSWT/2,SIHPSWT)]

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

Rep_rates_df[,IU_agg := fifelse(partnered == 1 & Numb_dep == 0,"Couple",
                                fifelse(partnered == 1 & Numb_dep > 0, "Couple + Dep",
                                        fifelse(partnered == 0 & Numb_dep == 0, "Single", "Single + Dep")))]


###########################################################################################3

### Clean the Data - This should already be done for some of this in the tax Calc stuff

Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$AGEEC > 21)

Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$AGEEC < 55)

Rep_rates_df <- subset(Rep_rates_df, wage > 15)


# Drop those with Negative incomes 

Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$current_net_income >= 0)
Rep_rates_df <- subset(Rep_rates_df , Rep_rates_df$hours0_net_income >= 0)
Rep_rates_df <- subset(Rep_rates_df , Rep_rates_df$NonWageIncome >= 0)
Rep_rates_df[is.na(current_wk_partner_earnings), current_net_income_partner := 0] # Only replacing for those without partner earnings with partner - so may be some still there.
Rep_rates_df[is.na(current_wk_partner_earnings), current_wk_partner_earnings := 0]

# Construct earnings measures 

Rep_rates_df$hours0_wk_partner_earnings <- ifelse(is.na(Rep_rates_df$hours0_wk_partner_earnings), Rep_rates_df$hours0_gross_income_partner - Rep_rates_df$hours0_income_tax_partner + Rep_rates_df$hours0_RA_partner + Rep_rates_df$hours0_ES_partner, Rep_rates_df$hours0_wk_partner_earnings )

Rep_rates_df$hhincome <- Rep_rates_df$hours0_net_income + Rep_rates_df$NonWageIncome + Rep_rates_df$hours0_wk_partner_earnings

Rep_rates_df$hhincome <- ifelse(Rep_rates_df$hhincome < 0, 0, Rep_rates_df$hhincome)

Rep_rates_df[,":=" (eq_scale = 1 + 0.5*partnered + 0.3*Numb_dep)][,eq_hhinc := hhincome/eq_scale][,hhld_size := 1 + partnered + Numb_dep]

Rep_rates_df[,hhincome_pre := current_net_income + NonWageIncome + current_wk_partner_earnings] 

Rep_rates_df$hhincome_pre <- ifelse(Rep_rates_df$hhincome_pre < 0, 0, Rep_rates_df$hhincome_pre)

Rep_rates_df[,eq_hhinc_pre := hhincome_pre/eq_scale]
Rep_rates_df[,eq_hours0 := hhincome/eq_scale]

Rep_rates_df[, weighted_median_50 := wtd.quantile(eq_hhinc_pre, weights = hhld_size * hhld_wgt, probs = 0.5, na.rm = TRUE)*0.5] 
Rep_rates_df[, weighted_median_60 := wtd.quantile(eq_hhinc_pre, weights = hhld_size * hhld_wgt, probs = 0.5, na.rm = TRUE)*0.6] 
 #XXX Get the correct post income
Rep_rates_df[,wm_50_BHC := eq_hours0 - weighted_median_50]
Rep_rates_df[,pre_wm_50 := eq_hhinc_pre - weighted_median_50]

# Create AHC measures

Rep_rates_df[,Rent := fifelse(is.na(Rent),0,Rent)]

Rep_rates_df[,current_AHC := hhincome_pre - Rent - Weekly_Mortgage_Repayments]# - current_RA - current_RA_partner: rent assistance is in the income where appropriate, so already net of rent
Rep_rates_df[,hours0_AHC := hhincome - Rent - Weekly_Mortgage_Repayments]#  - hours0_RA - hours0_RA_partner: So changes in RA are implicitly captured in changes in the net rent paid here for the poverty line.

Rep_rates_df[,AHC_eq_scale := 1 + 0.724*partnered + 0.35*Numb_dep] # New scale based on this (https://www.stats.govt.nz/assets/Uploads/Methods/Measuring-child-poverty-Equivalence-scale/measuring-child-poverty-equivalence-scale.pdf) due to the lower economies of scale in non-housing expenditures.

Rep_rates_df[,":=" (eq_current_AHC = current_AHC/AHC_eq_scale,eq_hours0_AHC = hours0_AHC/AHC_eq_scale)]

Rep_rates_df[, AHC_weighted_median_50 := wtd.quantile(eq_current_AHC, weights = hhld_size * hhld_wgt, probs = 0.5, na.rm = TRUE)*0.5] 

Rep_rates_df[,wm_50_AHC := eq_hours0_AHC - AHC_weighted_median_50]
Rep_rates_df[,pre_wm_50_AHC := eq_current_AHC - AHC_weighted_median_50]

# Drop those with negative incomes
Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$current_net_income >= 0)
Rep_rates_df <- subset(Rep_rates_df , Rep_rates_df$hours0_net_income >= 0)

setDT(Rep_rates_df)

# Create quantiles

Rep_rates_df[, quantile_current_net_income := cut(
  current_net_income,
  breaks = quantile(current_net_income, probs = seq(0, 1, by = 0.2), na.rm = TRUE),
  include.lowest = TRUE, labels = FALSE
)]

breaks_current_net_income <- quantile(Rep_rates_df$current_net_income, probs = seq(0, 1, by = 0.2), na.rm = TRUE)


Rep_rates_df[, quantile_eq_hhinc_pre := cut(
  eq_hhinc_pre,
  breaks = quantile(eq_hhinc_pre, probs = seq(0, 1, by = 0.2), na.rm = TRUE),
  include.lowest = TRUE, labels = FALSE
)]

breaks_eq_net_income <- quantile(Rep_rates_df$eq_hhinc_pre, probs = seq(0, 1, by = 0.2), na.rm = TRUE)

# Subset

Rep_rates_df_subset <- subset(Rep_rates_df, Rep_rates_df$hours > hour_limit)
Rep_rates_df_subset[, normalized_weight_total := SIHPSWT / sum(SIHPSWT)]

## Create liquid asset categories

if (liquid_type == "ind"){
  Rep_rates_df_subset$weeks_of_liquid_assets <- Rep_rates_df_subset$Liquid_Assets_Person / Rep_rates_df_subset$current_work_income
  
  # Replace weeks_of_liquid_assets with 0 if numerator or denominator are negative
  Rep_rates_df_subset$weeks_of_liquid_assets[Rep_rates_df_subset$Liquid_Assets_Person < 0 | Rep_rates_df_subset$current_work_income < 0] <- 0
} else{
  Rep_rates_df_subset$weeks_of_liquid_assets_hhldinc <- Rep_rates_df_subset$LiquidAssets_Household / (Rep_rates_df_subset$current_work_income+Rep_rates_df_subset$current_work_income_partner)
  Rep_rates_df_subset$weeks_of_liquid_assets <- Rep_rates_df_subset$LiquidAssets_Household / (Rep_rates_df_subset$current_work_income) # This is a comparison of household liquid assets to the loss of a single income.
}

# Replace weeks_of_liquid_assets with 104 if it exceeds 104
Rep_rates_df_subset$weeks_of_liquid_assets[Rep_rates_df_subset$weeks_of_liquid_assets > 104] <- 104

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

## Start with the "poverty by replacement rate" plots ----

## BHC
for (i in c(1,5,13)){
  summary_med <- Rep_rates_df_subset[,.(Home_owner,liquid = weeks_of_liquid_assets > i,net_RR,poverty_med = wm_50_BHC <= 0,eligible = eligibility_status != "Ineligible",IU_agg,weight = hhld_wgt*hhld_size)]
  
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
  
  # Create bins for net_RR
  summary_med[, net_RR_binned := cut(pmax(net_RR, 0), 
                                     breaks = seq(0, 1, by = 0.1), 
                                     include.lowest = TRUE, right = FALSE)]
  
  # Compute weighted proportions by net_RR bins
  poverty_summary_med_bins <- summary_med[, .(
    Total_Poverty_Proportion = weighted.mean(poverty_med, weight), # Total poverty proportion in the population
    Ineligible_Poverty = weighted.mean(!eligible & poverty_med, weight),
    Eligible_HomeOwner_Liquid = weighted.mean(eligible & Home_owner & liquid & poverty_med, weight),
    Eligible_HomeOwner_NotLiquid = weighted.mean(eligible & Home_owner & !liquid & poverty_med, weight),
    Eligible_NotHomeOwner_Liquid = weighted.mean(eligible & !Home_owner & liquid & poverty_med, weight),
    Eligible_NotHomeOwner_NotLiquid = weighted.mean(eligible & !Home_owner & !liquid & poverty_med, weight)
  ), by = .(net_RR_binned)]
  
  # Convert to long format
  poverty_long_med_bins <- melt(poverty_summary_med_bins, 
                                id.vars = c("net_RR_binned", "Total_Poverty_Proportion"), 
                                variable.name = "Category", 
                                value.name = "Proportion")
  
  poverty_long_med_bins
  
  poverty_long_med_bins[,net_RR_binned := fcase(net_RR_binned == "[0,0.1)","0-10%",
                                                net_RR_binned == "[0.1,0.2)","10-20%",
                                                net_RR_binned == "[0.2,0.3)","20-30%",
                                                net_RR_binned == "[0.3,0.4)","30-40%",
                                                net_RR_binned == "[0.4,0.5)","40-50%",
                                                net_RR_binned == "[0.5,0.6)","50-60%",
                                                net_RR_binned == "[0.6,0.7)","60-70%",
                                                net_RR_binned == "[0.7,0.8)","70-80%",
                                                net_RR_binned == "[0.8,0.9)","80-90%",
                                                default = NA)]
  
  BHC_plot <- ggplot(poverty_long_med_bins[net_RR_binned != "80-90%"], aes(x = net_RR_binned, y = Proportion*100, fill = Category)) +
    geom_bar(stat = "identity") +
    #geom_hline(aes(yintercept = Total_Poverty_Proportion), linetype = "dashed", color = "black") +
    labs_e61(subtitle = "Composition of Poverty by Replacement Rate - Median Income line",
             x = "",
             y = "%",
             fill = "Poverty Category",
             footnotes = c(paste0("Sufficient liquid assets equal ",i," weeks")),
             sources = c("e61","ABS")) + coord_flip() +
    scale_y_continuous_e61(limits = c(0,100,25)) +
    #theme_e61(legend = "bottom") + 
    format_flip() +
    #plab(label = c("Ineligible","House & Liquid","Home Owner","Liquid renter","Illiquid renter"),x = c(2,1.5,1,2,1.5),y= c(55,55,55,80,80),colour = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))
    plab(label = c("Ineligible","House & Liquid","Home Owner","Liquid renter","Illiquid renter"),x = c(2,1.5,1,3,2.5),y= c(55,55,55,55,55),colour = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))
  
  print(BHC_plot)
  save_e61(paste0("Pov_RR_BHC_",hour_limit,"_liq",i,".pdf"),pad_width = 1)
  
  ## AHC
  
  summary_med_AHC <- Rep_rates_df_subset[,.(Home_owner,liquid = weeks_of_liquid_assets > i,net_RR,poverty_med = wm_50_AHC <= 0,eligible = eligibility_status != "Ineligible",IU_agg,weight = hhld_wgt*hhld_size)]
  
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
  
  summary_med_AHC[, net_RR_binned := cut(pmax(net_RR, 0), 
                                         breaks = seq(0, 1, by = 0.1), 
                                         include.lowest = TRUE, right = FALSE)]
  
  # Compute weighted proportions by net_RR bins
  poverty_summary_med_bins_AHC <- summary_med_AHC[, .(
    Total_Poverty_Proportion = weighted.mean(poverty_med, weight), # Total poverty proportion in the population
    Ineligible_Poverty = weighted.mean(!eligible & poverty_med, weight),
    Eligible_HomeOwner_Liquid = weighted.mean(eligible & Home_owner & liquid & poverty_med, weight),
    Eligible_HomeOwner_NotLiquid = weighted.mean(eligible & Home_owner & !liquid & poverty_med, weight),
    Eligible_NotHomeOwner_Liquid = weighted.mean(eligible & !Home_owner & liquid & poverty_med, weight),
    Eligible_NotHomeOwner_NotLiquid = weighted.mean(eligible & !Home_owner & !liquid & poverty_med, weight)
  ), by = .(net_RR_binned)]
  
  # Convert to long format
  poverty_long_med_bins_AHC <- melt(poverty_summary_med_bins_AHC, 
                                    id.vars = c("net_RR_binned", "Total_Poverty_Proportion"), 
                                    variable.name = "Category", 
                                    value.name = "Proportion")
  
  
  poverty_long_med_bins_AHC[,net_RR_binned := fcase(net_RR_binned == "[0,0.1)","0-10%",
                                                    net_RR_binned == "[0.1,0.2)","10-20%",
                                                    net_RR_binned == "[0.2,0.3)","20-30%",
                                                    net_RR_binned == "[0.3,0.4)","30-40%",
                                                    net_RR_binned == "[0.4,0.5)","40-50%",
                                                    net_RR_binned == "[0.5,0.6)","50-60%",
                                                    net_RR_binned == "[0.6,0.7)","60-70%",
                                                    net_RR_binned == "[0.7,0.8)","70-80%",
                                                    net_RR_binned == "[0.8,0.9)","80-90%",
                                                    default = NA)]
  
  poverty_long_med_bins_AHC
  
  poverty_long_med_bins_AHC <- poverty_long_med_bins_AHC[net_RR_binned != "80-90%"]
  
  AHC_plot <- ggplot(poverty_long_med_bins_AHC, aes(x = net_RR_binned, y = Proportion*100, fill = Category)) +
    geom_bar(stat = "identity") +
    #geom_hline(aes(yintercept = Total_Poverty_Proportion), linetype = "dashed", color = "black") +
    labs_e61(subtitle = "Composition of Poverty by Replacement Rate - Median Income line (AHC)",
             x = "",
             y = "%",
             fill = "Poverty Category",
             footnotes = c(paste0("Sufficient liquid assets equal ",i," weeks")),
             sources = c("e61","ABS")) + coord_flip() +
    scale_y_continuous_e61(limits = c(0,100,25)) +
    #theme_e61(legend = "bottom") + 
    format_flip()
    #plab(label = c("Ineligible","House & Liquid","Home Owner","Liquid renter","Illiquid renter"),x = c(3.5,3,2.5,2,1.5),y= c(80,80,80,80,80),colour = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))
  
  print(AHC_plot)
  save_e61(paste0("Pov_RR_AHC_",hour_limit,"_liq",i,".pdf"),pad_width = 1)
  save_e61(paste0("Pov_RR_",hour_limit,"_liq",i,".pdf"),BHC_plot,AHC_plot,pad_width = 2)
}


### Household asset amounts ----
# Liquidity using household liquid assets


for (i in seq(5,30,5)){
  Rep_rates_df_subset[,hhld_LA_eq := LiquidAssets_Household/eq_scale]
  
  summary_med <- Rep_rates_df_subset[,.(Home_owner,liquid = hhld_LA_eq > i*1000,net_RR,poverty_med = wm_50_BHC <= 0,eligible = eligibility_status != "Ineligible",IU_agg,weight = hhld_wgt*hhld_size)]
  
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
  
  # Create bins for net_RR
  summary_med[, net_RR_binned := cut(pmax(net_RR, 0), 
                                     breaks = seq(0, 1, by = 0.1), 
                                     include.lowest = TRUE, right = FALSE)]
  
  # Compute weighted proportions by net_RR bins
  poverty_summary_med_bins <- summary_med[, .(
    Total_Poverty_Proportion = weighted.mean(poverty_med, weight), # Total poverty proportion in the population
    Ineligible_Poverty = weighted.mean(!eligible & poverty_med, weight),
    Eligible_HomeOwner_Liquid = weighted.mean(eligible & Home_owner & liquid & poverty_med, weight),
    Eligible_HomeOwner_NotLiquid = weighted.mean(eligible & Home_owner & !liquid & poverty_med, weight),
    Eligible_NotHomeOwner_Liquid = weighted.mean(eligible & !Home_owner & liquid & poverty_med, weight),
    Eligible_NotHomeOwner_NotLiquid = weighted.mean(eligible & !Home_owner & !liquid & poverty_med, weight)
  ), by = .(net_RR_binned)]
  
  # Convert to long format
  poverty_long_med_bins <- melt(poverty_summary_med_bins, 
                                id.vars = c("net_RR_binned", "Total_Poverty_Proportion"), 
                                variable.name = "Category", 
                                value.name = "Proportion")
  
  poverty_long_med_bins
  
  poverty_long_med_bins[,net_RR_binned := fcase(net_RR_binned == "[0,0.1)","0-10%",
                                                net_RR_binned == "[0.1,0.2)","10-20%",
                                                net_RR_binned == "[0.2,0.3)","20-30%",
                                                net_RR_binned == "[0.3,0.4)","30-40%",
                                                net_RR_binned == "[0.4,0.5)","40-50%",
                                                net_RR_binned == "[0.5,0.6)","50-60%",
                                                net_RR_binned == "[0.6,0.7)","60-70%",
                                                net_RR_binned == "[0.7,0.8)","70-80%",
                                                net_RR_binned == "[0.8,0.9)","80-90%",
                                                default = NA)]
  
  BHC_plot <- ggplot(poverty_long_med_bins[net_RR_binned != "80-90%"], aes(x = net_RR_binned, y = Proportion*100, fill = Category)) +
    geom_bar(stat = "identity") +
    #geom_hline(aes(yintercept = Total_Poverty_Proportion), linetype = "dashed", color = "black") +
    labs_e61(title = "Composition of Poverty by Replacement Rate - Median Income line",
             x = "",
             y = "%",
             fill = "Poverty Category",
             footnotes = c(paste0("Household Liquid Assets $",i*1000," or higher"))) + coord_flip() +
    scale_y_continuous_e61(limits = c(0,100,25)) +
    #theme_e61(legend = "bottom") + 
    format_flip() +
    plab(label = c("Ineligible","House & Liquid","Home Owner","Liquid renter","Illiquid renter"),x = c(2,1.5,1,2,1.5),y= c(55,55,55,80,80),colour = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))
  
  print(BHC_plot)
  
  ## AHC
  
  summary_med_AHC <- Rep_rates_df_subset[,.(Home_owner,liquid = LiquidAssets_Household > i*1000,net_RR,poverty_med = wm_50_AHC <= 0,eligible = eligibility_status != "Ineligible",IU_agg,weight = hhld_wgt*hhld_size)]
  
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
  
  summary_med_AHC[, net_RR_binned := cut(pmax(net_RR, 0), 
                                         breaks = seq(0, 1, by = 0.1), 
                                         include.lowest = TRUE, right = FALSE)]
  
  # Compute weighted proportions by net_RR bins
  poverty_summary_med_bins_AHC <- summary_med_AHC[, .(
    Total_Poverty_Proportion = weighted.mean(poverty_med, weight), # Total poverty proportion in the population
    Ineligible_Poverty = weighted.mean(!eligible & poverty_med, weight),
    Eligible_HomeOwner_Liquid = weighted.mean(eligible & Home_owner & liquid & poverty_med, weight),
    Eligible_HomeOwner_NotLiquid = weighted.mean(eligible & Home_owner & !liquid & poverty_med, weight),
    Eligible_NotHomeOwner_Liquid = weighted.mean(eligible & !Home_owner & liquid & poverty_med, weight),
    Eligible_NotHomeOwner_NotLiquid = weighted.mean(eligible & !Home_owner & !liquid & poverty_med, weight)
  ), by = .(net_RR_binned)]
  
  # Convert to long format
  poverty_long_med_bins_AHC <- melt(poverty_summary_med_bins_AHC, 
                                    id.vars = c("net_RR_binned", "Total_Poverty_Proportion"), 
                                    variable.name = "Category", 
                                    value.name = "Proportion")
  
  
  poverty_long_med_bins_AHC[,net_RR_binned := fcase(net_RR_binned == "[0,0.1)","0-10%",
                                                    net_RR_binned == "[0.1,0.2)","10-20%",
                                                    net_RR_binned == "[0.2,0.3)","20-30%",
                                                    net_RR_binned == "[0.3,0.4)","30-40%",
                                                    net_RR_binned == "[0.4,0.5)","40-50%",
                                                    net_RR_binned == "[0.5,0.6)","50-60%",
                                                    net_RR_binned == "[0.6,0.7)","60-70%",
                                                    net_RR_binned == "[0.7,0.8)","70-80%",
                                                    net_RR_binned == "[0.8,0.9)","80-90%",
                                                    default = NA)]
  
  poverty_long_med_bins_AHC
  
  poverty_long_med_bins_AHC <- poverty_long_med_bins_AHC[net_RR_binned != "80-90%"]
  
  AHC_plot <- ggplot(poverty_long_med_bins_AHC, aes(x = net_RR_binned, y = Proportion*100, fill = Category)) +
    geom_bar(stat = "identity") +
    #geom_hline(aes(yintercept = Total_Poverty_Proportion), linetype = "dashed", color = "black") +
    labs_e61(title = "Composition of Poverty by Replacement Rate - Median Income line (AHC)",
             x = "",
             y = "%",
             fill = "Poverty Category",
             footnotes = c(paste0("Household Liquid Assets $",i*1000," or higher"))) + coord_flip() +
    scale_y_continuous_e61(limits = c(0,100,25)) +
    #theme_e61(legend = "bottom") + 
    format_flip() +
    plab(label = c("Ineligible","House & Liquid","Home Owner","Liquid renter","Illiquid renter"),x = c(3.5,3,2.5,2,1.5),y= c(80,80,80,80,80),colour = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))
  
  #print(AHC_plot)
}


### Make animation
# Empty list to collect results
poverty_all <- list()

for (i in seq(5, 30, 5)) {
  summary_med <- Rep_rates_df_subset[, .(
    Home_owner,
    liquid = LiquidAssets_Household > i * 1000,
    net_RR,
    poverty_med = wm_50_BHC <= 0,
    eligible = eligibility_status != "Ineligible",
    IU_agg,
    weight = hhld_wgt * hhld_size
  )]
  
  summary_med[, net_RR_binned := cut(pmax(net_RR, 0), breaks = seq(0, 1, by = 0.1), include.lowest = TRUE, right = FALSE)]
  
  poverty_summary_med_bins <- summary_med[, .(
    Total_Poverty_Proportion = weighted.mean(poverty_med, weight),
    Ineligible_Poverty = weighted.mean(!eligible & poverty_med, weight),
    Eligible_HomeOwner_Liquid = weighted.mean(eligible & Home_owner & liquid & poverty_med, weight),
    Eligible_HomeOwner_NotLiquid = weighted.mean(eligible & Home_owner & !liquid & poverty_med, weight),
    Eligible_NotHomeOwner_Liquid = weighted.mean(eligible & !Home_owner & liquid & poverty_med, weight),
    Eligible_NotHomeOwner_NotLiquid = weighted.mean(eligible & !Home_owner & !liquid & poverty_med, weight)
  ), by = .(net_RR_binned)]
  
  poverty_long_med_bins <- melt(
    poverty_summary_med_bins,
    id.vars = c("net_RR_binned", "Total_Poverty_Proportion"),
    variable.name = "Category",
    value.name = "Proportion"
  )
  
  poverty_long_med_bins[, net_RR_binned := fcase(
    net_RR_binned == "[0,0.1)", "0-10%",
    net_RR_binned == "[0.1,0.2)", "10-20%",
    net_RR_binned == "[0.2,0.3)", "20-30%",
    net_RR_binned == "[0.3,0.4)", "30-40%",
    net_RR_binned == "[0.4,0.5)", "40-50%",
    net_RR_binned == "[0.5,0.6)", "50-60%",
    net_RR_binned == "[0.6,0.7)", "60-70%",
    net_RR_binned == "[0.7,0.8)", "70-80%",
    net_RR_binned == "[0.8,0.9)", "80-90%",
    default = NA_character_
  )]
  
  poverty_long_med_bins <- poverty_long_med_bins[!is.na(net_RR_binned)]
  poverty_long_med_bins[, threshold := paste0("$", i * 1000)]
  
  poverty_all[[as.character(i)]] <- poverty_long_med_bins
}

# Combine all into one data.table
poverty_combined <- rbindlist(poverty_all)

animated_plot <- ggplot(poverty_combined[net_RR_binned != "80-90%"], 
                        aes(x = net_RR_binned, y = Proportion * 100, fill = Category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Composition of Poverty by Replacement Rate (BHC)",
       subtitle = "Threshold HHLD Liquidity: {closest_state}",
       x = "", y = "%",
       fill = "Poverty Category") +
  scale_y_continuous(limits = c(0, 100)) +
  transition_states(threshold, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out") +
  plab(label = c("Ineligible","House & Liquid","Home Owner","Liquid renter","Illiquid renter"),x = c(2,1.5,1,2,1.5),y= c(55,55,55,80,80),colour = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) + format_flip()

# Save or display the animation
#animate(animated_plot, nframes = 100, fps = 10, width = 800, height = 600)
# Broken in new theme61

anim <- animate(animated_plot, 
                nframes = 100, 
                fps = 10, 
                width = 800, 
                height = 600, 
                renderer = gifski_renderer())

# Save to file
anim_save("poverty_animation.gif", animation = anim)

## Make a plot of eligible individuals by family type

poverty_summary_med_BHC <- summary_med[, .(
  Total_Poverty_Proportion = weighted.mean(poverty_med, weight), # Total poverty proportion in the population
  Ineligible_Poverty = weighted.mean(!eligible & poverty_med, weight),
  Eligible_HomeOwner_Liquid = weighted.mean(eligible & Home_owner & liquid & poverty_med, weight),
  Eligible_HomeOwner_NotLiquid = weighted.mean(eligible & Home_owner & !liquid & poverty_med, weight),
  Eligible_NotHomeOwner_Liquid = weighted.mean(eligible & !Home_owner & liquid & poverty_med, weight),
  Eligible_NotHomeOwner_NotLiquid = weighted.mean(eligible & !Home_owner & !liquid & poverty_med, weight)
), by = .(IU_agg)]

poverty_all_IU <- list()

for (i in seq(5, 30, 5)) {
  summary_med <- Rep_rates_df_subset[, .(
    Home_owner,
    liquid = LiquidAssets_Household > i * 1000,
    net_RR,
    poverty_med = wm_50_BHC <= 0,
    eligible = eligibility_status != "Ineligible",
    IU_agg,
    weight = hhld_wgt * hhld_size
  )]
  
  summary_med[, net_RR_binned := cut(pmax(net_RR, 0), breaks = seq(0, 1, by = 0.1), include.lowest = TRUE, right = FALSE)]
  
  poverty_summary_med_bins <- summary_med[, .(
    Total_Poverty_Proportion = weighted.mean(poverty_med, weight),
    Ineligible_Poverty = weighted.mean(!eligible & poverty_med, weight),
    Eligible_HomeOwner_Liquid = weighted.mean(eligible & Home_owner & liquid & poverty_med, weight),
    Eligible_HomeOwner_NotLiquid = weighted.mean(eligible & Home_owner & !liquid & poverty_med, weight),
    Eligible_NotHomeOwner_Liquid = weighted.mean(eligible & !Home_owner & liquid & poverty_med, weight),
    Eligible_NotHomeOwner_NotLiquid = weighted.mean(eligible & !Home_owner & !liquid & poverty_med, weight)
  ), by = .(IU_agg)]
  
  poverty_long_med_bins <- melt(
    poverty_summary_med_bins,
    id.vars = c("IU_agg", "Total_Poverty_Proportion"),
    variable.name = "Category",
    value.name = "Proportion"
  )
  
  poverty_long_med_bins[, threshold := paste0("$", i * 1000)]
  
  poverty_all_IU[[as.character(i)]] <- poverty_long_med_bins
}

# Combine all into one data.table
poverty_combined_IU <- rbindlist(poverty_all_IU)

animated_plot_IU <- ggplot(poverty_combined_IU, 
                        aes(x = IU_agg, y = Proportion * 100, fill = Category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Composition of Poverty by Replacement Rate (BHC)",
       subtitle = "Threshold HHLD Liquidity: {closest_state}",
       x = "", y = "%",
       fill = "Poverty Category") +
  scale_y_continuous(limits = c(0, 100)) +
  transition_states(threshold, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out") +
  plab(label = c("Ineligible","House & Liquid","Home Owner","Liquid renter","Illiquid renter"),x = c(2,1.5,1,2,1.5),y= c(55,55,55,80,80),colour = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) + format_flip()

# Save or display the animation
#animate(animated_plot, nframes = 100, fps = 10, width = 800, height = 600)

anim_IU <- animate(animated_plot_IU, 
                nframes = 100, 
                fps = 10, 
                width = 800, 
                height = 600, 
                renderer = gifski_renderer())


#### Make plots of liquidity in note ----
# First, share of eligible households that in poverty and have insufficient liquid assets between 1 and 13 weeks - note above estimates look at everyone.

liquid_list <- list()

for (i in seq(1,13,1)){
  summary_med <- Rep_rates_df_subset[,.(Home_owner,liquid = weeks_of_liquid_assets > i,net_RR,poverty_med = wm_50_BHC <= 0,eligible = eligibility_status != "Ineligible",IU_agg,weight = hhld_wgt*hhld_size)]
  
  summary_med <- summary_med[eligible == TRUE] # Only calculate for eligible
  
  summary_med[, .(
    Poverty_Proportion = weighted.mean(poverty_med, weight),
    Home_Owner_Proportion_In_Poverty = weighted.mean(Home_owner, weight, na.rm = TRUE),
    Liquid_Proportion_In_Poverty = weighted.mean(liquid, weight, na.rm = TRUE),
    Home_Owner_And_Liquid_Proportion_In_Poverty = weighted.mean(Home_owner & liquid, weight, na.rm = TRUE),
    Ineligible_Proportion_In_Poverty = weighted.mean(!eligible, weight, na.rm = TRUE)
  ), by = .(IU_agg)]
  
  poverty_summary_med <- summary_med[, .(
    #Total_Poverty_Proportion = weighted.mean(poverty_med, weight), # Total poverty proportion in the population
    #Ineligible_Poverty = weighted.mean(!eligible & poverty_med, weight),
    Eligible_HomeOwner_Liquid = weighted.mean(eligible & Home_owner & liquid & poverty_med, weight),
    Eligible_HomeOwner_NotLiquid = weighted.mean(eligible & Home_owner & !liquid & poverty_med, weight),
    Eligible_NotHomeOwner_Liquid = weighted.mean(eligible & !Home_owner & liquid & poverty_med, weight),
    Eligible_NotHomeOwner_NotLiquid = weighted.mean(eligible & !Home_owner & !liquid & poverty_med, weight)
  ), by = .(IU_agg)]
  
  poverty_long_med <- melt(poverty_summary_med, 
                           #id.vars = c("IU_agg", "Total_Poverty_Proportion"),
                           id.vars = c("IU_agg"), 
                           variable.name = "Category", 
                           value.name = "Proportion")
  
  # Create bins for net_RR
  
  BHC_plot <- ggplot(poverty_long_med, aes(x = IU_agg, y = Proportion*100, fill = Category)) +
    geom_bar(stat = "identity") +
    #geom_hline(aes(yintercept = Total_Poverty_Proportion), linetype = "dashed", color = "black") +
    labs_e61(title = "Composition of Poverty by Replacement Rate - Median Income line",
             x = "",
             y = "%",
             fill = "Poverty Category",
             footnotes = c(paste0("Sufficient liquid assets equal ",i," weeks"))) + coord_flip() +
    scale_y_continuous_e61(limits = c(0,100,25)) +
    #theme_e61(legend = "bottom") + 
    format_flip() +
    plab(label = c("Ineligible","House & Liquid","Home Owner","Liquid renter","Illiquid renter"),x = c(2,1.5,1,2,1.5),y= c(55,55,55,80,80),colour = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))
  
  print(BHC_plot)
  
  poverty_long_med[, threshold := i]
  
  liquid_list[[as.character(i)]] <- poverty_long_med
}

liquid_list

liquid_combined <- rbindlist(liquid_list)

ggplot(liquid_combined,aes(x=threshold,y=Proportion,colour=IU_agg)) + geom_line() +
  facet_wrap(~ Category)

ggplot(liquid_combined[Category == "Eligible_NotHomeOwner_NotLiquid"],aes(x=threshold,y=Proportion,colour=IU_agg)) + geom_line() +
  theme_e61(legend = "bottom")

unique(liquid_combined$Category)

ggplot(liquid_combined[Category %in% c("Eligible_NotHomeOwner_NotLiquid","Eligible_HomeOwner_NotLiquid"),.(Proportion = sum(Proportion)),by=.(IU_agg,threshold)],aes(x=threshold,y=Proportion,colour=IU_agg)) +
  geom_line() +
  scale_y_continuous_e61(limits = c(0,0.5,0.1)) +
  scale_x_continuous_e61(limits = c(1,13,1))


