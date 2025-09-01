## Last update:  28/07/2025
## Author:  Matt Maltman
## Last update person:  Matt Nolan (adding FT versions of plots, and adjusting some stuff to DT). More recently changing graph format for new theme61
# Note: Refine the code down a bit to make the datasets being used clear and transparent - to avoid using the wrong data accidentally.

library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(tidyverse)
library(data.table)
library(readxl)

rm(list=ls())
gc()

work = TRUE

##################


#### Raw output from the tax calculator. Start here if you're using that output
#### Otherwise start from the mark below. 
# 
# Rep_rates_df <- read_csv("C:/Users/MattNolan/Downloads/RRs_csv 2.csv")
# setDT(Rep_rates_df)
# 
# ##########################################################################################33
# 
# ########### Does the person fail the assets test?
# Rep_rates_df$Asset_test_flag <- 0 
# Rep_rates_df$Asset_test_flag <- ifelse(Rep_rates_df$Home_owner == 0 & 
#                                          Rep_rates_df$partnered == 1 & 
#                                          Rep_rates_df$Illiquid_Assets +
#                                          Rep_rates_df$LiquidAssets_Household > 722000,
#                                        1, Rep_rates_df$Asset_test_flag)
# Rep_rates_df$Asset_test_flag <- ifelse(Rep_rates_df$Home_owner == 0 &
#                                          Rep_rates_df$partnered == 0 &
#                                          Rep_rates_df$Illiquid_Assets +
#                                          Rep_rates_df$Liquid_Assets_Person > 566000,
#                                        1, Rep_rates_df$Asset_test_flag)
# 
# Rep_rates_df$Asset_test_flag <- ifelse(Rep_rates_df$Home_owner == 1 &
#                                          Rep_rates_df$partnered == 1 &
#                                          Rep_rates_df$Illiquid_Assets +
#                                          Rep_rates_df$LiquidAssets_Household > 470000, 1,
#                                        Rep_rates_df$Asset_test_flag)
# Rep_rates_df$Asset_test_flag <- ifelse(Rep_rates_df$Home_owner == 1 &
#                                          Rep_rates_df$partnered == 0 &
#                                          Rep_rates_df$Illiquid_Assets +
#                                          Rep_rates_df$Liquid_Assets_Person > 314000,
#                                        1, Rep_rates_df$Asset_test_flag)
# ###################################3
# 
# #### Does the person fail the JSP Partner income test
# 
# Rep_rates_df$partner_test_flag <- ifelse(Rep_rates_df$partner_earnings > 61347, 1, 0)
# 
# Rep_rates_df$nonworkincome_income_test <- ifelse(Rep_rates_df$NonWageIncome > 2263.75,
#                                                  1, 0)
# Rep_rates_df$nonworkincome_income_test <- ifelse(Rep_rates_df$NonWageIncome > 1479 & Rep_rates_df$Numb_dep == 0,
#                                                  1, Rep_rates_df$nonworkincome_income_test)
# Rep_rates_df$nonworkincome_income_test <- ifelse(Rep_rates_df$NonWageIncome > 1368 & Rep_rates_df$partnered == 1,
#                                                  1, Rep_rates_df$nonworkincome_income_test)
# 
# 
# ######
# 
# 
# Rep_rates_df$ineligible <- ifelse(Rep_rates_df$taxben_flag + Rep_rates_df$partner_test_flag  > 0, 1, 0)
# 
# 
# 
# write.csv( Rep_rates_df, "RRs_csv.csv")
# 
# 
# ############################################################################################################
##########################################################################################################

# START HERE IF YOU HAVE DONE THE ABOVE

if (work == TRUE){
  Rep_rates_df <- read_csv("C:/Users/MattNolan/Downloads/RRs_csv 3.csv") # Work version
} else {
  Rep_rates_df <- read_csv("C:/Users/OEM/Downloads/RRs_csv 3.csv") # Home version
}

setDT(Rep_rates_df)

hour_limit <- 30

############################################################################################

### Bug in the tax calc where negative incomes returned NAs on HECS replayments. FiX! 

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






###########################################################################################3

### Clean the Data - This should already be done for some of this in the tax Calc stuff

Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$AGEEC > 21)

Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$AGEEC < 55)

Rep_rates_df <- subset(Rep_rates_df, wage > 15)


# Drop those with Negative incomes 

Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$current_net_income >= 0)
Rep_rates_df <- subset(Rep_rates_df , Rep_rates_df$hours0_net_income >= 0)
Rep_rates_df <- subset(Rep_rates_df , Rep_rates_df$NonWageIncome >= 0)
Rep_rates_df[is.na(current_wk_partner_earnings), current_wk_partner_earnings := 0]


##########################################

#### AGGREGATE REPLACEMENT RATES #########################################################
####                             #########################################################
####                            ##########################################################

################# Subset data to remove those working less than 5 hours. 
Rep_rates_df_subset <- subset(Rep_rates_df, Rep_rates_df$hours > hour_limit)

#### Throughout, we use "normalized_weight", which says the proportion of the population 
### The individual represents. 
Rep_rates_df_subset[, normalized_weight := SIHPSWT / sum(SIHPSWT)]

# Calculate the weighted mean replacement rate
mean_net_RR_3 <- sum(Rep_rates_df_subset$net_RR * Rep_rates_df_subset$SIHPSWT) / 
  sum(Rep_rates_df_subset$SIHPSWT)

# Calculate the weighted median replacement rate
sorted_data <- Rep_rates_df_subset[order(net_RR)]
cumulative_weights <- cumsum(sorted_data$normalized_weight)
median_index <- which(cumulative_weights >= 0.5)[1]
median_net_RR_3 <- sorted_data$net_RR[median_index]

# Plot Baselin Replacements 
RR1 <- ggplot(Rep_rates_df_subset, aes(x = net_RR * 100, weight = normalized_weight * 100)) +
  geom_histogram(binwidth = 1, fill = e61_tealdark) +
  geom_vline(aes(xintercept = mean_net_RR_3 * 100), color = "purple", linetype = "dashed") +
  geom_vline(aes(xintercept = median_net_RR_3 * 100), color = e61_orangedark, linetype = "dashed") +
  labs(
    title = "Distribution of Replacement Rates",
    x = "Replacement Rate",
    y = "%"
  ) + plot_label(c("Mean", "Median"),c(25,4), c(25,25), c("purple", e61_orangedark))


save_e61(paste0("RR_hour_min",hour_limit,".svg"), RR1)

######################
### Replacement rates for those who are partnered with positive earnings # MN Note this has been setting all the data to partnered only at this stage.
Rep_rates_df_partnered <- subset(Rep_rates_df_subset,
                              hours0_wk_partner_earnings + current_wk_partner_earnings > 0)


# Calculate the weighted mean
mean_net_RR_3 <- sum(Rep_rates_df_partnered$net_RR_household * Rep_rates_df_partnered$SIHPSWT) / 
  sum(Rep_rates_df_partnered$SIHPSWT)

# Calculate the weighted median
sorted_data <- Rep_rates_df_partnered[order(net_RR_household)]
cumulative_weights <- cumsum(sorted_data$normalized_weight)
median_index <- which(cumulative_weights >= 0.5)[1]
median_net_RR_3 <- sorted_data$net_RR_household[median_index]

Rep_rates_df_partnered[, normalized_weight2 := SIHPSWT / sum(SIHPSWT)]

RR2 <- ggplot(Rep_rates_df_partnered, aes(x = net_RR_household * 100, weight = normalized_weight2 * 100)) +
  geom_histogram(binwidth = 1,  fill = e61_tealdark) +
  labs(title = "Labour Income Replacement Rates for the household",
       subtitle = "If one person loses their job, but the other is unaffected", 
       x = "Household labour income with one job loss /  \nCurrent Household Labour Income ",
       y = "%") + xlim(-0.05, 100) +
  geom_vline(aes(xintercept = mean_net_RR_3 * 100), color = "purple", linetype = "dashed") +
  geom_vline(aes(xintercept = median_net_RR_3 * 100), color = e61_orangedark, linetype = "dashed") +
  plot_label(c("Mean", "Median"),c(40,57), c(3,3), c("purple", e61_orangedark))

save_e61(paste0("RR_household_hour_min",hour_limit,".svg"), RR2)

################ Loss of Income After Job loss 

Rep_rates_df_partnered$HouseholdIncomeAfterJobLoss <- (Rep_rates_df_partnered$hours0_net_income + 
                                                      Rep_rates_df_partnered$NonWageIncome +
                                                      Rep_rates_df_partnered$hours0_wk_partner_earnings)/
  (Rep_rates_df_partnered$current_net_income + Rep_rates_df_partnered$NonWageIncome + 
     Rep_rates_df_partnered$current_wk_partner_earnings)

# Calculate the weighted median
sorted_data <- Rep_rates_df_partnered[order(HouseholdIncomeAfterJobLoss)]
cumulative_weights <- cumsum(sorted_data$normalized_weight2)
median_index <- which(cumulative_weights >= 0.5)[1]
median_net_RR_3 <- sorted_data$HouseholdIncomeAfterJobLoss[median_index]

# Calculate the weighted mean
mean_net_RR_3 <- sum(Rep_rates_df_partnered$HouseholdIncomeAfterJobLoss * Rep_rates_df_partnered$SIHPSWT) / 
  sum(Rep_rates_df_partnered$SIHPSWT)


NotanRR <- ggplot(Rep_rates_df_partnered, aes(x = HouseholdIncomeAfterJobLoss * 100, weight = normalized_weight2 * 100)) +
  geom_histogram(binwidth = 1,  fill = e61_tealdark) +
  labs(title = "All Household Income After Job Loss",
       subtitle = "If one person loses their job, but the other is unaffected", 
       x = "Household Income as a % previous \npre-job loss household income ",
       y = "%") + xlim(-0.05, 100) +
  geom_vline(aes(xintercept = mean_net_RR_3 * 100), color = "purple", linetype = "dashed") +
  geom_vline(aes(xintercept = median_net_RR_3 * 100), color = e61_orangedark, linetype = "dashed") + 
  plot_label(c("Mean", "Median"),c(40,59), c(3,3), c("purple", e61_orangedark))

save_e61(paste0("Post_lossY_household_hour_min",hour_limit,".svg"), NotanRR)


###############################################################################################

### Alternative metric

#Rep_rates_df_partnered$HouseholdIncomeAfterJobLoss <- (Rep_rates_df_partnered$hours0_net_income +
#                                                      Rep_rates_df_partnered$wk_partner_earnings)/
#  (Rep_rates_df_partnered$current_net_income + Rep_rates_df_partnered$wk_partner_earnings)

Rep_rates_df_partnered[,HouseholdIncomeAfterJobLoss := (hours0_net_income + hours0_wk_partner_earnings)/(current_net_income + current_wk_partner_earnings)]

# Calculate the weighted median
sorted_data <- Rep_rates_df_partnered[order(HouseholdIncomeAfterJobLoss)]
cumulative_weights <- cumsum(sorted_data$normalized_weight)
median_index <- which(cumulative_weights >= 0.5)[1]
median_net_RR_3 <- sorted_data$HouseholdIncomeAfterJobLoss[median_index]

# Calculate the weighted mean
mean_net_RR_3 <- sum(Rep_rates_df_partnered$HouseholdIncomeAfterJobLoss * Rep_rates_df_partnered$SIHPSWT) / 
  sum(Rep_rates_df_partnered$SIHPSWT)


NotanRR <- ggplot(Rep_rates_df_partnered, aes(x = HouseholdIncomeAfterJobLoss * 100, weight = normalized_weight2 * 100)) +
  geom_histogram(binwidth = 1,  fill = e61_tealdark) +
  labs(title = "Household labour income ",
       subtitle = "If one person loses their job, but the other is unaffected", 
       x = "Household Income as a % previous \npre-job loss household income ",
       y = "%") + xlim(-0.05, 100) +
  geom_vline(aes(xintercept = mean_net_RR_3 * 100), color = "purple", linetype = "dashed") +
  geom_vline(aes(xintercept = median_net_RR_3 * 100), color = e61_orangedark, linetype = "dashed") + 
  plot_label(c("Mean", "Median"),c(40,59), c(3,3), c("purple", e61_orangedark))

save_e61(paste0("Post_lossY_household2_hour_min",hour_limit,".svg"), NotanRR)


####################################################################################################3

#### Singles Only ##########################################################################

Rep_rates_df4 <- subset(Rep_rates_df_subset, partnered == 0)

Rep_rates_df4[, normalized_weight2 := SIHPSWT / sum(SIHPSWT)]

RR3 <- ggplot(Rep_rates_df4, aes(x = net_RR * 100, weight = normalized_weight2 * 100)) +
  geom_histogram(binwidth = 1,  fill = e61_tealdark) +
  labs(title = "Distribution of Replacement Rates for singles",
       subtitle = "", 
       x = "Replacement Rate",
       y = "%") 

save_e61(paste0("RR_single_hour_min",hour_limit,".svg"), RR3)

###############################################################################################

### Only those eligible for the benefit ####################################################

# Filter rows where net_RR > 0
Rep_rates_df_subset_filtered <- Rep_rates_df_subset[hours0_taxable_benefit > 0]
Rep_rates_df_subset_filtered[, normalized_weight2 := SIHPSWT / sum(SIHPSWT) * 100]
# Calculate the weighted mean
mean_net_RR_3 <- sum(Rep_rates_df_subset_filtered$net_RR * Rep_rates_df_subset_filtered$SIHPSWT) / 
  sum(Rep_rates_df_subset_filtered$SIHPSWT)

mean_net_RR_3
max(Rep_rates_df_subset_filtered$net_RR)


# Calculate the weighted median
sorted_data <- Rep_rates_df_subset_filtered[order(net_RR)]
cumulative_weights <- cumsum(sorted_data$normalized_weight2 / 100)
median_index <- which(cumulative_weights >= 0.5)[1]
median_net_RR_3 <- sorted_data$net_RR[median_index]


RR3 <- ggplot(Rep_rates_df_subset_filtered, aes(x = net_RR * 100, weight = normalized_weight2)) +
  geom_histogram(binwidth = 1,  fill = e61_tealdark) +
  labs(title = "Distribution of Replacement Rates: those eligible for an income support payment",
       subtitle = "", 
       x = "Replacement Rate",
       y = "%") +
  geom_vline(aes(xintercept = mean_net_RR_3 * 100), color = "purple", linetype = "dashed") +
  geom_vline(aes(xintercept = median_net_RR_3 * 100), color = e61_orangedark, linetype = "dashed") 

save_e61(paste0("RR_recipient_hour_min",hour_limit,".svg"), RR3)


###########################################################################################

############### Replacement rates by Characteristics #####################################
################ #########################################################################
#########################################################################################

# Combine 1 and 2 dependents into one category and rename categories
Rep_rates_df_subset[, Numb_dep_cat := fifelse(Numb_dep >= 3, "3+", 
                                         fifelse(Numb_dep %in% c(1, 2), "1-2", 
                                                 as.character(Numb_dep)))]

# Create more descriptive interaction categories
Rep_rates_df_subset[, interaction_cat := paste0(
  fifelse(partnered == 0, "Single", "Partnered"), 
  ", ", 
  fifelse(Numb_dep_cat == "0", "no dependents", 
          fifelse(Numb_dep_cat == "1-2", "1-2 dependents", 
                  "3+ dependents"))
)]

# Manually order the interaction_cat column
interaction_order <- c("Single, no dependents",
                       "Single, 1-2 dependents", "Single, 3+ dependents",
                       "Partnered, no dependents",
                       "Partnered, 1-2 dependents",
                       "Partnered, 3+ dependents"
)
Rep_rates_df_subset[, interaction_cat := factor(interaction_cat, levels = interaction_order)]

# Normalize weights within the new interaction categories
Rep_rates_df_subset[, normalized_weight := SIHPSWT / sum(SIHPSWT), by = interaction_cat]

# Calculate weighted mean and median for each interaction_cat
facet_summary <- Rep_rates_df_subset[, .(
  weighted_mean = sum(net_RR * normalized_weight) / sum(normalized_weight),
  weighted_median = {
    ord <- order(net_RR)
    x <- net_RR[ord]
    w <- normalized_weight[ord]
    cum_w <- cumsum(w) / sum(w)
    x[which(cum_w >= 0.5)[1]]
  }
), by = interaction_cat]

# Plot with facet_wrap and ordered categories, adding both mean and median lines
FamilyPanel <- ggplot(Rep_rates_df_subset, aes(x = net_RR * 100, weight = normalized_weight * 100)) +
  geom_histogram(binwidth = 5, fill = e61_tealdark, col = "black") +
  facet_wrap(~interaction_cat, ncol = 3, nrow = 2) +
  labs_e61(
    subtitle = "Replacement Rates by Partner Status and Number of Dependents",
    x = "Replacement Rate (%)",
    y = "%", 
    footnotes = "The red dashed line represents the mean replacement rate,
    and the purple dashed line represents the median replacement rate for that category."
  ) +
  geom_vline(data = facet_summary, aes(xintercept = weighted_mean * 100), 
             color = "purple", linetype = "dashed", size = 1) +
  geom_vline(data = facet_summary, aes(xintercept = weighted_median * 100), 
             color = e61_orangedark, linetype = "dashed", size = 1) +
  plab(x=c(30,30),y=c(45,35),label = c("Mean","Median"),colour=c(e61_orangedark,"purple"),facet_name = "interaction_cat",facet_value = "Partnered, 1-2 dependents")

FamilyPanel

# Save the plot
save_e61(paste0("FamilyPanel_hour_min",hour_limit,".svg"), FamilyPanel, 
         footnotes = "The red dashed line represents the mean replacement rate,
         and the purple dashed line represents the median replacement rate for that category.")

save_e61(paste0("FamilyPanel_hour_min",hour_limit,".pdf"), FamilyPanel, 
         footnotes = "The red dashed line represents the mean replacement rate,
         and the purple dashed line represents the median replacement rate for that category.")


################################################################################################################################3

# Ensure Numb_dep is categorized as 1, 2, 3, and 4+
Rep_rates_df_subset[, Numb_dep_cat := fifelse(Numb_dep >= 3, "3+", as.character(Numb_dep))]

# Create interaction between Numb_dep and partnered
Rep_rates_df_subset[, interaction_cat := paste0("Partnered: ", partnered, ", Numb_dep: ", Numb_dep_cat)]

# Normalize weights by `Numb_dep_cat` within each `partnered` group
Rep_rates_df_subset[, normalized_weight := SIHPSWT / sum(SIHPSWT), by = .(partnered, Numb_dep_cat)]

# Calculate weighted mean for each `partnered` and `Numb_dep_cat`
facet_summary <- Rep_rates_df_subset[, .(
  weighted_mean = sum(net_RR * normalized_weight) / sum(normalized_weight)
), by = .(partnered, Numb_dep_cat)]

# Define the color palette for the categories
color_palette <- c("0" = e61_tealdark,  "1" = e61_maroondark, "2" = e61_greydark, "3+" = e61_coraldark)

# Plot for Partnered == 0
FamilyPanel_Partnered0 <- ggplot(Rep_rates_df_subset[partnered == 0], 
                                 aes(x = net_RR * 100, weight = normalized_weight,
                                     color = Numb_dep_cat, fill = Numb_dep_cat)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = facet_summary[partnered == 0], 
             aes(xintercept = weighted_mean * 100, color = Numb_dep_cat), 
             linetype = "dashed") +
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +
  labs(title = "Single",
       x = "Replacement Rate (%)",
       y = "Density",
       color = "Number of Dependents",
       fill = "Number of Dependents") +
  xlim(-5, 100)

# Plot for Partnered == 1
FamilyPanel_Partnered1 <- ggplot(Rep_rates_df_subset[partnered == 1], 
                                 aes(x = net_RR * 100, weight = normalized_weight,
                                     color = Numb_dep_cat, fill = Numb_dep_cat)) +
  geom_density(alpha = 0.1, adjust = 0.1) +
  geom_vline(data = facet_summary[partnered == 1], 
             aes(xintercept = weighted_mean * 100, color = Numb_dep_cat), 
             linetype = "dashed") +
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +
  labs(title = "Partnered",
       x = "Replacement Rate (%)",
       y = "Density",
       color = "Number of Dependents",
       fill = "Number of Dependents") +
  xlim(0, 100)

# Save the plots
save_e61(paste0("FamilyPanel_Partnered0_hour_min",hour_limit,".svg"), FamilyPanel_Partnered0,FamilyPanel_Partnered1 , 
         footnotes = "Dashed lines represent the weighted mean replacement rate for each category",
         title = "Replacement Rates by Number of Dependent Children")
save_e61(paste0("FamilyPanel_Partnered1_hour_min",hour_limit,".svg"), FamilyPanel_Partnered1, 
         footnotes = "Dashed lines represent the weighted mean replacement rate for each category")




########################################################################################################################################
############################################################################################################################3
# By prior income

setDT(Rep_rates_df_subset)
Rep_rates_df_subset[, income_group := fifelse(current_work_income < 500, "< $500",
                                         fifelse(current_work_income >= 500 & current_work_income < 1000, "$500 - $1000",
                                                 fifelse(current_work_income >= 1000 & current_work_income < 1500, "$1000 - $1500",
                                                         "> $1500")))]

Rep_rates_df_subset[, income_group := factor(income_group, 
                                        levels = c("< $500", "$500 - $1000", "$1000 - $1500", "> $1500"))]

# Normalize weights by income_group
Rep_rates_df_subset[, normalized_weight := SIHPSWT / sum(SIHPSWT), by = income_group]

# Calculate weighted means and medians for each income group
facet_summary_income <- Rep_rates_df_subset[, .(
  weighted_mean = sum(net_RR * normalized_weight) / sum(normalized_weight),
  weighted_median = {
    ord <- order(net_RR)
    x <- net_RR[ord]
    w <- normalized_weight[ord]
    cum_w <- cumsum(w) / sum(w)
    x[which(cum_w >= 0.5)[1]]
  }
), by = income_group]

# Plot with facet_wrap, adding both mean and median lines
IncomeGroupPanel <- ggplot(Rep_rates_df_subset, aes(x = net_RR * 100, weight = normalized_weight * 100)) +
  geom_histogram(binwidth = 5, fill = e61_tealdark) +
  facet_wrap(~income_group, ncol = 2, nrow = 2) +
  labs_e61(
    subtitle = "Replacement Rates by Weekly Labour Income Pre Job Loss",
    x = "Replacement Rate (%)",
    y = "%", 
    footnotes = "The red dashed line is the median replacement rate,
    and the purple dashed line is the mean replacement rate for that category."
  ) +
  geom_vline(data = facet_summary_income, aes(xintercept = weighted_mean * 100), 
             color = "purple", linetype = "dashed", size = 1) +
  geom_vline(data = facet_summary_income, aes(xintercept = weighted_median * 100), 
             color = e61_orangedark, linetype = "dashed", size = 1)

# Save the plot
save_e61(paste0("UpdatedIncome_hour_min",hour_limit,".svg"), IncomeGroupPanel)


Rep_rates_df_subset[, normalized_weight := SIHPSWT / sum(SIHPSWT)]

# Stacked histogram filled by income_group
IncomeGroupPanel <- ggplot(Rep_rates_df_subset, aes(x = net_RR * 100, weight = normalized_weight * 100, fill = income_group)) +
  geom_histogram(binwidth = 2, position = "stack", color = "black") +
  labs_e61(
    subtitle = "B. Replacement Rates by Weekly Labour Income Pre Job Loss",
    x = "Replacement Rate (%)",
    y = "%",
    footnotes = "Dashed line is the median for each category."
  )  +
  geom_vline(data = facet_summary_income, aes(xintercept = weighted_mean * 100, color = income_group), 
              linetype = "dashed", size = 1) +
  # Apply consistent colors to both fill and color
  scale_fill_manual(
    name = "Income Group",
    values = c(
      "< $500" = e61_teallight, 
      "$500 - $1000" = e61_tealdark, 
      "$1000 - $1500" = e61_orangedark, 
      "> $1500" = e61_maroondark
    )
  ) +
  scale_color_manual(
    name = "Income Group",
    values = c(
      "< $500" = e61_teallight, 
      "$500 - $1000" = e61_tealdark, 
      "$1000 - $1500" = e61_orangedark, 
      "> $1500" = e61_maroondark
    )
  )  + plot_label(c(">$1500", "$1000-1500", "$500-$1000", "<$500"), 
                 c(70, 70, 70, 70), c(32, 29, 26, 23), c(e61_maroondark, e61_orangedark,  e61_tealdark, e61_teallight))

# Save the plot
save_e61(paste0("UpdatedIncome2_hour_min",hour_limit,".svg"), IncomeGroupPanel)


#######################################################################################################################################3

# by liquid assets 
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


# Normalize weights by Liquid_Assets_Person_group
Rep_rates_df_subset[, normalized_weight := SIHPSWT / sum(SIHPSWT), by = Liquid_Assets_Person_group]

# Calculate weighted medians for each Liquid_Assets_Person_group
facet_summary_Liquid_Assets_Person <- Rep_rates_df_subset[, .(
  weighted_median = {
    ord <- order(net_RR)
    x <- net_RR[ord]
    w <- normalized_weight[ord]
    cum_w <- cumsum(w) / sum(w)
    x[which(cum_w >= 0.5)[1]]
  }
), by = Liquid_Assets_Person_group]

# Order the Liquid_Assets_Person_group
Rep_rates_df_subset[, Liquid_Assets_Person_group := factor(Liquid_Assets_Person_group, 
                                          levels = c("< 1000", "1,000 - 10,000", "10,000 - 50,000", "50,000+"))]

Liquid_Assets_PersonPanel <- ggplot(Rep_rates_df_subset, aes(x = net_RR * 100, weight = normalized_weight * 100)) +
  geom_histogram(binwidth = 5, fill = e61_tealdark) +
  facet_wrap(~Liquid_Assets_Person_group, ncol = 2, nrow = 2) +  # Adjust layout if needed
  geom_vline(data = facet_summary_Liquid_Assets_Person, aes(xintercept = weighted_median * 100), color = e61_orangedark, linetype = "dashed") +
  labs(title = "Replacement Rates by Liquid Assets",
       x = "Replacement Rate (%)",
       y = "%") + xlim(-5,100)


save_e61(paste0("LiquidAssets_hour_min",hour_limit,".svg"), Liquid_Assets_PersonPanel, footnotes =  "The red dashed line is the median replacement rate for that category")

##################################################################################################################

####################################################################################################
#### HEAT MAP ################################################################################

Rep_rates_df_subset$weeks_of_liquid_assets <- Rep_rates_df_subset$Liquid_Assets_Person / Rep_rates_df_subset$current_work_income
# Step 1: Create a subset of the data where partner_earnings + NonWageIncome > 0
subset_data <- Rep_rates_df_subset %>%
  filter(hours0_wk_partner_earnings + NonWageIncome >= 0)

subset_data <- subset_data  %>%
  filter(net_RR >= 0)

# Step 2: Define custom thresholds
partner_income_thresholds <- c(0, 500, 1000, 1500, 2000, Inf)
liquid_assets_thresholds <- c(-Inf, 1, 20, 40, 60, Inf)

# Step 3: Categorize the variables based on the thresholds
subset_data <- subset_data %>%
  mutate(
    partner_income_category = cut(hours0_wk_partner_earnings + NonWageIncome,
                                  breaks = partner_income_thresholds,
                                  include.lowest = TRUE,
                                  labels = c("0", "0-500", "500-1000", "1000-1500", "2000+")),
    liquid_assets_category = cut(weeks_of_liquid_assets,
                                 breaks = liquid_assets_thresholds,
                                 include.lowest = TRUE,
                                 labels = c("<1", "1-20", "21-40", "40-60", "61+"))
  )

# Step 4: Calculate median `net_RR` for each combination and filter for cells with at least 5 rows
heatmap_data <- subset_data %>%
  group_by(partner_income_category, liquid_assets_category) %>%
  summarise(
    median_net_RR = if (n() >= 5) round(median(net_RR * 100, na.rm = TRUE)) else NA,
    mean_net_RR = if (n() >= 5) round(mean(net_RR * 100, na.rm = TRUE)) else NA,
    count = n()
  ) %>%
  ungroup()

# Step 5: Create the heatmap
heatmap_plot <- ggplot(heatmap_data, aes(
  x = partner_income_category, 
  y = liquid_assets_category, 
  fill = median_net_RR)) +
  geom_tile(color = "white") + # Add gridlines
  geom_text(aes(label = ifelse(count >= 5, median_net_RR, "")), # Display rounded median values
            color = "black", size = 3) + 
  scale_fill_gradient(high = "#008080", low = "#b40000", na.value = "grey90", name = "Median net_RR") +
  labs_e61(
    title = "Average Replacement rates by private resources",
    x = "Weekly household income from other sources",
    y = "Weeks of Liquid Assets", 
    footnotes = "Weeks of liquid assets represent the number of weeks a household could
    fully replace lost work income using their liquid assets.
    Other private household income includes partner earnings and non-wage income. 
    Only households with non-negative income from other sources are included."
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(), legend.position = "none") 


# Save the plot
save_e61(paste0("heatmap_hour_min",hour_limit,".pdf"),  heatmap_plot, dim = list(width = 15, height = 10))


########################################################################################################
### BY AGE #################################################################################
# Create AGEEC_Group categories
Rep_rates_df_subset$AGEEC_Group <- cut(Rep_rates_df_subset$AGEEC,
                                  breaks = c(20, 30, 40, 50, Inf),
                                  labels = c("20-30", "30-40", "40-50", "50-55"),
                                  right = TRUE, include.lowest = TRUE)

# Normalize weights by AGEEC_Group
Rep_rates_df_subset[, normalized_weight := SIHPSWT / sum(SIHPSWT), by = AGEEC_Group]

# Calculate weighted means and medians for each AGEEC_Group
facet_summary_AGEEC <- Rep_rates_df_subset[, .(
  weighted_mean = sum(net_RR * normalized_weight) / sum(normalized_weight),
  weighted_median = {
    ord <- order(net_RR)
    x <- net_RR[ord]
    w <- normalized_weight[ord]
    cum_w <- cumsum(w) / sum(w)
    x[which(cum_w >= 0.5)[1]]
  }
), by = AGEEC_Group]

# Plot with facet_wrap, adding both mean and median lines
AGEECPanel <- ggplot(Rep_rates_df_subset, aes(x = net_RR * 100, weight = normalized_weight * 100)) +
  geom_histogram(binwidth = 5, fill = e61_tealdark) +
  facet_wrap(~AGEEC_Group, ncol = 2) +  # Adjust layout if needed
  geom_vline(data = facet_summary_AGEEC, aes(xintercept = weighted_mean * 100), 
             color = "purple", linetype = "dashed", size = 1) +
  geom_vline(data = facet_summary_AGEEC, aes(xintercept = weighted_median * 100), 
             color = e61_orangedark, linetype = "dashed", size = 1) +
  labs(
    title = "Replacement Rates by Age",
    x = "Replacement Rate (%)",
    y = "%"
  ) +
  ylim(0, 60)

# Save the plot
save_e61(paste0("AgeGroups_hour_min",hour_limit,".svg"), AGEECPanel, 
         footnotes = "The red dashed line is the median replacement rate, and the purple dashed line is the mean replacement rate for that category.")

save_e61(paste0("AgeGroups_hour_min",hour_limit,".pdf"), AGEECPanel, 
         footnotes = "The red dashed line is the median replacement rate, and the purple dashed line is the mean replacement rate for that category.")


#######################################################################################################
############################################################################################
###############################################################################################
### JOB LOSS ESTIMATES  ######################################################################
############################################################################################
#### Transition Probabilites 

TRANSITION_PROBABILITIES <- read_csv("TRANSITION_PROBABILITIES_yearly 1.csv")

TRANSITION_PROBABILITIES <- subset(TRANSITION_PROBABILITIES, year == 2019)



# Step 1: Create factor variables for sex, age, and state
Rep_rates_df_subset <- Rep_rates_df_subset %>%
  mutate(
    sex = factor(ifelse(SEXP == 1, "Males", ifelse(SEXP == 2, "Females", NA))),
    age = factor(ifelse(AGEEC >= 15 & AGEEC <= 24, "15-24", "25-54")),
    state = factor(case_when(
      STATEHEC == 1 ~ "New South Wales",
      STATEHEC == 2 ~ "Victoria",
      STATEHEC == 3 ~ "Queensland",
      STATEHEC == 4 ~ "South Australia",
      STATEHEC == 5 ~ "Western Australia",
      TRUE ~ NA_character_  # To handle other or missing STATEHEC values
    ))
  )

# Step 2: Merge with TRANSITION_PROBABILITIES by 'age', 'sex', and 'state'
Rep_rates_df_subset_merged <- Rep_rates_df_subset %>%
  left_join(TRANSITION_PROBABILITIES, by = c("age", "sex", "state"))


Rep_rates_df_subset_merged <- subset(Rep_rates_df_subset_merged, Rep_rates_df_subset$STATEHEC < 7)

# Assign EU_category
Rep_rates_df_subset_merged <- Rep_rates_df_subset_merged %>%
  mutate(
    EU_category = factor(
      case_when(
        EU < 0.0795 ~ "Q1",
        EU >= 0.0795 & EU < 0.0842 ~ "Q2",
        EU >= 0.0842 & EU < 0.0951 ~ "Q3",
        EU >= 0.0951 ~ "Q4",
        TRUE ~ NA_character_  # Handle missing or invalid values
      ),
      levels = c("Q1", "Q2", "Q3", "Q4")  # Set factor levels
    )
  )

# Filter out rows with missing EU_category
Rep_rates_df_subset_merged <- subset(Rep_rates_df_subset_merged, !is.na(EU_category))

# Normalize weights by EU_category
Rep_rates_df_subset_merged[, normalized_weight := SIHPSWT / sum(SIHPSWT), by = EU_category]

# Calculate weighted means and medians for each EU_category
facet_summary_EU <- Rep_rates_df_subset_merged[, .(
  weighted_mean = sum(net_RR * normalized_weight) / sum(normalized_weight),
  weighted_median = {
    ord <- order(net_RR)
    x <- net_RR[ord]
    w <- normalized_weight[ord]
    cum_w <- cumsum(w) / sum(w)
    x[which(cum_w >= 0.5)[1]]
  }
), by = EU_category]

# Plot with facet_wrap, adding both mean and median lines
EUPanel <- ggplot(Rep_rates_df_subset_merged, aes(x = net_RR * 100, weight = normalized_weight * 100)) +
  geom_histogram(binwidth = 5, fill = e61_tealdark, col = "black") +
  facet_wrap(~EU_category, ncol = 4) + 
  geom_vline(data = facet_summary_EU, aes(xintercept = weighted_median * 100), 
             color = e61_orangedark, linetype = "dashed", size = 1) +
  labs_e61(
    subtitle = "Replacement Rates by the probability of job loss in the next year (Quartiles) ",
    x = "Replacement Rate (%)",
    y = "%", 
    footnotes = "The red dashed line is the median replacement rate for each group."
  ) 

# Save the plot
save_e61(paste0("EUGroups_hour_min",hour_limit,".svg"), EUPanel, 
         footnotes = "The red dashed line is the median replacement rate,
         and the purple dashed line is the mean replacement rate for that category.")

save_e61(paste0("EUGroups_hour_min",hour_limit,".pdf"), EUPanel, footnotes =  "The red dashed line is the median replacement rate for that category")


#### Just those on the benefit 

Rep_rates_df_subset_merged2 <- subset(Rep_rates_df_subset_merged, Rep_rates_df_subset_merged$hours0_taxable_benefit > 0)
# Normalize weights by EU_cateogry
Rep_rates_df_subset_merged2[, normalized_weight := SIHPSWT / sum(SIHPSWT), by = EU_category]

# Calculate weighted medians for each EU_cateogry
facet_summary_EU <- Rep_rates_df_subset_merged2[, .(
  weighted_median = {
    ord <- order(net_RR)
    x <- net_RR[ord]
    w <- normalized_weight[ord]
    cum_w <- cumsum(w) / sum(w)
    x[which(cum_w >= 0.5)[1]]
  }
), by = EU_category]





EUPanel2 <- ggplot(Rep_rates_df_subset_merged2, aes(x = net_RR * 100, weight = normalized_weight * 100)) +
  geom_histogram(binwidth = 5, fill = e61_tealdark) +
  facet_wrap(~EU_category, ncol = 4) +  # Adjust layout if needed
  geom_vline(data = facet_summary_EU, aes(xintercept = weighted_median * 100), color = e61_orangedark, linetype = "dashed") +
  labs(title = "Replacement Rates by Job Loss Probability Quartile",
       subtitle = "Among those eligble for transfer payments upon job loss",
       x = "Replacement Rate (%)",
       y = "%") + xlim(-5,100)


save_e61(paste0("EUGroups2_hour_min",hour_limit,".svg"), EUPanel2, footnotes =  "The red dashed line is the median replacement rate for that category")
#### Summary states of the groups 

Rep_rates_df_subset[,hhld_inc := current_gross_income + current_wk_partner_earnings][,OECD_scale := 1 + 0.5*partnered + 0.3*Numb_dep][,eqv_hhld_gross_inc := hhld_inc/OECD_scale]

Rep_rates_df_subset[,group := fcase(net_RR == 0,1,
                      net_RR < 0.25, 2,
                      net_RR < 0.5, 3,
                      net_RR < 0.75, 4,
                      default = 5)]
# Summary stats
Rep_rates_df_subset[,.(income = mean(current_gross_income),cap_inc = mean(NonWageIncome),existing_benefits = mean(current_taxable_benefit),age=mean(AGEEC),hours_worked = mean(hours),kids = mean(Numb_dep),hhld_eq_ginc = mean(eqv_hhld_gross_inc),L_Asset = mean(LiquidAssets_Household),.N),by=.(group)][order(group)]

# Summary stats "full time workers"
Rep_rates_df_subset[hours >= 32,.(income = mean(current_gross_income),cap_inc = mean(NonWageIncome),existing_benefits = mean(current_taxable_benefit),age=mean(AGEEC),hours_worked = mean(hours),kids = mean(Numb_dep),hhld_eq_ginc = mean(eqv_hhld_gross_inc),L_Asset = mean(LiquidAssets_Household),.N),by=.(group)][order(group)]

# Create additional variables
Rep_rates_df_subset[, hhld_inc := current_gross_income + current_wk_partner_earnings]
Rep_rates_df_subset[, OECD_scale := 1 + 0.5 * partnered + 0.3 * Numb_dep]
Rep_rates_df_subset[, eqv_hhld_gross_inc := hhld_inc / OECD_scale]

# Categorize net_RR into groups
Rep_rates_df_subset[, group := fcase(
  net_RR == 0, 1,
  net_RR < 0.25, 2,
  net_RR < 0.5, 3,
  net_RR < 0.75, 4,
  default = 5
)]

# Normalize weights
Rep_rates_df_subset[, normalized_weight := SIHPSWT / sum(SIHPSWT)]

# Summary stats with weighted means
summary_stats <- Rep_rates_df_subset[, .(
  income = sum(current_gross_income * normalized_weight) / sum(normalized_weight),
  cap_inc = sum(NonWageIncome * normalized_weight) / sum(normalized_weight),
  existing_benefits = sum(current_taxable_benefit * normalized_weight) / sum(normalized_weight),
  age = sum(AGEEC * normalized_weight) / sum(normalized_weight),
  hours_worked = sum(hours * normalized_weight) / sum(normalized_weight),
  kids = sum(Numb_dep * normalized_weight) / sum(normalized_weight),
  hhld_eq_ginc = sum(eqv_hhld_gross_inc * normalized_weight) / sum(normalized_weight),
  L_Asset = sum(LiquidAssets_Household * normalized_weight) / sum(normalized_weight),
  .N
), by = .(group)][order(group)]

# Summary stats for "full-time workers" (hours >= 32) with weighted means
summary_stats_full_time <- Rep_rates_df_subset[hours >= 32, .(
  income = sum(current_gross_income * normalized_weight) / sum(normalized_weight),
  cap_inc = sum(NonWageIncome * normalized_weight) / sum(normalized_weight),
  existing_benefits = sum(current_taxable_benefit * normalized_weight) / sum(normalized_weight),
  age = sum(AGEEC * normalized_weight) / sum(normalized_weight),
  hours_worked = sum(hours * normalized_weight) / sum(normalized_weight),
  kids = sum(Numb_dep * normalized_weight) / sum(normalized_weight),
  hhld_eq_ginc = sum(eqv_hhld_gross_inc * normalized_weight) / sum(normalized_weight),
  L_Asset = sum(LiquidAssets_Household * normalized_weight) / sum(normalized_weight),
  .N
), by = .(group)][order(group)]

# Print summary tables
print("Summary Statistics - All Workers")
print(summary_stats)

print("Summary Statistics - Full-Time Workers")
print(summary_stats_full_time) 

#############################################################################################3
### ELIGIBILITY ANALYSIS ##################################################################
###########################################################################################
##############################################################################################3
##########################################################################################

# Subset data
#Rep_rates_df_subset <- subset(Rep_rates_df, hours > 5) # This should be unnecessary - check code above.
Rep_rates_df_subset[, normalized_weight := SIHPSWT / sum(SIHPSWT)]

# Create the new factor variable
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

# Set the factor levels
Rep_rates_df_subset[, eligibility_status := factor(eligibility_status, 
                                              levels = c("Ineligible", "Benefit Eligible", 
                                                         "FTB Eligible", "Benefit + FTB Eligible"))]

# Create a subset without "Ineligible"
Rep_rates_df_subset_no_ineligible <- Rep_rates_df_subset[eligibility_status != "Ineligible"]

# Calculate the weighted mean and median for all data
mean_net_RR_3 <- sum(Rep_rates_df_subset$net_RR * Rep_rates_df_subset$SIHPSWT) / 
  sum(Rep_rates_df_subset$SIHPSWT)

sorted_data <- Rep_rates_df_subset[order(net_RR)]
cumulative_weights <- cumsum(sorted_data$normalized_weight)
median_index <- which(cumulative_weights >= 0.5)[1]
median_net_RR_3 <- sorted_data$net_RR[median_index]

# Calculate the weighted mean and median for data without "Ineligible"
mean_net_RR_no_ineligible <- sum(Rep_rates_df_subset_no_ineligible$net_RR * Rep_rates_df_subset_no_ineligible$SIHPSWT) / 
  sum(Rep_rates_df_subset_no_ineligible$SIHPSWT)

sorted_data_no_ineligible <- Rep_rates_df_subset_no_ineligible[order(net_RR)]
cumulative_weights_no_ineligible <- cumsum(sorted_data_no_ineligible$normalized_weight)
median_index_no_ineligible <- which(cumulative_weights_no_ineligible >= 0.5)[1]
median_net_RR_no_ineligible <- sorted_data_no_ineligible$net_RR[median_index_no_ineligible]

# Combine data into a single table for plotting
Rep_rates_df_subset[, panel := "All Categories"]
Rep_rates_df_subset_no_ineligible[, panel := "Excluding Ineligible"]
combined_data <- rbind(Rep_rates_df_subset, Rep_rates_df_subset_no_ineligible)

# Plot for "All Categories"
RR1_all <- ggplot(Rep_rates_df_subset, aes(x = net_RR * 100, weight = normalized_weight * 100, fill = eligibility_status)) +
  geom_histogram(binwidth = 1, position = "stack") +
  scale_fill_manual(
    values = c(
      "Ineligible" = "#999999", 
      "Benefit Eligible" = "#ED7F00", 
      "FTB Eligible" = "#008080", 
      "Benefit + FTB Eligible" = "#10485E"
    )
  ) +
  labs_e61(
    subtitle = "Distribution of Replacement Rates",
    x = "Replacement Rate (%)",
    y = "%", sources = c("e61", "ABS"), 
    fill = "Eligibility Status"
  ) +
  scale_y_continuous_e61(limits = c(0, 35,5)) + plot_label(c("Not Eligible for \nAny Transfers"), c(5), c(20), c("#999999"))


save_e61(paste0("DistributionEligibility_hour_min",hour_limit,".pdf"), RR1_all)

#################################################################################################

# Plot for "Excluding Ineligible"
RR1_no_ineligible <- ggplot(Rep_rates_df_subset_no_ineligible, aes(x = net_RR * 100,
                                                                   weight = normalized_weight * 100,
                                                                   fill = eligibility_status)) +
  geom_histogram(binwidth = 2, position = "stack") +
  scale_fill_manual(
    values = c(
      "Benefit Eligible" = "#ED7F00", 
      "FTB Eligible" = "#008080", 
      "Benefit + FTB Eligible" = "#10485E"
    )
  ) +
  labs_e61(
    subtitle = "A. Distribution of Replacement Rates for eligible recipients*",
    x = "Replacement Rate (%)",
    y = "%",
    fill = "Eligibility Status", footnotes = "Eligible recipients are those who would receive either taxable benefits or FTB."
  ) + plot_label(c("Benefit \n(JSP or PP)\n Only", "FTB Only", "Both"),
                 c(30, 5, 80), c(1.5, 0.5, 0.4), c("white", "white", "#10485E" ))

# Save the second plot
save_e61(paste0("Distribution_xineligible_hour_min",hour_limit,".pdf"), RR1_no_ineligible, IncomeGroupPanel)

ggplot(Rep_rates_df_subset_no_ineligible, aes(x = net_RR * 100,
                                              weight = normalized_weight * 100,
                                              fill = eligibility_status)) +
  geom_histogram(binwidth = 2, position = "stack") +
  scale_fill_manual(
    values = c(
      "Benefit Eligible" = "#ED7F00", 
      "FTB Eligible" = "#008080", 
      "Benefit + FTB Eligible" = "#10485E"
    )
  ) +
  labs_e61(
    subtitle = "Distribution of Replacement Rates for eligible recipients*",
    x = "Replacement Rate (%)",
    y = "%",
    sources = c("e61","ABS"),
    fill = "Eligibility Status", footnotes = "Eligible recipients are those who would receive either taxable benefits or FTB."
  ) + plot_label(c("Benefit \n(JSP or PP)\n Only", "FTB Only", "Both"),
                 c(23, 5, 70), c(1.9, 0.7, 0.7), c("white", "white", "#10485E" )) +
  scale_y_continuous_e61(limits=c(0,3.5,0.5))

if (hour_limit == 5){
  print(ggplot(Rep_rates_df_subset_no_ineligible, aes(x = net_RR * 100,
                                                      weight = normalized_weight * 100,
                                                      fill = eligibility_status)) +
          geom_histogram(binwidth = 2, position = "stack") +
          scale_fill_manual(
            values = c(
              "Benefit Eligible" = "#ED7F00", 
              "FTB Eligible" = "#008080", 
              "Benefit + FTB Eligible" = "#10485E"
            )
          ) +
          labs_e61(
            subtitle = "Distribution of Replacement Rates for eligible recipients*",
            x = "Replacement Rate (%)",
            y = "%",
            sources = c("e61","ABS"),
            fill = "Eligibility Status", footnotes = "Eligible recipients are those who would receive either taxable benefits or FTB."
          ) + plot_label(c("Benefit Only", "FTB Only", "Both"),
                         c(23, 5, 75), c(1.7, 0.7, 0.7), c("white", "white", "#10485E" )) +
          scale_y_continuous_e61(limits=c(0,3.5,0.5)))
}

save_e61(paste0("Distribution_xineligible_only_hour_min",hour_limit,".pdf"))

#################################################################################################
### VENN DIAGRAM 

library(VennDiagram)
library(grid)
library(data.table)

# Ensure the data is a data.table
setDT(Rep_rates_df_subset)

# Total weight for proportions
total_weight <- sum(Rep_rates_df_subset$SIHPSWT)

# Calculate percentages for each unique region
proportions <- list(
  "Asset Test Only" = sum(Rep_rates_df_subset[Asset_test_flag == 1 & partner_test_flag == 0 & nonworkincome_income_test == 0]$SIHPSWT) / total_weight * 100,
  "Partner Test Only" = sum(Rep_rates_df_subset[Asset_test_flag == 0 & partner_test_flag == 1 & nonworkincome_income_test == 0]$SIHPSWT) / total_weight * 100,
  "Non-Work Income Test Only" = sum(Rep_rates_df_subset[Asset_test_flag == 0 & partner_test_flag == 0 & nonworkincome_income_test == 1]$SIHPSWT) / total_weight * 100,
  "Asset + Partner Test" = sum(Rep_rates_df_subset[Asset_test_flag == 1 & partner_test_flag == 1 & nonworkincome_income_test == 0]$SIHPSWT) / total_weight * 100,
  "Asset + Non-Work Income Test" = sum(Rep_rates_df_subset[Asset_test_flag == 1 & partner_test_flag == 0 & nonworkincome_income_test == 1]$SIHPSWT) / total_weight * 100,
  "Partner + Non-Work Income Test" = sum(Rep_rates_df_subset[Asset_test_flag == 0 & partner_test_flag == 1 & nonworkincome_income_test == 1]$SIHPSWT) / total_weight * 100,
  "All Tests" = sum(Rep_rates_df_subset[Asset_test_flag == 1 & partner_test_flag == 1 & nonworkincome_income_test == 1]$SIHPSWT) / total_weight * 100,
  "Outside Venn" = sum(Rep_rates_df_subset[Asset_test_flag == 0 & partner_test_flag == 0 & nonworkincome_income_test == 0]$SIHPSWT) / total_weight * 100
)

# Create the Venn Diagram with percentages
venn <- venn.diagram(
  x = list(
    "Asset Test" = which(Rep_rates_df_subset$Asset_test_flag == 1),
    "Partner Test" = which(Rep_rates_df_subset$partner_test_flag == 1),
    "Income Test (from non-work income)" = which(Rep_rates_df_subset$nonworkincome_income_test == 1)
  ),
  filename = NULL,
  fill = c("#ED7F00", "#008080", "#6A5ACD"),
  alpha = 0.5,
  cat.col = c("#ED7F00", "#008080", "#6A5ACD"),
  cex = 0, # Disable default counts
  cat.cex = 1.2,
  lwd = 2,
  main = "Job Seeker and Parenting Payment Eligibility Tests"
)

# Draw the Venn Diagram
grid.newpage()
grid.draw(venn)

# Annotate percentages inside the Venn diagram
grid.text(label = paste0(round(proportions[["Asset Test Only"]], 2), "%"), 
          x = 0.2, y = 0.7, gp = gpar(col = "black", fontsize = 20))
grid.text(label = paste0(round(proportions[["Partner Test Only"]], 2), "%"), 
          x = 0.8, y = 0.7, gp = gpar(col = "black", fontsize = 20))
grid.text(label = paste0(round(proportions[["Non-Work Income Test Only"]], 2), "%"), 
          x = 0.5, y = 0.15, gp = gpar(col = "black", fontsize = 20))
grid.text(label = paste0(round(proportions[["Asset + Partner Test"]], 2), "%"), 
          x = 0.5, y = 0.75, gp = gpar(col = "black", fontsize = 20))
grid.text(label = paste0(round(proportions[["Asset + Non-Work Income Test"]], 2), "%"), 
          x = 0.3, y = 0.4, gp = gpar(col = "black", fontsize = 20))
grid.text(label = paste0(round(proportions[["Partner + Non-Work Income Test"]], 2), "%"), 
          x = 0.7, y = 0.4, gp = gpar(col = "black", fontsize = 20))
grid.text(label = paste0(round(proportions[["All Tests"]], 2), "%"), 
          x = 0.5, y = 0.5, gp = gpar(col = "black", fontsize = 20))

# Annotate the "Outside Venn" percentage below the diagram
grid.text(label = paste0("Eligible: ", round(proportions[["Outside Venn"]], 2), "%"), 
          x = 0.15, y = 0.1, gp = gpar(col = "black", fontsize = 20, fontface = "bold"))


###########################################################################################
## POVERTY ANALYSIS ########################################################################
############################################################################################
#############################################################################################
##############################################################################################

### Add in the "household" weights - were we halve the weight for partnered individuals, as they are recorded twice in the data.
## As we don't have partner hours this is a bit messy. Just have to use current_work_income_partner and set greater that 5*hour_limit

Rep_rates_df_subset[,hhld_wgt := fifelse(partnered == 1 & current_work_income_partner > 20*hour_limit,SIHPSWT/2,SIHPSWT)]

Rep_rates_df_subset[,.(hhld_wgt,SIHPSWT)]

#Rep_rates_df_subset <- subset(Rep_rates_df, Rep_rates_df$hours > 5)
# Create the dataframe
poverty_lines <- data.frame(
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
## Adjust below to non-work line from MI: https://melbourneinstitute.unimelb.edu.au/__data/assets/pdf_file/0006/4961229/Poverty-Lines-Australia-March-Quarter-2024.pdf
poverty_lines_nonwork <- data.frame(
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


Rep_rates_df_subset$weeks_of_liquid_assets <- Rep_rates_df_subset$Liquid_Assets_Person / Rep_rates_df_subset$current_work_income
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

# Step 2: Perform a left join with poverty_lines
Rep_rates_df_subset <- Rep_rates_df_subset %>%
  left_join(poverty_lines, by = "Income_Unit")

Rep_rates_df_subset <- Rep_rates_df_subset %>%
  left_join(poverty_lines_nonwork, by = "Income_Unit")


Rep_rates_df_subset$hours0_wk_partner_earnings <- ifelse(is.na(Rep_rates_df_subset$hours0_wk_partner_earnings), Rep_rates_df_subset$hours0_gross_income_partner - Rep_rates_df_subset$hours0_income_tax_partner + Rep_rates_df_subset$hours0_RA_partner + Rep_rates_df_subset$hours0_ES_partner, Rep_rates_df_subset$hours0_wk_partner_earnings )

Rep_rates_df_subset$hhincome <- Rep_rates_df_subset$hours0_net_income + Rep_rates_df_subset$NonWageIncome + Rep_rates_df_subset$hours0_wk_partner_earnings

Rep_rates_df_subset$hhincome <- ifelse(Rep_rates_df_subset$hhincome < 0, 0, Rep_rates_df_subset$hhincome)

### Poverty definition -----  XXXXXXXXXXXXX
# This is the poverty rate when using the "including housing" line for everyone.
#Rep_rates_df_subset$poverty_gap <- Rep_rates_df_subset$hhincome - Rep_rates_df_subset$Including_Housing

# Poverty gap with variable line - attribute half of the mortgage to principle repayments, but also don't attribute more of a cost than the same imputed value
Rep_rates_df_subset[,poverty_gap := fifelse(Home_owner == 1,hhincome - min(Weekly_Mortgage_Repayments*0.5, (Including_Housing - Other_than_Housing)) - Other_than_Housing,hhincome - Including_Housing)]

# Excluding mortgage line
#Rep_rates_df_subset[,poverty_gap := fifelse(Home_owner == 1,hhincome - Other_than_Housing,hhincome - Including_Housing)]

# If we assume that the primary recipient is "not in workforce" - MI assumes they are actively searching for work so use higher line.
#Rep_rates_df_subset[,poverty_gap := fifelse(current_work_income_partner > 1,hhincome - Including_Housing,hhincome - Including_Housing_nonwork)]


##############################



Rep_rates_df_subset <- Rep_rates_df_subset %>%
  mutate(liquid_assets_category = cut(weeks_of_liquid_assets,
                                 breaks = liquid_assets_thresholds,
                                 include.lowest = TRUE,
                                 labels = c("<1", "1-20", "21-40", "40-60", "61+"))
  )



# Normalize weights within each category of weeks_of_liquid_assets
Rep_rates_df_subset <- Rep_rates_df_subset %>%
  group_by(liquid_assets_category) %>%
  mutate(normalized_weight_hhld_la = hhld_wgt / sum(hhld_wgt)) %>%
  ungroup()

subset <- subset(Rep_rates_df_subset, !is.na(Rep_rates_df_subset$poverty_gap))

# Plot the distribution
poverty_gap_plot <- ggplot(subset, aes(x = poverty_gap, weight = normalized_weight_hhld_la, col = as.factor(liquid_assets_category))) +
  geom_density(alpha = 0.1) +  
  labs(
    title = "Distribution of Poverty Gap by Weeks of Liquid Assets",
    x = "Poverty Gap",
    y = "Weighted Count"
  ) + xlim(-1500,1500)


poverty_gap_plot <- ggplot(Rep_rates_df_subset, aes(x = poverty_gap, weight = normalized_weight_hhld_la * 100,
                                                    fill = as.factor(liquid_assets_category))) +
  geom_histogram(binwidth = 100, alpha = 0.8, color = "black") +  # Adjust binwidth as needed
  facet_wrap(~as.factor(liquid_assets_category), scales = "free_y", ncol = 3) +  # Facet by weeks_of_liquid_assets
  labs(
    title = "Distribution of Poverty Gap by Weeks of Liquid Assets",
    x = "Poverty Gap",
    y = "%",
    fill = "Weeks of Liquid Assets"
  ) + scale_x_continuous_e61(limits = c(-1500,2000,500)) + geom_vline(xintercept = 0, linetype = "dashed")

setDT(Rep_rates_df_subset)

# Create the new factor variable
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

# Set the factor levels
Rep_rates_df_subset[, eligibility_status := factor(eligibility_status, 
                                              levels = c("Ineligible", "Benefit Eligible", 
                                                         "FTB Eligible", "Benefit + FTB Eligible"))]

Rep_rates_df_subset <- Rep_rates_df_subset %>%
  group_by(eligibility_status) %>%
  mutate(normalized_weight_hhld_es = hhld_wgt / sum(hhld_wgt)) %>%
  ungroup()


# Calculate the proportion below 0 for each eligibility_status - replace with datatable code
# facet_annotations <- Rep_rates_df_subset %>%
#   group_by(eligibility_status) %>%
#   summarize(
#     proportion_below_zero = sum((poverty_gap < 0) * normalized_weight) / sum(normalized_weight) * 100
#   ) %>%
#   ungroup()

setDT(Rep_rates_df_subset)

facet_annotations <- Rep_rates_df_subset[, .(
  proportion_below_zero = sum((poverty_gap < 0) * normalized_weight_hhld_es) / sum(normalized_weight_hhld_es) * 100
), by = .(eligibility_status)]

Rep_rates_df_subset$amount_of_liquid <- ifelse(Rep_rates_df_subset$weeks_of_liquid_assets > 5, 1, 0)

# Add annotations to the plot
poverty_gap_plot2 <- ggplot(Rep_rates_df_subset, aes(x = poverty_gap, weight = normalized_weight_hhld_es * 100,
                                                fill = as.factor(amount_of_liquid))) +
  geom_histogram(binwidth = 100, alpha = 0.8, color = "black") +  # Adjust binwidth as needed
  facet_wrap(~as.factor(eligibility_status), scales = "free_y", ncol = 2) +  # Facet by weeks_of_liquid_assets
  labs(
    title = "After Replacement Poverty Gap, by Benefit Eligibility",
    x = "Poverty Gap",
    y = "%",
    fill = "Weeks of Liquid Assets"
  ) +
  scale_x_continuous(limits = c(-1500, 2000), breaks = seq(-1500, 2000, 500)) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(
    data = facet_annotations,
    aes(
      x = 1500,  # Position the label on the left side of each facet
      y = 30,     # Adjust the vertical position as needed
      label = paste0("In Poverty:\n ", round(proportion_below_zero, 1), "%")
    ),
    inherit.aes = FALSE,  # Prevent inheriting aesthetics from the main plot
    size = 4, color = "black"
  ) + plot_label("<5 weeks of \n Liquid Assets", -1500, 30, e61_teallight) + 
  plot_label("5+ weeks of \n Liquid Assets", -1500, 24, e61_tealdark)

print(poverty_gap_plot)

save_e61(paste0("Poverty_gap1_hour_min",hour_limit,".svg"), poverty_gap_plot)

save_e61(paste0("Poverty_gap2_hour_min",hour_limit,".svg"), poverty_gap_plot2)



# Calculate the overall proportion below 0 - replace tidyverse with datatable code
# overall_proportion <- Rep_rates_df_subset %>%
#   summarize(
#     proportion_below_zero = sum((poverty_gap < 0) * normalized_weight) / sum(normalized_weight) * 100
#   ) %>%
#   pull(proportion_below_zero)


overall_proportion <- Rep_rates_df_subset[, sum((poverty_gap < 0) * normalized_weight_hhld_es) / sum(normalized_weight_hhld_es) * 100]

Rep_rates_df_subset <- Rep_rates_df_subset %>%
  mutate(normalized_weight_hhld = hhld_wgt / sum(hhld_wgt)) %>%
  ungroup()

# Calculate the value
total_normalized_weight <- Rep_rates_df_subset[amount_of_liquid == 0 & poverty_gap < 0, 
                                          sum(normalized_weight_hhld * 100, na.rm = TRUE)]
total_normalized_weight



# Create the single-panel chart
poverty_gap_plot_single <- ggplot(Rep_rates_df_subset, aes(x = poverty_gap, weight = normalized_weight_hhld * 100,
                                                           fill = as.factor(amount_of_liquid))) +
  geom_histogram(binwidth = 100, alpha = 0.8, color = "black", position = "stack") +  # Stacked histogram +  # Single color for all bars
  labs(
    subtitle = "After Job Loss Poverty Gap",
    x = "Poverty Gap",
    y = "%", 
    footnotes = c("Poverty gap is estimated by the Henderson Poverty lines, at the household level.")
  ) +
  scale_x_continuous(limits = c(-1500, 2000), breaks = seq(-1500, 2000, 500)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
    geom_text(
      data = facet_annotations,
      aes(
        x = 1200,  # Position the label on the left side of each facet
        y = 15,     # Adjust the vertical position as needed
        label = paste0("In Poverty:\n ", round(overall_proportion, 1), "%", 
                       "\n& <5 weeks of liquid assets:\n", round(total_normalized_weight, 2), "%")
      ),
      inherit.aes = FALSE,  # Prevent inheriting aesthetics from the main plot
      size = 3, color = "black"
    ) + 
  plot_label(c("More than \n5 weeks of \nliquid assets", "Less than \n5 weeks of \nliquid assets"), 
             c(-1250, -1250), c(15, 10), c(e61_tealdark, e61_teallight)) 

# Display the plot
print(poverty_gap_plot_single)


save_e61(paste0("Poverty_gap_overall_hour_min",hour_limit,".pdf"), poverty_gap_plot_single )


###########################################################################################


low_liquid_assets <- subset(Rep_rates_df_subset, Rep_rates_df_subset$weeks_of_liquid_assets < 5)

low_liquid_assets <- low_liquid_assets %>%
  mutate(normalized_weight_hhld = hhld_wgt / sum(hhld_wgt)) %>%
  ungroup()

setDT(low_liquid_assets)

low_liquid_assets[,.(median_la = quantile(net_RR, probs = 0.5, type = 7, na.rm = TRUE, weights = normalized_weight_hhld))]

# Replace with data table code
# facet_annotations <- low_liquid_assets %>%
#   group_by(eligibility_status) %>%
#   summarize(
#     proportion_below_zero = sum((poverty_gap < 0) * normalized_weight) / sum(normalized_weight) * 100
#   ) %>%
#   ungroup()

facet_annotations <- low_liquid_assets[, .(
  proportion_below_zero = sum((poverty_gap < 0) * normalized_weight_hhld) / sum(normalized_weight_hhld) * 100
), by = .(eligibility_status)]


# Create the single-panel chart
poverty_gap_plot_single2 <- ggplot(low_liquid_assets, aes(x = poverty_gap, weight = normalized_weight_hhld * 100,
                                                          fill = as.factor(eligibility_status))) +
  geom_histogram(binwidth = 100, alpha = 0.8, color = "black", position = "stack") +  # Stacked histogram +  # Single color for all bars
  labs(
    title = "After Replacement Poverty Gap",
    x = "Poverty Gap",
    y = "%"
  ) +
  scale_x_continuous(limits = c(-1500, 2000), breaks = seq(-1500, 2000, 500)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(
    data = facet_annotations,
    aes(
      x = 1500,  # Position the label on the left side of each facet
      y = 15,     # Adjust the vertical position as needed
      label = paste0("In Poverty:\n ", round(overall_proportion, 1), "%")
    ),
    inherit.aes = FALSE,  # Prevent inheriting aesthetics from the main plot
    size = 3, color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Ineligible" = "#999999", 
      "Benefit Eligible" = "#ED7F00", 
      "FTB Eligible" = "#008080", 
      "Benefit + FTB Eligible" = "#10485E"
    )
  ) + plot_label(c("Ineligible", "Benefit Only", "FTB Only", "Benefit + JSP"), 
                 c(-1500, -1500, -1500, -1500), 
                 c(18, 16, 14, 12), c(
                   "#999999", "#ED7F00", "#008080", "#10485E"
                 ))

# Display the plot
print(poverty_gap_plot_single)

Rep_rates_df_subset$weeks_of_all_assets <- (Rep_rates_df_subset$Liquid_Assets_Person + Rep_rates_df_subset$Illiquid_Assets) / 
  Rep_rates_df_subset$current_work_income

save_e61(paste0("Poverty_gap_overall2_hour_min",hour_limit,".pdf"), poverty_gap_plot_single2 )



##########################################################################################
############# POVERTY GAP CHANGING BY INCREASING THE TAXABLE BENEFIT ######################
#############################################################################################
setDT(Rep_rates_df_subset)
# Normalize weights
# Rep_rates_df_subset[, normalized_weight := SIHPSWT / sum(SIHPSWT)]
##############################################################################################
################################################################################################

# Create a new categorical variable for poverty with three categories
Rep_rates_df_subset[, poverty := fcase(
  poverty_gap < 0 & amount_of_liquid == 1, "In Poverty & Liquid Assets",
  poverty_gap < 0 & amount_of_liquid == 0, "In Poverty & No Liquid Assets",
  poverty_gap >= 0, "Not in Poverty"
)]

summary(Rep_rates_df_subset[amount_of_liquid == 0]$weeks_of_liquid_assets)

# Calculate normalized_weight for everyone
# Rep_rates_df_subset[, normalized_weight_hhld := hhld_wgt / sum(hhld_wgt)]

# Calculate the weighted median for each poverty category
weighted_medians <- Rep_rates_df_subset[, .(
  weighted_median = quantile(net_RR, probs = 0.5, type = 7, na.rm = TRUE, weights = normalized_weight_hhld)
), by = poverty]

Rep_rates_df_subset[poverty_gap < 0 & amount_of_liquid == 1 & net_RR >= 0.5,sum(normalized_weight_hhld)]/Rep_rates_df_subset[poverty_gap < 0 & amount_of_liquid == 1,sum(normalized_weight_hhld)]

weighted_medians_taxable_receipt <- Rep_rates_df_subset[hours0_taxable_benefit > 0, .(
  weighted_median = quantile(net_RR, probs = 0.5, type = 7, na.rm = TRUE, weights = normalized_weight_hhld)
), by = poverty]

# Create the histogram
histogram_plot <- ggplot(Rep_rates_df_subset, aes(x = net_RR * 100, weight = normalized_weight_hhld * 100, fill = poverty)) +
  geom_histogram(binwidth = 5, position = "stack", color = "black", alpha = 0.8) +
  geom_vline(data = weighted_medians, aes(xintercept = weighted_median * 100, color = poverty),
             linetype = "dashed", size = 1) +
  labs_e61(
    subtitle = "B. Replacement Rates, by Poverty Status",
    x = "Net Replacement Rate",
    y = "%",
    fill = "Poverty Category",
    color = "Poverty Category", 
    footnotes = ""
  ) +
  scale_fill_manual(
    values = c(
      "In Poverty & Liquid Assets" = e61_tealdark,
      "In Poverty & No Liquid Assets" = e61_orangedark,
      "Not in Poverty" = e61_maroondark
    )
  ) +
  scale_color_manual(
    values = c(
      "In Poverty & Liquid Assets" = e61_tealdark,
      "In Poverty & No Liquid Assets" = e61_orangedark,
      "Not in Poverty" = e61_maroondark
    )
  ) + plot_label(
    c("Not in Poverty", 
      "In Poverty After \nJob Loss \n& >5 weeks of \nLiquid Assets", 
      "In Poverty After \nJob Loss \n& <5 weeks of \nLiquid Assets"), 
    c(60, 60, 60), 
    c(33, 27, 17.5), c(e61_maroondark, e61_tealdark, e61_orangedark ), size = 3
  )

# Print the plot
#print(histogram_plot)

#########################################################################################


# Rep_rates_df_subset <- Rep_rates_df_subset %>%
#   mutate(normalized_weight = SIHPSWT / sum(SIHPSWT)) %>%
#   ungroup() # Why do we keep reconstructing the same weights - if we are doing it for different groups we should create new variables to minimise accidental errors.

# Calculate the proportions
proportions <- data.table(
  category = c(
    "In Poverty \nAfter Job Loss",
    "& Passes the \nAsset Test",
    "& <5 weeks \nof Liquid \nAssets",
    "& Main \npayment is \nJobSeeker"
  ),
  proportion = c(
    Rep_rates_df_subset[poverty_gap < 0, sum(normalized_weight_hhld)] / sum(Rep_rates_df_subset$normalized_weight_hhld) * 100,
    Rep_rates_df_subset[poverty_gap < 0 & Asset_test_flag == 0,
                        sum(normalized_weight_hhld)] / sum(Rep_rates_df_subset$normalized_weight_hhld) * 100,
    Rep_rates_df_subset[poverty_gap < 0 & weeks_of_liquid_assets < 5 & Asset_test_flag == 0,
                        sum(normalized_weight_hhld) ] / sum(Rep_rates_df_subset$normalized_weight_hhld) * 100,
    Rep_rates_df_subset[poverty_gap < 0 & weeks_of_liquid_assets < 5 & eligibility_status == "Benefit Eligible" & Asset_test_flag == 0,
                        sum(normalized_weight_hhld)] / sum(Rep_rates_df_subset$normalized_weight_hhld) * 100
  )
)

# Set factor levels to maintain order on x-axis
proportions[, category := factor(category, levels = c(
  "In Poverty \nAfter Job Loss",
  "& Passes the \nAsset Test",
  "& <5 weeks \nof Liquid \nAssets",
  "& Main \npayment is \nJobSeeker"
))]

# Create the bar chart
bar_chart <- ggplot(proportions, aes(x = category, y = proportion, fill = category)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  labs_e61(
    subtitle = "A. Households in poverty after job loss",
    x = "",
    y = "%",
    fill = "Category"
  ) + scale_y_continuous_e61(limits = c(0, 45, 10))

# Print the plot
print(bar_chart)
print(histogram_plot)

# Moved this from above as plot not available until this point
save_e61(paste0("Poverty2Panel_hour_min",hour_limit,".pdf"), bar_chart,  histogram_plot,
         footnotes = "Poverty defined by the Henderson Poverty Line, at a household level. Experiment is how many households
    would be in poverty if one member of the household lost their job. Therefore, each partnered household where both members
    are working are included twice here; once for each person losing their job. Dashed lines on panel B. represent the median for each category.", 
         sources = c("e61", "ABS"))

######################################################################################
###############################################################


#   # Normalize weights
# Rep_rates_df_subset[, normalized_weight := SIHPSWT / sum(SIHPSWT)]
  
  # Initialize a data frame to store results
  results <- data.table(
    i = integer(),
    group = character(),
    weighted_sum = numeric()
  )
  
  # Loop over i values from 0 to 200
  for (i in 0:265) {
    # Adjust poverty_gap for rows with taxable_benefit > 0
    adjusted_data <- Rep_rates_df_subset %>%
      mutate(adjusted_poverty_gap = if_else(hours0_taxable_benefit > 0, poverty_gap + i, poverty_gap))
    
    # Calculate the weighted sum for each group
    group_results <- adjusted_data %>%
      mutate(group = case_when(
        Asset_test_flag == 1 ~ "Asset Test Failed",
        Asset_test_flag == 0 & weeks_of_liquid_assets > 5 ~ "Asset Test Passed & >5 Weeks Liquid Assets",
        Asset_test_flag == 0 & weeks_of_liquid_assets < 5 & eligibility_status %in% c("Benefit Eligible",
                                                                                      "Benefit + FTB Eligible") ~ "Asset Test Passed & <5 Weeks Liquid Assets & Eligible",
        TRUE ~ "Other"
      )) %>%
      group_by(group) %>%
      summarise(
        weighted_sum = sum((adjusted_poverty_gap <= 0) * normalized_weight_hhld, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(i = i)
    
    # Append results for each i value
    results <- bind_rows(results, group_results)
  }
  
  results[, group := factor(group, levels = c(
    "Asset Test Failed",
    "Other",
    "Asset Test Passed & >5 Weeks Liquid Assets",
    "Asset Test Passed & <5 Weeks Liquid Assets & Eligible"
    
    
  ))]
  
  # Create the stacked line chart
  povertyhyp <- ggplot(results, aes(x = i, y = weighted_sum * 100, fill = group)) +
    geom_area(alpha = 0.8, color = "black") +
    labs_e61(
      subtitle = paste("A. Post Job Loss Poverty Head Count"),
      x = "Increase in Weekly Benefits ($)",
      y = "%",
      fill = "Group"
    ) + plot_label(c("On Benefit* with \nfew liquid assets", 
                   " On Benefit* \nwith >5 weeks \nliquid assets", 
                   "Failed \nAsset test", 
                   "JSP Singles + \nRA recipients \nmove out \nof poverty", 
                   "JSP Singles \nmove out \nof poverty"), 
                   c(22,22,22, 120, 222), c(7, 18, 30, 34, 34),
                   c("white", "white", "white", "black", "black"), size = 3) + geom_vline(xintercept = 114, 
                                                                                         linetype = "dashed") + 
    geom_vline(xintercept = 219, linetype = "dashed")
########################################################################################################
##################################################################################################################

  print(povertyhyp)
  
  save_e61(paste0("povertytotalstack_hour_min",hour_limit,".pdf"), povertyhyp, 
           footnotes = "`On Benefit' Includes both those on JSP and the Parenting Payment, including those also on FTB.
      Poverty defined by the Henderson Poverty Line, at a household level. Experiment is how many households
    would be in poverty if one member of the household lost their job. Therefore, each partnered household where both members
    are working are included twice here; once for each person losing their job.
    Note that only those eligible for the JSP or PP receive the hypothetical increase here, so the `Failed Assets Test`
      Category is unchanged over the period" )

#   # Normalize weights
# Rep_rates_df_subset[, normalized_weight := SIHPSWT / sum(SIHPSWT)]
  
  # Define the poverty line (set a valid value)
  poverty_line <- abs(min(Rep_rates_df_subset$poverty_gap, na.rm = TRUE))  # Use the absolute minimum poverty gap
  
  # Initialize a data frame to store results
  results <- data.table(
    i = integer(),
    group = character(),
    pgi = numeric()
  )
  
  # Loop over i values from 0 to 200
  for (i in 0:265) {
    # Adjust poverty_gap for rows with taxable_benefit > 0
    adjusted_data <- Rep_rates_df_subset %>%
      mutate(adjusted_poverty_gap = if_else(hours0_taxable_benefit > 0, poverty_gap + i, poverty_gap)) %>%
      mutate(adjusted_poverty_gap = if_else(adjusted_poverty_gap > 0, 0, adjusted_poverty_gap))
    
    # Calculate PGI for each group
    group_results <- adjusted_data %>%
      mutate(
        group = case_when(
          Asset_test_flag == 1 ~ "Asset Test Failed",
          Asset_test_flag == 0 & weeks_of_liquid_assets > 5 ~ "Asset Test Passed & >5 Weeks Liquid Assets",
          Asset_test_flag == 0 & weeks_of_liquid_assets < 5 & eligibility_status %in% c("Benefit Eligible",
      "Benefit + FTB Eligible") ~ "Asset Test Passed & <5 Weeks Liquid Assets & Eligible",
          TRUE ~ "Other"
        ),
        gap_normalized = abs(adjusted_poverty_gap) / poverty_line  # Normalize by poverty line
      ) %>%
      group_by(group) %>%
      summarise(
        pgi = sum(gap_normalized * normalized_weight_hhld, na.rm = TRUE) / sum(normalized_weight_hhld, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(i = i)
    
    # Append results for each i value
    results <- bind_rows(results, group_results)
  }
  
  # Set the order of the groups
  results[, group := factor(group, levels = c(
    "Asset Test Failed",
    "Other",
    "Asset Test Passed & >5 Weeks Liquid Assets",
    "Asset Test Passed & <5 Weeks Liquid Assets & Eligible"
    
    
  ))]
  
  # Create the stacked line chart
  plot_povJL <- ggplot(results, aes(x = i, y = pgi * 100, fill = group)) +
    geom_area(alpha = 0.8, color = "black") +
    labs(
      subtitle = paste("B. Post Job Loss Poverty Gap Index"),
      x = "Increase in Weekly Benefits ($)",
      y = "%",
      fill = "Group"
    )  + scale_y_continuous_e61(limits = c(0,40,5))

print(povertyhyp)

  save_e61(paste0("povertyhyp_hour_min",hour_limit,".pdf"), povertyhyp, plot_povJL, 
           footnotes = "`On Benefit' Includes both those on JSP and the Parenting Payment, including those also on FTB.
      Poverty defined by the Henderson Poverty Line, at a household level. Experiment is how many households
    would be in poverty if one member of the household lost their job. Therefore, each partnered household where both members
    are working are included twice here; once for each person losing their job.
    Note that only those eligible for the JSP or PP receive the hypothetical increase here, so the `Failed Assets Test`
      Category is unchanged over the period" )

  
  
################ Create single and partnered versions

  # Function to compute results based on partnered status
  compute_results <- function(subset_data) {
    subset_data[, normalized_weight_hhld := hhld_wgt / sum(hhld_wgt)]
    
    results <- data.table(
      i = integer(),
      group = character(),
      weighted_sum = numeric()
    )
    
    for (i in 0:265) {
      adjusted_data <- subset_data %>%
        mutate(adjusted_poverty_gap = if_else(hours0_taxable_benefit > 0, poverty_gap + i, poverty_gap))
      
      group_results <- adjusted_data %>%
        mutate(group = case_when(
          Asset_test_flag == 1 ~ "Asset Test Failed",
          Asset_test_flag == 0 & weeks_of_liquid_assets > 5 ~ "Asset Test Passed & >5 Weeks Liquid Assets",
          Asset_test_flag == 0 & weeks_of_liquid_assets < 5 & eligibility_status %in% c("Benefit Eligible", "Benefit + FTB Eligible") ~ "Asset Test Passed & <5 Weeks Liquid Assets & Eligible",
          TRUE ~ "Other"
        )) %>%
        group_by(group) %>%
        summarise(
          weighted_sum = sum((adjusted_poverty_gap <= 0) * normalized_weight_hhld, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(i = i)
      
      results <- bind_rows(results, group_results)
    }
    
    results[, group := factor(group, levels = c(
      "Asset Test Failed",
      "Other",
      "Asset Test Passed & >5 Weeks Liquid Assets",
      "Asset Test Passed & <5 Weeks Liquid Assets & Eligible"
    ))]
    
    return(results)
  }
  
  
results[i == 0]

  # Compute results for partnered and single individuals
results_couple <- compute_results(Rep_rates_df_subset[partnered == 1])
results_single <- compute_results(Rep_rates_df_subset[partnered == 0])
results_single_dep <- compute_results(Rep_rates_df_subset[partnered == 0 & Numb_dep > 0])
results_single_nodep <- compute_results(Rep_rates_df_subset[partnered == 0 & Numb_dep == 0])
results_couple_dep <- compute_results(Rep_rates_df_subset[partnered == 1 & Numb_dep > 0])
results_couple_nodep <- compute_results(Rep_rates_df_subset[partnered == 1 & Numb_dep == 0])

results_single_nodep_home <- compute_results(Rep_rates_df_subset[partnered == 0 & Numb_dep == 0 & Home_owner == 1])
results_single_nodep_nohome <- compute_results(Rep_rates_df_subset[partnered == 0 & Numb_dep == 0 & Home_owner == 0])

Rep_rates_df_subset[partnered == 0 & Numb_dep == 0][,.N,by=.(Home_owner)]

# Create the stacked line chart
povertyhyp_couple <- ggplot(results_couple, aes(x = i, y = weighted_sum * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("B. Couple Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)

povertyhyp_single <- ggplot(results_single, aes(x = i, y = weighted_sum * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("A. Single Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) + plot_label(c("On Benefit* with \nfew liquid assets", 
                   " On Benefit* \nwith >5 weeks \nliquid assets", 
                   "Failed \nAsset test", 
                   "JSP + \nRA recipients \nmove out \nof poverty", 
                   "JSP \nmove out \nof poverty"), 
                 c(21,21,21, 120, 222), c(15, 41, 62, 75, 75),
                 c("white", "white", "white", "black", "black"), size = 3) + geom_vline(xintercept = 114, 
                                                                                        linetype = "dashed") + 
  geom_vline(xintercept = 219, linetype = "dashed") +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)

povertyhyp_single_dep <- ggplot(results_single_dep, aes(x = i, y = weighted_sum * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("A. Single with Kids Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) + plot_label(c("On Benefit* with \nfew liquid assets", 
                   " On Benefit* \nwith >5 weeks \nliquid assets", 
                   "Failed \nAsset test", 
                   "JSP + \nRA recipients \nmove out \nof poverty", 
                   "JSP \nmove out \nof poverty"), 
                 c(21,21,21, 120, 222), c(15, 41, 62, 75, 75),
                 c("white", "white", "white", "black", "black"), size = 3) + geom_vline(xintercept = 114, 
                                                                                        linetype = "dashed") + 
  geom_vline(xintercept = 219, linetype = "dashed") +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)

povertyhyp_single_nodep <- ggplot(results_single_nodep, aes(x = i, y = weighted_sum * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("A. Single no Kids Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) + plot_label(c("On Benefit* with \nfew liquid assets", 
                   " On Benefit* \nwith >5 weeks \nliquid assets", 
                   "Failed \nAsset test", 
                   "JSP + \nRA recipients \nmove out \nof poverty", 
                   "JSP \nmove out \nof poverty"), 
                 c(21,21,21, 120, 222), c(15, 41, 62, 75, 75),
                 c("white", "white", "white", "black", "black"), size = 3) + geom_vline(xintercept = 114, 
                                                                                        linetype = "dashed") + 
  geom_vline(xintercept = 219, linetype = "dashed") +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)

povertyhyp_single_nodep_home <- ggplot(results_single_nodep_home, aes(x = i, y = weighted_sum * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("A. Single no Kids with home Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) + plot_label(c("On Benefit* with \nfew liquid assets", 
                   " On Benefit* \nwith >5 weeks \nliquid assets", 
                   "Failed \nAsset test", 
                   "JSP + \nRA recipients \nmove out \nof poverty", 
                   "JSP \nmove out \nof poverty"), 
                 c(21,21,21, 120, 222), c(15, 41, 62, 75, 75),
                 c("white", "white", "white", "black", "black"), size = 3) + geom_vline(xintercept = 114, 
                                                                                        linetype = "dashed") + 
  geom_vline(xintercept = 219, linetype = "dashed") +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)

povertyhyp_single_nodep_nohome <- ggplot(results_single_nodep_nohome, aes(x = i, y = weighted_sum * 100, fill = group)) +
  geom_area(alpha = 0.8, color = "black") +
  labs_e61(
    subtitle = paste("A. Single no Kids with nohome Post Job Loss Poverty Head Count"),
    x = "Increase in Weekly Benefits ($)",
    y = "%",
    fill = "Group"
  ) + plot_label(c("On Benefit* with \nfew liquid assets", 
                   " On Benefit* \nwith >5 weeks \nliquid assets", 
                   "Failed \nAsset test", 
                   "JSP + \nRA recipients \nmove out \nof poverty", 
                   "JSP \nmove out \nof poverty"), 
                 c(21,21,21, 120, 222), c(15, 41, 62, 75, 75),
                 c("white", "white", "white", "black", "black"), size = 3) + geom_vline(xintercept = 114, 
                                                                                        linetype = "dashed") + 
  geom_vline(xintercept = 219, linetype = "dashed") +
  scale_y_continuous_e61(limits = c(0,100,20),sec_axis = FALSE)


povertyhyp_single
povertyhyp_couple
povertyhyp_single_dep
povertyhyp_single_nodep
povertyhyp_single_nodep_home
povertyhyp_single_nodep_nohome

min(Rep_rates_df_subset$hours)

sum(results_single_nodep[i==0]$weighted_sum)

sum(results_single_nodep[i==0]$weighted_sum) - results_single_nodep[i==0 & group == "Asset Test Failed"]$weighted_sum

save_e61(paste0("povertyfamtype_hour_nomort_min",hour_limit,".pdf"), povertyhyp_single, povertyhyp_couple, 
         footnotes = "`On Benefit' Includes both those on JSP and the Parenting Payment, including those also on FTB.
      Poverty defined by the Henderson Poverty Line, at a household level. Experiment is how many households
    would be in poverty if one member of the household lost their job. Therefore, each partnered household where both members
    are working are included twice here; once for each person losing their job.
    Note that only those eligible for the JSP or PP receive the hypothetical increase here, so the `Failed Assets Test`
      Category is unchanged over the period" )

# save_e61(paste0("povertyfamtype_hour_min",hour_limit,".pdf"), povertyhyp_single, povertyhyp_couple, 
#          footnotes = "`On Benefit' Includes both those on JSP and the Parenting Payment, including those also on FTB.
#       Poverty defined by the Henderson Poverty Line, at a household level. Experiment is how many households
#     would be in poverty if one member of the household lost their job. Therefore, each partnered household where both members
#     are working are included twice here; once for each person losing their job.
#     Note that only those eligible for the JSP or PP receive the hypothetical increase here, so the `Failed Assets Test`
#       Category is unchanged over the period" )


single_nodep_initial <- results_single_nodep[i == 0][,type := "Single No Children"]
single_dep_initial <- results_single_dep[i == 0][,type := "Single with Children"]
couple_nodep_initial <- results_couple_nodep[i == 0][,type := "Couple No Children"]
couple_dep_initial <- results_couple_dep[i == 0][,type := "Couple with Children"]

poverty_family_type <- rbind(single_nodep_initial,single_dep_initial,couple_nodep_initial,couple_dep_initial)

ggplot(poverty_family_type,aes(x=type,y=weighted_sum*100,fill=group)) + geom_col(colour = "black") +
  scale_y_continuous_e61(limits=c(0,100,25)) +
  labs_e61(subtitle = "Proportion of total family type",y="%",
           sources = c("ABS","e61")) +
  plot_label(c("On Benefit* with \nfew liquid assets", 
                 " On Benefit* \nwith >5 weeks \nliquid assets", 
                 "Failed \nAsset test"), 
               c(1.5,1.5,1.5), c(25, 50, 75),
               c(palette_e61(4)[4], palette_e61(4)[3],palette_e61(4)[1]), size = 3) +
  coord_flip()

save_e61(paste0("poverty_family_hour",hour_limit,".pdf"))


ggplot(poverty_family_type,aes(x=type,y=weighted_sum*100,fill=group)) + geom_col(colour = "black") +
  scale_y_continuous_e61(limits=c(0,100,25)) +
  labs_e61(title = "Poverty after job loss",
           subtitle = "Proportion of total family type",y="%",
           sources = c("ABS","e61")) +
  plot_label(c("On Benefit* with \nfew liquid assets", 
               " On Benefit* \nwith >5 weeks \nliquid assets", 
               "Failed \nAsset test"), 
             c(1.5,1.5,1.5), c(25, 50, 75),
             c(palette_e61(4)[4], palette_e61(4)[3],palette_e61(4)[1]), size = 3) +
  coord_flip()

save_e61(paste0("poverty_family_hour",hour_limit,".png"),res=2,pad_width = 1)
