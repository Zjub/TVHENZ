## Last update:  23/04/2025
## Last update person:  Matt Nolan 
# Constructing RR plots for the higher payment scenario

library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(tidyverse)
library(data.table)

work_home <- "work"

if (work_home == "work"){
  Rep_rates_df <- read_csv("C:/Users/MattNolan/Downloads/RRs_csv 3.csv") # Work version original
} else {
  Rep_rates_df <- read_csv("C:/Users/OEM/Downloads/RRs_csv 3.csv") # Home version original
}

setDT(Rep_rates_df)

hour_limit <- 30

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


####### Drop those with Negative incomes 

Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$current_net_income >= 0)
Rep_rates_df <- subset(Rep_rates_df , Rep_rates_df$hours0_net_income >= 0)

setDT(Rep_rates_df)
##########################################

#### AGGREGATE REPLACEMENT RATES #########################################################
####                             #########################################################
####                            ##########################################################

################# Subset data to remove those working less than 5 hours. 
Rep_rates_df_subset <- subset(Rep_rates_df, Rep_rates_df$hours > hour_limit)

#### Throughout, we use "normalized_weight", which says the proportion of the population 
### The individual represents. 
Rep_rates_df_subset[, normalized_weight := SIHPSWT / sum(SIHPSWT)]

### Start calculations here ----

# 
# mean_net_RR_3 <- sum(Rep_rates_df_subset$net_RR * Rep_rates_df_subset$SIHPSWT) / 
#   sum(Rep_rates_df_subset$SIHPSWT)
# 
# # Calculate the weighted median replacement rate
# sorted_data <- Rep_rates_df_subset[order(net_RR)]
# cumulative_weights <- cumsum(sorted_data$normalized_weight)
# median_index <- which(cumulative_weights >= 0.5)[1]
# median_net_RR_3 <- sorted_data$net_RR[median_index]


# Filter rows where net_RR > 0
Rep_rates_df_subset_filtered <- Rep_rates_df_subset[hours0_taxable_benefit > 0] # Only JSP/PPP
#Rep_rates_df_subset_filtered <- Rep_rates_df_subset[net_RR > 0] # All positive RRs
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

mean_net_RR_3
median_net_RR_3



RR3 <- ggplot(Rep_rates_df_subset_filtered, aes(x = net_RR * 100, weight = normalized_weight2)) +
  geom_histogram(binwidth = 1,  fill = e61_tealdark) +
  labs(title = "Distribution of Replacement Rates: those eligible for an income support payment",
       subtitle = "", 
       x = "Replacement Rate",
       y = "%") +
  geom_vline(aes(xintercept = mean_net_RR_3 * 100), color = "purple", linetype = "dashed") +
  geom_vline(aes(xintercept = median_net_RR_3 * 100), color = e61_orangedark, linetype = "dashed") 

print(RR3)

initial_dist <- Rep_rates_df_subset_filtered[,.(net_RR,normalized_weight2)]

### New distribution

if (work_home == "work"){
  Rep_rates_df <- read_csv("C:/Users/MattNolan/Downloads/RRs_JSP_increase_csv.csv") # Work version scenario
} else {
  Rep_rates_df <- read_csv("C:/Users/OEM/Downloads/RRs_JSP_increase_csv.csv") # Home version scenario
}




setDT(Rep_rates_df)

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


####### Drop those with Negative incomes 

Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$current_net_income >= 0)
Rep_rates_df <- subset(Rep_rates_df , Rep_rates_df$hours0_net_income >= 0)

setDT(Rep_rates_df)
##########################################

#### AGGREGATE REPLACEMENT RATES #########################################################
####                             #########################################################
####                            ##########################################################

################# Subset data to remove those working less than the limit hours. 
Rep_rates_df_subset <- subset(Rep_rates_df, Rep_rates_df$hours > hour_limit)

#### Throughout, we use "normalized_weight", which says the proportion of the population 
### The individual represents. 
Rep_rates_df_subset[, normalized_weight := SIHPSWT / sum(SIHPSWT)]

Rep_rates_df_subset

# Filter rows where net_RR > 0
Rep_rates_df_subset_filtered <- Rep_rates_df_subset[hours0_taxable_benefit > 0] # Only JSP/PPP
#Rep_rates_df_subset_filtered <- Rep_rates_df_subset[net_RR > 0] # All positive RRs
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

mean_net_RR_3
median_net_RR_3



RR3 <- ggplot(Rep_rates_df_subset_filtered, aes(x = net_RR * 100, weight = normalized_weight2)) +
  geom_histogram(binwidth = 1,  fill = e61_tealdark) +
  labs(title = "Distribution of Replacement Rates: those eligible for an income support payment",
       subtitle = "", 
       x = "Replacement Rate",
       y = "%") +
  geom_vline(aes(xintercept = mean_net_RR_3 * 100), color = "purple", linetype = "dashed") +
  geom_vline(aes(xintercept = median_net_RR_3 * 100), color = e61_orangedark, linetype = "dashed") 

print(RR3)

new_dist <- Rep_rates_df_subset_filtered[,.(net_RR,normalized_weight2)]

### Compare the two distributions


# Add a 'source' column to identify datasets
initial_dist[, source := "Initial"]
new_dist[, source := "New"]

# Combine datasets
combined_dist <- rbind(initial_dist, new_dist)

sorted_data_initial <- combined_dist[source == "Initial"][order(net_RR)]
cumulative_weights_initial <- cumsum(sorted_data_initial$normalized_weight2 / 100)
median_index_initial <- which(cumulative_weights_initial >= 0.5)[1]
median_net_initial <- sorted_data_initial$net_RR[median_index]

sorted_data_new <- combined_dist[source == "New"][order(net_RR)]
cumulative_weights_new <- cumsum(sorted_data_new$normalized_weight2 / 100)
median_index_new <- which(cumulative_weights_new >= 0.5)[1]
median_net_new <- sorted_data_new$net_RR[median_index]

median_net_initial
median_net_new

# Plot overlayed histograms
ggplot(combined_dist, aes(x = net_RR * 100, weight = normalized_weight2, fill = source)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
  labs_e61(subtitle = "Replacement Rate Distributions",
       x = "",
       y = "",
       fill = "Dataset") +
  scale_fill_manual(values = c("Initial" = palette_e61(3)[2], "New" = palette_e61(3)[3])) +
  plab(c("Current","Increased"),x=c(0,72),y=c(2.2,2.2),colour=c(palette_e61(3)[1],palette_e61(3)[3])) +
  scale_y_continuous_e61(limits = c(0,3,0.5)) +
  geom_vline(xintercept = median_net_initial*100,linetype="solid",colour=palette_e61(3)[2],size=1) +
  geom_vline(xintercept = median_net_new*100,linetype="solid",colour=palette_e61(3)[3],size=1)

save_e61(paste0("Reform_RR_hours",hour_limit,".pdf"),footnotes = "Distribution of Replacement Rates for those eligible for either JSP or PPP after job loss.")


combined_dist[net_RR >= 0.5,.(sum(normalized_weight2)),by=.(source)]

combined_dist[net_RR >= 0.70,.(sum(normalized_weight2)),by=.(source)]

combined_dist[net_RR == 0,.(sum(normalized_weight2)),by=.(source)]

combined_dist[,.(sum(normalized_weight2)),by=.(source)]


