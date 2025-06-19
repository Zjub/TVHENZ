## Last update:  19/06/2025
## Last update person:  Matt Nolan 
# A script to compare the box plot for "all" and a given group.

library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(tidyverse)
library(data.table)
library(Hmisc)

work_home <- "home"

if (work_home == "work"){
  Rep_rates_df <- read_csv("C:/Users/MattNolan/Downloads/RRs_csv 3.csv") # Work version original
} else {
  Rep_rates_df <- read_csv("C:/Users/OEM/Downloads/RRs_csv 3.csv") # Home version original
}

filter_group <- "JSP" # Three potential groups:  JSP, pos_RR, and ALL.  JSP is only those with taxable benefits, pos_RR is all individuals with a positive replacement rate, ALL is everyone including zeros.
remove_PPonly <- FALSE # Removes individuals whose net_RR is coming from changes in FTB only.

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

# Drop those with Negative incomes 

Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$current_net_income >= 0)
Rep_rates_df <- subset(Rep_rates_df , Rep_rates_df$hours0_net_income >= 0)
Rep_rates_df <- subset(Rep_rates_df , Rep_rates_df$NonWageIncome >= 0)
Rep_rates_df[is.na(current_wk_partner_earnings), current_net_income_partner := 0] # Only replacing for those without partner earnings with partner - so may be some still there.
Rep_rates_df[is.na(current_wk_partner_earnings), current_wk_partner_earnings := 0]

# First create a variety of categories to consider the data
# Complex
# Rep_rates_df_subset[, fam_interaction_cat := paste0(
#   fifelse(partnered == 0, "Single", "Partnered"), 
#   ", ", 
#   fifelse(Numb_dep == "0", "no dependents", 
#           fifelse(Numb_dep == 1, "1 dependent", 
#                   fifelse(Numb_dep == 2, "2 dependents",
#                           "3+ dependents")))
# )]
# 
# # Manually order the interaction_cat column
# fam_interaction_order <- c("Single, no dependents",
#                        "Single, 1 dependent", 
#                        "Single, 2 dependents", 
#                        "Single, 3+ dependents",
#                        "Partnered, no dependents",
#                        "Partnered, 1 dependent",
#                        "Partnered, 2 dependents",
#                        "Partnered, 3+ dependents"
# )
# Rep_rates_df_subset[, fam_interaction_cat := factor(fam_interaction_cat, levels = fam_interaction_order)]

# Simple
# Rep_rates_df_subset[, fam_interaction_cat := paste0(
#   fifelse(partnered == 0, "Single", "Partnered"), 
#   ", ", 
#   fifelse(Numb_dep == 0, "no dependents", "with dependents"))]
# 
#
Rep_rates_df[, fam_interaction_cat := fifelse(partnered == 0 & Numb_dep == 0,"Single, no dependents",
                                                     fifelse(partnered == 0 & Numb_dep != 0 , "Single, with dependents",
                                                             fifelse(partnered == 1 & Numb_dep == 0, "Partnered, no dependents",
                                                                     "Partnered, with dependents")))]

table(Rep_rates_df$partnered)
table(Rep_rates_df$Numb_dep)
Rep_rates_df[is.na(Numb_dep)]


# Manually order the interaction_cat column
fam_interaction_order <- c("Single, no dependents",
                           "Single, with dependents",
                           "Partnered, no dependents",
                           "Partnered, with dependents"
)

Rep_rates_df[, fam_interaction_cat := factor(fam_interaction_cat, levels = fam_interaction_order)]

## Text is broken, replace with numbers
# Rep_rates_df_subset[, fam_interaction_cat := fifelse(partnered == 0 & Numb_dep == 0, 1,
#                                                      fifelse(partnered == 0 & Numb_dep != 0 , 2,
#                                                              fifelse(partnered == 1 & Numb_dep == 0, 3,
#                                                                      4)))]


Rep_rates_df[partnered == 0,.(partnered,Numb_dep,fam_interaction_cat)]
unique(Rep_rates_df[partnered == 0,.(partnered,Numb_dep,fam_interaction_cat)]$fam_interaction_cat)


# Construct earnings measures 

Rep_rates_df$hours0_wk_partner_earnings <- ifelse(is.na(Rep_rates_df$hours0_wk_partner_earnings), Rep_rates_df$hours0_gross_income_partner - Rep_rates_df$hours0_income_tax_partner + Rep_rates_df$hours0_RA_partner + Rep_rates_df$hours0_ES_partner, Rep_rates_df$hours0_wk_partner_earnings )

Rep_rates_df$hhincome <- Rep_rates_df$hours0_net_income + Rep_rates_df$NonWageIncome + Rep_rates_df$hours0_wk_partner_earnings

Rep_rates_df$hhincome <- ifelse(Rep_rates_df$hhincome < 0, 0, Rep_rates_df$hhincome)

Rep_rates_df[,":=" (eq_scale = 1 + 0.5*partnered + 0.3*Numb_dep)][,eq_hhinc := hhincome/eq_scale][,hhld_size := 1 + partnered + Numb_dep]

Rep_rates_df[,hhincome_pre := current_net_income + NonWageIncome + current_wk_partner_earnings] 

Rep_rates_df$hhincome_pre <- ifelse(Rep_rates_df$hhincome_pre < 0, 0, Rep_rates_df$hhincome_pre)

Rep_rates_df[,eq_hhinc_pre := hhincome_pre/eq_scale]

# And total assets

Rep_rates_df[,total_assets :=LiquidAssets_Household + Illiquid_Assets]
Rep_rates_df[,eq_total_assets :=total_assets/eq_scale]

################# Subset data to remove those working less than X hours. 
Rep_rates_df_subset <- subset(Rep_rates_df, Rep_rates_df$hours > hour_limit)


## Create quantiles
## Unweighted
#Rep_rates_df[, quantile_current_net_income := cut(
#   current_net_income,
#   breaks = quantile(current_net_income, probs = seq(0, 1, by = 0.2), na.rm = TRUE),
#   include.lowest = TRUE, labels = FALSE
# )]
# 
# breaks_current_net_income <- quantile(Rep_rates_df$current_net_income, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
# 
# 
# Rep_rates_df[, quantile_eq_hhinc_pre := cut(
#   eq_hhinc_pre,
#   breaks = quantile(eq_hhinc_pre, probs = seq(0, 1, by = 0.2), na.rm = TRUE),
#   include.lowest = TRUE, labels = FALSE
# )]
# 
# breaks_eq_net_income <- quantile(Rep_rates_df$eq_hhinc_pre, probs = seq(0, 1, by = 0.2), na.rm = TRUE)

# Weighted 
breaks_current_net_income <- wtd.quantile(
  Rep_rates_df_subset$current_net_income,
  weights = Rep_rates_df$SIHPSWT,
  probs = seq(0, 1, by = 0.2),
  na.rm = TRUE
)

breaks_eq_net_income <- wtd.quantile(
  Rep_rates_df_subset$eq_hhinc_pre,
  weights = Rep_rates_df$SIHPSWT,
  probs = seq(0, 1, by = 0.2),
  na.rm = TRUE
)

breaks_liq_personal <- wtd.quantile(
  Rep_rates_df_subset$Liquid_Assets_Person,
  weights = Rep_rates_df$SIHPSWT,
  probs = seq(0, 1, by = 0.2),
  na.rm = TRUE
)

breaks_hhld_assets<- wtd.quantile(
  Rep_rates_df_subset$total_assets,
  weights = Rep_rates_df$SIHPSWT,
  probs = seq(0, 1, by = 0.2),
  na.rm = TRUE
)

breaks_eq_hhld_assets<- wtd.quantile(
  Rep_rates_df_subset$eq_total_assets,
  weights = Rep_rates_df$SIHPSWT,
  probs = seq(0, 1, by = 0.2),
  na.rm = TRUE
)

### Remember to change below initial and all to look at these categories - rather than the raw variables

# Step 2: Now apply these breaks
Rep_rates_df_subset[, quantile_current_net_income := cut(
  current_net_income,
  breaks = breaks_current_net_income,
  include.lowest = TRUE,
  labels = FALSE
)]

Rep_rates_df_subset[, quantile_eq_hhinc_pre := cut(
  eq_hhinc_pre,
  breaks = breaks_eq_net_income,
  include.lowest = TRUE,
  labels = FALSE
)]

Rep_rates_df_subset[, quantile_LA_person := cut(
  Liquid_Assets_Person,
  breaks = breaks_liq_personal,
  include.lowest = TRUE,
  labels = FALSE
)]

Rep_rates_df_subset[, quantile_TA_hhld := cut(
  total_assets,
  breaks = breaks_hhld_assets,
  include.lowest = TRUE,
  labels = FALSE
)]

Rep_rates_df_subset[, quantile_eq_TA_hhld := cut(
  eq_total_assets,
  breaks = breaks_hhld_assets,
  include.lowest = TRUE,
  labels = FALSE
)]


##########################################

#### AGGREGATE REPLACEMENT RATES #########################################################
####                             #########################################################
####                            ##########################################################


#### Throughout, we use "normalized_weight", which says the proportion of the population 
### The individual represents. 
Rep_rates_df_subset[, normalized_weight := SIHPSWT / sum(SIHPSWT)]

# Cross check run - remove the FTB only individuals.
if(remove_PPonly == TRUE) {Rep_rates_df_subset <- Rep_rates_df_subset[!(hours0_taxable_benefit == 0 & net_RR > 0)]} # Only run to check if the higher RRs in the "all" situation are due to changes in FTB for some individuals.

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

if (filter_group == "JSP") {
  Rep_rates_df_subset_filtered <- Rep_rates_df_subset[hours0_taxable_benefit > 0]
} else if (filter_group == "pos_RR") {
  Rep_rates_df_subset_filtered <- Rep_rates_df_subset[net_RR > 0]
} else if (filter_group == "ALL") {
  Rep_rates_df_subset_filtered <- Rep_rates_df_subset
}

#Rep_rates_df_subset_filtered <- Rep_rates_df_subset[net_RR > 0] # All positive RRs
Rep_rates_df_subset_filtered[, normalized_weight2 := SIHPSWT / sum(SIHPSWT) * 100]
# Calculate the weighted mean
mean_net_RR_3 <- sum(Rep_rates_df_subset_filtered$net_RR * Rep_rates_df_subset_filtered$SIHPSWT) / 
  sum(Rep_rates_df_subset_filtered$SIHPSWT)


initial_dist <- Rep_rates_df_subset_filtered[,.(net_RR,normalized_weight2,current_net_income,eq_hhinc_pre,quantile_current_net_income,quantile_eq_hhinc_pre,fam_interaction_cat,quantile_LA_person,quantile_TA_hhld,quantile_eq_TA_hhld,hhld_size,partnered,Numb_dep)]

### Create all distribution

Rep_rates_df_subset_filtered_all <- Rep_rates_df_subset

Rep_rates_df_subset_filtered_all[, normalized_weight2 := SIHPSWT / sum(SIHPSWT) * 100]

all_dist <- Rep_rates_df_subset_filtered_all[,.(net_RR,normalized_weight2,current_net_income,eq_hhinc_pre,quantile_current_net_income,quantile_eq_hhinc_pre,fam_interaction_cat,quantile_LA_person,quantile_TA_hhld,quantile_eq_TA_hhld,hhld_size,partnered,Numb_dep)]

# Step 1: Add a source column
initial_dist[, source := "Eligible"]
all_dist[, source := "All"]

initial_dist[net_RR < 0, net_RR:= 0]
all_dist[net_RR < 0, net_RR:= 0]

# Step 2: Stack them together
long_dist <- rbind(initial_dist, all_dist)

# Step 3: Make income quantile labels (optional, for pretty x-axis)
income_labels <- c(
  paste0("< $", format(round(breaks_eq_net_income[2]), big.mark = ",")),
  paste0("$", format(round(breaks_eq_net_income[2]), big.mark = ","), 
         "–$", format(round(breaks_eq_net_income[3]), big.mark = ",")),
  paste0("$", format(round(breaks_eq_net_income[3]), big.mark = ","), 
         "–$", format(round(breaks_eq_net_income[4]), big.mark = ",")),
  paste0("$", format(round(breaks_eq_net_income[4]), big.mark = ","), 
         "–$", format(round(breaks_eq_net_income[5]), big.mark = ",")),
  paste0("> $", format(round(breaks_eq_net_income[5]), big.mark = ","))
)

income_labels_curr <- c(
  paste0("< $", format(round(breaks_current_net_income[2]), big.mark = ",")),
  paste0("$", format(round(breaks_current_net_income[2]), big.mark = ","), 
         "–$", format(round(breaks_current_net_income[3]), big.mark = ",")),
  paste0("$", format(round(breaks_current_net_income[3]), big.mark = ","), 
         "–$", format(round(breaks_current_net_income[4]), big.mark = ",")),
  paste0("$", format(round(breaks_current_net_income[4]), big.mark = ","), 
         "–$", format(round(breaks_current_net_income[5]), big.mark = ",")),
  paste0("> $", format(round(breaks_current_net_income[5]), big.mark = ","))
)

# Step 4: Plot
ggplot(long_dist, aes(x = as.factor(quantile_eq_hhinc_pre), y = net_RR*100, weight = normalized_weight2, color = source)) +
  geom_boxplot(position = position_dodge(width = 0.6), outlier.size = 0.8, outlier.alpha = 0.5) +
  labs(
    #title = "Replacement Rates by Income Quantile",
    subtitle = "By Prior Equivalised Income",
    x = "",
    y = "%",
    color = "Sample",
    sources = c("e61", "ABS"),
    footnotes = c("Replacement Rates following Job Loss, after one-year. Income quantiles defined for all Full Time workers.")
  ) +
  scale_x_discrete(labels = income_labels) +
  scale_y_continuous_e61(limits = c(0,100,20)) + 
  plab(label = c("All","Eligible"),y=c(82,82),x=c(4,5)) +
  coord_flip()

save_e61(paste0("Box_EI_RR_",hour_limit,".pdf"),pad_width = 1)


ggplot(long_dist, aes(x = as.factor(quantile_current_net_income), y = net_RR*100, weight = normalized_weight2, color = source)) +
  geom_boxplot(position = position_dodge(width = 0.6), outlier.size = 0.8, outlier.alpha = 0.5) +
  labs(
    #title = "Replacement Rates by Income Quantile",
    subtitle = "By Prior Labour Income",
    x = "",
    y = "%",
    color = "Sample",
    sources = c("e61", "ABS")
  ) +
  scale_x_discrete(labels = income_labels_curr) +
  scale_y_continuous_e61(limits = c(0,100,20)) + 
  plab(label = c("All","Eligible"),y=c(82,82),x=c(4,5)) +
  coord_flip()


save_e61(paste0("Box_PI_RR",hour_limit,".pdf"),pad_width = 1,
         footnotes = c("Replacement Rates following Job Loss, after one-year. Income quantiles defined for all Full Time workers."))


box_CI <- ggplot(long_dist, aes(x = as.factor(quantile_current_net_income), y = net_RR*100, weight = normalized_weight2, color = source)) +
  geom_boxplot(position = position_dodge(width = 0.6), outlier.size = 0.8, outlier.alpha = 0.5) +
  labs(
    #title = "Replacement Rates by Income Quantile",
    subtitle = "By Prior Labour Income",
    x = "",
    y = "%",
    color = "Sample"
    ) +
  scale_x_discrete(labels = income_labels_curr) +
  scale_y_continuous_e61(limits = c(0,100,20)) + 
  plab(label = c("All","Eligible"),y=c(82,82),x=c(3.5,4.5)) +
  coord_flip()

box_EI <- ggplot(long_dist, aes(x = as.factor(quantile_eq_hhinc_pre), y = net_RR*100, weight = normalized_weight2, color = source)) +
  geom_boxplot(position = position_dodge(width = 0.6), outlier.size = 0.8, outlier.alpha = 0.5) +
  labs(
    #title = "Replacement Rates by Income Quantile",
    subtitle = "By Prior Equivalised Income",
    x = "",
    y = "%",
    color = "Sample"
  ) +
  scale_x_discrete(labels = income_labels) +
  scale_y_continuous_e61(limits = c(0,100,20)) + 
  plab(label = c("All","Eligible"),y=c(82,82),x=c(3.5,4.5)) +
  coord_flip()

save_e61(paste0("Box_incomes_RR_",hour_limit,".pdf"),box_CI,box_EI,footnotes = c("Replacement Rates following Job Loss, after one-year. Income quantiles defined for all Full Time workers."),sources = c("ABS","e61"),pad_width = 1)

## The above are unweighted.  We can make weighting without the dots in the following way

weighted_box_current <- long_dist[, .(
  ymin = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.0),   # min
  lower = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.25),  # Q1
  middle = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.5),  # median
  upper = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.75),  # Q3
  ymax = max(net_RR)#wtd.quantile(net_RR, weights = normalized_weight2, probs = 1.0)     # max
), by = .(quantile_current_net_income, source)]

weighted_box_eq <- long_dist[, .(
  ymin = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.0),   # min
  lower = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.25),  # Q1
  middle = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.5),  # median
  upper = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.75),  # Q3
  ymax = max(net_RR)#wtd.quantile(net_RR, weights = normalized_weight2, probs = 1.0)     # max
), by = .(quantile_eq_hhinc_pre, source)]


box_CI_weight <- ggplot(weighted_box_current, aes(x = as.factor(quantile_current_net_income), color = source, fill = source)) +
  geom_boxplot(
    aes(
      ymin = ymin * 100,
      lower = lower * 100,
      middle = middle * 100,
      upper = upper * 100,
      ymax = ymax * 100
    ),
    stat = "identity",
    position = position_dodge(width = 0.6),
    width = 0.5,
    alpha = 0.3  # slight transparency if you want
  ) +
  labs(
    subtitle = "By Prior Labour Income",
    x = "",
    y = "%",
    color = "Sample",
    fill = "Sample"
  ) +
  scale_x_discrete(labels = income_labels_curr) +
  scale_y_continuous_e61(limits = c(0, 100, 20)) +
  plab(label = c("All", "Eligible"), y = c(82, 82), x = c(3.5, 4.5)) +
  coord_flip() 

box_EI_weight <- ggplot(weighted_box_eq[!is.na(quantile_eq_hhinc_pre)], aes(x = as.factor(quantile_eq_hhinc_pre), color = source, fill = source)) + # Removing a single observation that isn't computing
  geom_boxplot(
    aes(
      ymin = ymin * 100,
      lower = lower * 100,
      middle = middle * 100,
      upper = upper * 100,
      ymax = ymax * 100
    ),
    stat = "identity",
    position = position_dodge(width = 0.6),
    width = 0.5,
    alpha = 0.3  # slight transparency if you want
  ) +
  labs(
    subtitle = "By Prior Equivalised Income",
    x = "",
    y = "%",
    color = "Sample",
    fill = "Sample"
  ) +
  scale_x_discrete(labels = income_labels) +
  scale_y_continuous_e61(limits = c(0, 100, 20)) +
  plab(label = c("All", "Eligible"), y = c(82, 82), x = c(3.5, 4.5)) +
  coord_flip() 


if(hour_limit >= 30){
  save_e61(paste0("Box_income_RR_weighted_",hour_limit,".pdf"),box_CI_weight,box_EI_weight,footnotes = c("Replacement Rates following Job Loss, after one-year. Income quantiles defined for all Full Time workers."),sources = c("ABS","e61"),pad_width = 1)
} else{
  save_e61(paste0("Box_incomes_RR_weighted_",hour_limit,".pdf"),box_CI_weight,box_EI_weight,footnotes = c("Replacement Rates following Job Loss, after one-year. Income quantiles defined for all workers."),sources = c("ABS","e61"),pad_width = 1)
}

## Similar thing for assets, liquidity, and family types

## Family types

weighted_box_fam <- long_dist[, .(
  ymin = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.0),   # min
  lower = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.25),  # Q1
  middle = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.5),  # median
  upper = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.75),  # Q3
  ymax = max(net_RR)#wtd.quantile(net_RR, weights = normalized_weight2, probs = 1.0)     # max
), by = .(fam_interaction_cat, source)]

box_fam_weight <- ggplot(weighted_box_fam, aes(x = as.factor(fam_interaction_cat), color = source, fill = source)) +
  geom_boxplot(
    aes(
      ymin = ymin * 100,
      lower = lower * 100,
      middle = middle * 100,
      upper = upper * 100,
      ymax = ymax * 100
    ),
    stat = "identity",
    position = position_dodge(width = 0.6),
    width = 0.5,
    alpha = 0.3  # slight transparency if you want
  ) +
  labs_e61(
    subtitle = "By Family Type",
    x = "",
    y = "%",
    color = "Sample",
    fill = "Sample",footnotes = c("Replacement Rates following Job Loss, after one-year. For all Full Time workers."),sources = c("ABS","e61")
  ) +
  scale_x_discrete(labels = fam_interaction_order) +
  scale_y_continuous_e61(limits = c(0, 100, 20)) +
  plab(label = c("All", "Eligible"), y = c(82, 82), x = c(2.5, 3.5)) +
  coord_flip() 

box_fam_weight

if(hour_limit >= 30){
  save_e61(paste0("Box_fam_RR_weighted_",hour_limit,".pdf"),box_fam_weight,footnotes = c("Replacement Rates following Job Loss, after one-year. For all Full Time workers."),sources = c("ABS","e61"),pad_width = 2)
} else{
  save_e61(paste0("Box_fam_RR_weighted_",hour_limit,".pdf"),box_fam_weight,footnotes = c("Replacement Rates following Job Loss, after one-year. For all workers."),sources = c("ABS","e61"),pad_width = 2)
}

save_e61(paste0("Box_fam_RR_weighted_",hour_limit,".png"),box_fam_weight,footnotes = c("Replacement Rates following Job Loss, after one-year. For all Full Time workers."),sources = c("ABS","e61"),pad_width = 2,res=2)


## Personal liquidity

liquid_labels <- c(
  paste0("< $", format(round(breaks_liq_personal[2]), big.mark = ",")),
  paste0("$", format(round(breaks_liq_personal[2]), big.mark = ","),
         "–$", format(round(breaks_liq_personal[3]), big.mark = ",")),
  paste0("$", format(round(breaks_liq_personal[3]), big.mark = ","),
         "–$", format(round(breaks_liq_personal[4]), big.mark = ",")),
  paste0("$", format(round(breaks_liq_personal[4]), big.mark = ","),
         "–$", format(round(breaks_liq_personal[5]), big.mark = ",")),
  paste0("> $", format(round(breaks_liq_personal[5]), big.mark = ",")))

weighted_box_LA <- long_dist[, .(
  ymin = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.0),   # min
  lower = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.25),  # Q1
  middle = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.5),  # median
  upper = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.75),  # Q3
  ymax = max(net_RR)#wtd.quantile(net_RR, weights = normalized_weight2, probs = 1.0)     # max
), by = .(quantile_LA_person, source)]

box_LA_weight <- ggplot(weighted_box_LA, aes(x = as.factor(quantile_LA_person), color = source, fill = source)) +
  geom_boxplot(
    aes(
      ymin = ymin * 100,
      lower = lower * 100,
      middle = middle * 100,
      upper = upper * 100,
      ymax = ymax * 100
    ),
    stat = "identity",
    position = position_dodge(width = 0.6),
    width = 0.5,
    alpha = 0.3  # slight transparency if you want
  ) +
  labs_e61(
    subtitle = "By personal Liquid Assets",
    x = "",
    y = "%",
    color = "Sample",
    fill = "Sample"
  ) +
  scale_x_discrete(labels = liquid_labels) +
  scale_y_continuous_e61(limits = c(0, 100, 20)) +
  plab(label = c("All", "Eligible"), y = c(82, 82), x = c(3.5, 4.5)) +
  coord_flip() 



## Household net worth

asset_labels <- c(
  paste0("< $", format(round(breaks_hhld_assets[2]), big.mark = ",")),
  paste0("$", format(round(breaks_hhld_assets[2]), big.mark = ","),
         "–$", format(round(breaks_hhld_assets[3]), big.mark = ",")),
  paste0("$", format(round(breaks_hhld_assets[3]), big.mark = ","),
         "–$", format(round(breaks_hhld_assets[4]), big.mark = ",")),
  paste0("$", format(round(breaks_hhld_assets[4]), big.mark = ","),
         "–$", format(round(breaks_hhld_assets[5]), big.mark = ",")),
  paste0("> $", format(round(breaks_hhld_assets[5]), big.mark = ",")))

weighted_box_TA <- long_dist[, .(
  ymin = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.0),   # min
  lower = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.25),  # Q1
  middle = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.5),  # median
  upper = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.75),  # Q3
  ymax = max(net_RR)#wtd.quantile(net_RR, weights = normalized_weight2, probs = 1.0)     # max
), by = .(quantile_TA_hhld, source)]

box_TA_weight <- ggplot(weighted_box_TA, aes(x = as.factor(quantile_TA_hhld), color = source, fill = source)) +
  geom_boxplot(
    aes(
      ymin = ymin * 100,
      lower = lower * 100,
      middle = middle * 100,
      upper = upper * 100,
      ymax = ymax * 100
    ),
    stat = "identity",
    position = position_dodge(width = 0.6),
    width = 0.5,
    alpha = 0.3  # slight transparency if you want
  ) +
  labs_e61(
    subtitle = "By Total Household Assets",
    x = "",
    y = "%",
    color = "Sample",
    fill = "Sample"
  ) +
  scale_x_discrete(labels = asset_labels) +
  scale_y_continuous_e61(limits = c(0, 100, 20)) +
  plab(label = c("All", "Eligible"), y = c(82, 82), x = c(3.5, 4.5)) +
  coord_flip() 

box_TA_weight



# Eq household assets

eq_asset_labels <- c(
  paste0("< $", format(round(breaks_eq_hhld_assets[2]), big.mark = ",")),
  paste0("$", format(round(breaks_eq_hhld_assets[2]), big.mark = ","),
         "–$", format(round(breaks_eq_hhld_assets[3]), big.mark = ",")),
  paste0("$", format(round(breaks_eq_hhld_assets[3]), big.mark = ","),
         "–$", format(round(breaks_eq_hhld_assets[4]), big.mark = ",")),
  paste0("$", format(round(breaks_eq_hhld_assets[4]), big.mark = ","),
         "–$", format(round(breaks_eq_hhld_assets[5]), big.mark = ",")),
  paste0("> $", format(round(breaks_eq_hhld_assets[5]), big.mark = ",")))

weighted_box_eq_TA <- long_dist[, .(
  ymin = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.0),   # min
  lower = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.25),  # Q1
  middle = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.5),  # median
  upper = wtd.quantile(net_RR, weights = normalized_weight2, probs = 0.75),  # Q3
  ymax = max(net_RR) #wtd.quantile(net_RR, weights = normalized_weight2, probs = 1.0)     # max
), by = .(quantile_eq_TA_hhld, source)]

box_eq_TA_weight <- ggplot(weighted_box_eq_TA, aes(x = as.factor(quantile_eq_TA_hhld), color = source, fill = source)) +
  geom_boxplot(
    aes(
      ymin = ymin * 100,
      lower = lower * 100,
      middle = middle * 100,
      upper = upper * 100,
      ymax = ymax * 100
    ),
    stat = "identity",
    position = position_dodge(width = 0.6),
    width = 0.5,
    alpha = 0.3  # slight transparency if you want
  ) +
  labs_e61(
    subtitle = "By Total Household Assets (equivalised)",
    x = "",
    y = "%",
    color = "Sample",
    fill = "Sample"
  ) +
  scale_x_discrete(labels = eq_asset_labels) +
  scale_y_continuous_e61(limits = c(0, 100, 20)) +
  plab(label = c("All", "Eligible"), y = c(82, 82), x = c(3.5, 4.5)) +
  coord_flip() 

box_eq_TA_weight

if(hour_limit >= 30){
  save_e61(paste0("Box_asset_RR_weighted_",hour_limit,".pdf"),box_LA_weight,box_eq_TA_weight,footnotes = c("Replacement Rates following Job Loss, after one-year. Asset quantiles defined for all Full Time workers."),sources = c("ABS","e61"),pad_width = 1)
} else{
  save_e61(paste0("Box_asset_RR_weighted_",hour_limit,".pdf"),box_LA_weight,box_eq_TA_weight,footnotes = c("Replacement Rates following Job Loss, after one-year. Asset quantiles defined for all workers."),sources = c("ABS","e61"),pad_width = 1)
}

## Bruce questions about low replacement rates for single individuals


initial_dist[fam_interaction_cat == "Single, no dependents"]

