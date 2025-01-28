# Variance decomposition of RRs
# Author: Matt Nolan
# Created: 26/01/2025
# Last time and author:  Matt Nolan, 26/01/2025


library(tidyverse)
library(data.table)
library(theme61)
library(readxl)
library(Hmisc)
library(DescTools)
library(flextable)
library(boot)
library(relaimpo)

### Import data ----
Rep_rates_df <- read_csv("C:/Users/OEM/Downloads/RRs_csv 2.csv") # Home version

setDT(Rep_rates_df)

hour_limit <- 30

Rep_rates_df <- Rep_rates_df[hours >= hour_limit]

# Focus on only those who receive replacement:

weighted <- TRUE
winsor <- TRUE

nrow(Rep_rates_df)

Rep_rates_df <- Rep_rates_df[(hours0_taxable_benefit + hours0_net_fam_a_income + hours0_net_fam_b_income) - (current_taxable_benefit + current_net_fam_a_income + current_net_fam_b_income) > 0] 

group <- "Sing_Dep" # "ALL","Sing_NoDep", "Sing_Dep", "Coup_Dep", "Coup_NoDep"



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


# Add values for decomposition

Rep_rates_df[,":=" (income_replaced = fifelse(NonWageIncome == 0, hours0_net_income,
                                     fifelse(hours0_gross_income == 0,0,
                                             ((hours0_net_income-(NonWageIncome - NonWageIncome/hours0_gross_income*hours0_IT))))))]


Rep_rates_df[,":=" (income_lost = fifelse(NonWageIncome == 0, current_net_income,
                                              fifelse(hours0_gross_income == 0,0,
                                                      ((current_net_income-(NonWageIncome - NonWageIncome/current_gross_income*current_IT))))))]



###########################################################################################3

### Clean the Data - This should already be done for some of this in the tax Calc stuff

Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$AGEEC > 21)

Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$AGEEC < 55)

Rep_rates_df <- subset(Rep_rates_df, wage > 15)


####### Drop those with Negative incomes 

Rep_rates_df <- subset(Rep_rates_df, Rep_rates_df$current_net_income >= 0)
Rep_rates_df <- subset(Rep_rates_df , Rep_rates_df$hours0_net_income >= 0)

setDT(Rep_rates_df)




### Start VD -----

# With windsoring prior to the group subsetting we are windsoring for each group.
if (winsor == TRUE) {
  Rep_rates_df[,":=" (income_lost = Winsorize(income_lost, val = quantile(income_lost, probs = c(0.05, 0.95), na.rm = FALSE)),
                      income_replaced = Winsorize(income_replaced, val = quantile(income_replaced, probs = c(0.05, 0.95), na.rm = FALSE)))]
}

if (group == "Sing_Dep"){
  Rep_rates_df <- Rep_rates_df[partnered == 0 & Numb_dep > 0] # Very small sample
} else if (group == "Sing_NoDep") {
  Rep_rates_df <- Rep_rates_df[partnered == 0 & Numb_dep == 0]
} else if (group == "Coup_Dep") {
  Rep_rates_df <- Rep_rates_df[partnered == 1 & Numb_dep > 0]
} else if (group == "Coup_NoDep") {
  Rep_rates_df <- Rep_rates_df[partnered == 1 & Numb_dep == 0]
} else {
  Rep_rates_df <- Rep_rates_df
}


# Calculate means and variances
if (weighted == TRUE){
  mean_lost <- wtd.mean(Rep_rates_df$income_lost, Rep_rates_df$SIHPSWT, na.rm = TRUE)
  mean_replaced <- wtd.mean(Rep_rates_df$income_replaced, Rep_rates_df$SIHPSWT, na.rm = TRUE)
} else {
  mean_lost <- mean(Rep_rates_df$income_lost, na.rm = TRUE)
  mean_replaced <- mean(Rep_rates_df$income_replaced, na.rm = TRUE)
}

if (weighted == TRUE){
  var_lost <- wtd.var(Rep_rates_df$income_lost, Rep_rates_df$SIHPSWT, na.rm = TRUE)
  var_replaced <- wtd.var(Rep_rates_df$income_replaced, Rep_rates_df$SIHPSWT, na.rm = TRUE)
} else {
  var_lost <- var(Rep_rates_df$income_lost, na.rm = TRUE)
  var_replaced <- var(Rep_rates_df$income_replaced, na.rm = TRUE)
}


# Variance decomposition formula approximation
var_rr_approx <- (1 / mean_lost^2) * var_replaced +
  (mean_replaced / mean_lost^2)^2 * var_lost

cat("Approximate variance of RR:", var_rr_approx, "\n")

# Add log incomes for later analysis

# Transform income lost and income replaced to log scale
Rep_rates_df$log_income_lost <- log(Rep_rates_df$income_lost)
Rep_rates_df$log_income_replaced <- log(Rep_rates_df$income_replaced)
Rep_rates_df$log_RR <- log(Rep_rates_df$net_RR)

# Compute variances and covariance
if (weighted == TRUE){
  complete_cases <- complete.cases(Rep_rates_df$log_income_lost, Rep_rates_df$log_income_replaced, Rep_rates_df$SIHPSWT)
  
  var_log_lost <- wtd.var(Rep_rates_df$log_income_lost, Rep_rates_df$SIHPSWT, na.rm = TRUE)
  var_log_replaced <- wtd.var(Rep_rates_df$log_income_replaced, Rep_rates_df$SIHPSWT, na.rm = TRUE)
  cov_log <- cov.wt(cbind(Rep_rates_df$log_income_lost[complete_cases], Rep_rates_df$log_income_replaced[complete_cases]), wt = Rep_rates_df$SIHPSWT[complete_cases])$cov[1,2]
} else {
  var_log_lost <- var(Rep_rates_df$log_income_lost, na.rm = TRUE)
  var_log_replaced <- var(Rep_rates_df$log_income_replaced, na.rm = TRUE)
  cov_log <- cov(Rep_rates_df$log_income_lost, Rep_rates_df$log_income_replaced, use = "complete.obs")
}


#### Put in data checks ----

# Summary statistics
summary(Rep_rates_df$income_lost)
summary(Rep_rates_df$income_replaced)
summary(Rep_rates_df$log_income_lost)
summary(Rep_rates_df$log_income_replaced)


# Histograms and density plots
hist(Rep_rates_df$income_lost, main="Histogram of Income Lost", xlab="Income Lost", col="blue")
hist(Rep_rates_df$income_replaced, main="Histogram of Income Replaced", xlab="Income Replaced", col="red")

plot(density(Rep_rates_df$income_lost, na.rm=TRUE), main="Density of Incomes")
lines(density(Rep_rates_df$income_replaced, na.rm=TRUE), col="red")
legend("topright", legend = c("Income Lost", "Income Replaced"), col = c("black", "red"), lwd = 2)

# Scatter plot
plot(Rep_rates_df$income_lost, Rep_rates_df$income_replaced, 
     xlab="Income Lost", 
     ylab="Income Replaced", 
     main="Income Lost vs. Income Replaced", 
     pch=16, col=rgb(0, 0, 1, 0.5))
abline(lm(Rep_rates_df$income_replaced ~ Rep_rates_df$income_lost), col="red")

# Log transformation
Rep_rates_df$log_income_lost <- log(Rep_rates_df$income_lost + 1e-6)
Rep_rates_df$log_income_replaced <- log(Rep_rates_df$income_replaced + 1e-6)

# Histograms of log-transformed variables
hist(Rep_rates_df$log_income_lost, main="Log-transformed Income Lost", col="blue")
hist(Rep_rates_df$log_income_replaced, main="Log-transformed Income Replaced", col="red")

# QQ-plots for normality check
qqnorm(Rep_rates_df$log_income_lost, main="QQ-plot of Log Income Lost")
qqline(Rep_rates_df$log_income_lost, col="red")

qqnorm(Rep_rates_df$log_income_replaced, main="QQ-plot of Log Income Replaced")
qqline(Rep_rates_df$log_income_replaced, col="red")

# Variance-covariance matrix
cov_matrix <- cov(Rep_rates_df[, c("log_income_lost", "log_income_replaced")], use="complete.obs")
print(cov_matrix)

# Boxplots for outlier detection
boxplot(Rep_rates_df$income_lost, main="Boxplot of Income Lost")
boxplot(Rep_rates_df$income_replaced, main="Boxplot of Income Replaced")

# Correlation analysis
cor_lost_replaced <- cor(Rep_rates_df$income_lost, Rep_rates_df$income_replaced, use = "complete.obs")
cat("Correlation between Income Lost and Replaced:", cor_lost_replaced, "\n")

cor_log_lost_replaced <- cor(Rep_rates_df$log_income_lost, Rep_rates_df$log_income_replaced, use = "complete.obs")
cat("Correlation between Log Income Lost and Log Income Replaced:", cor_log_lost_replaced, "\n")

##### Regression -----
# 
# 
# model <- lm(log_RR ~ log_income_replaced + log_income_lost, data = Rep_rates_df)
# 
# summary(model)
# 
# # Compute relative importance of each predictor
# rel_importance <- calc.relimp(model, type = "lmg")  # LMG is the Lindeman, Merenda and Gold method
# 
# print(rel_importance)
# 
# 
# barplot(rel_importance$lmg, names.arg = names(rel_importance$lmg), main = "Relative Importance of Predictors", col = "steelblue")
# 
# # Standardize the data
# Rep_rates_df$log_income_replaced_scaled <- scale(Rep_rates_df$log_income_replaced)
# Rep_rates_df$log_income_lost_scaled <- scale(Rep_rates_df$log_income_lost)
# 
# # Re-run the regression with standardized variables
# model_standardized <- lm(log_RR ~ log_income_replaced_scaled + log_income_lost_scaled, data = Rep_rates_df)
# 
# 
# summary(model_standardized)
# 
# # Weighted regression with survey weights
# model_weighted <- lm(log_RR ~ log_income_replaced + log_income_lost, data = Rep_rates_df, weights = SIHPSWT)
# 
# summary(model_weighted)
# 
# rel_importance_weighted <- calc.relimp(model_weighted, type = "lmg")  # LMG is the Lindeman, Merenda and Gold method
# 
# print(rel_importance_weighted)
# 
# barplot(rel_importance_weighted$lmg, names.arg = names(rel_importance$lmg), main = "Relative Importance of Predictors", col = "steelblue")




# -------------- Log Transformation Approach -------------- #
# Variance decomposition using log transformation
var_log_rr <- var_log_replaced + var_log_lost - 2 * cov_log

cat("Variance of log(RR):", var_log_rr, "\n")
cat("Contribution from log income replaced:", var_log_replaced, "\n")
cat("Contribution from log income lost:", var_log_lost, "\n")
cat("Negative contribution from covariance:", -2 * cov_log, "\n")

var_log_replaced/var_log_rr
var_log_lost/var_log_rr
-2*cov_log/var_log_rr
nrow(Rep_rates_df)

# Summary statistics
summary(Rep_rates_df$income_lost)
summary(Rep_rates_df$income_replaced)
summary(Rep_rates_df$log_income_lost)
summary(Rep_rates_df$log_income_replaced)

