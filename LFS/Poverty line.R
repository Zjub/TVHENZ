# Income distribution and poverty talk

set.seed(123)

rm(list=ls())

.libPaths(new = 'C:/Rpackage')

library(remotes)
library(tidyverse)
library(data.table)
library(collapse)
library(readabs)
library(readr)
library(readxl)
library(theme61)
library(lubridate)
library(mFilter)
library(zoo)
library(Hmisc)
library(seasonal)

remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

# Step 1: Generate a log-normal income distribution
set.seed(123)  # For reproducibility
n <- 10000  # Number of observations
mean_income <- log(50000)  # Mean of the log-income
sd_income <- 1  # Standard deviation of the log-income

# Generate the log-normal distribution
income_data <- data.table(income = rlnorm(n, meanlog = mean_income, sdlog = sd_income))

# Ensuring all generated incomes are positive (log-normal distribution guarantees this)
income_data[income <= 0, income := 0.01]  # Adjusting any non-positive values if necessary

# Step 2: Calculate the relative poverty line (50% of median income)
median_income <- median(income_data$income)
poverty_line <- 0.5 * median_income

# Step 3: Compute headcount poverty
income_data[, poverty_status := income < poverty_line]
headcount_poverty_rate <- mean(income_data$poverty_status)

# Step 4: Calculate the Sen Inequality Index
# Poverty gap
income_data[, poverty_gap := fifelse(income < poverty_line, poverty_line - income, 0)]
# Gini index among the poor
gini_poor <- function(x) {
  if (length(x[x > 0]) > 1) {
    sorted <- sort(x[x > 0])
    n <- length(sorted)
    coef <- 2 / n
    sum_coef <- coef * sum((1:n) * sorted)
    return(1 - sum_coef / sum(sorted))
  } else {
    return(0)
  }
}
poverty_gap_ratio <- sum(income_data$poverty_gap) / (n * poverty_line)
gini_index_poor <- gini_poor(income_data$poverty_gap)
sen_index <- headcount_poverty_rate * poverty_gap_ratio * gini_index_poor

# Results
cat("Median Income:", median_income, "\n")
cat("Poverty Line:", poverty_line, "\n")
cat("Headcount Poverty Rate:", headcount_poverty_rate, "\n")
cat("Sen Inequality Index:", sen_index, "\n")

# Convert income to log-income for the plot
income_data[, log_income := log(income)]

# Plotting the log-income distribution with the poverty line (also in log scale)
plot <- ggplot(income_data, aes(x = log_income)) +
  geom_histogram(bins = 50, fill = "lightgreen", color = "black") +
  geom_vline(aes(xintercept = log(poverty_line)), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = log(poverty_line) + 0.5, y = 590, 
           label = sprintf("Line: $%.1f", log(poverty_line)), # Adding $ symbol
           hjust = 1.5, color = "red") +
  labs(title = "Log Income Distribution and Poverty Line (50% median earnings)",
       x = "Log Income",
       y = "Number") +
  theme_minimal()

# Adding annotations for headcount poverty rate and Sen inequality index
plot #+ annotate("text", x = max(income_data$log_income), y = 590, label = sprintf("Headcount Poverty Rate: %.2f%%", headcount_poverty_rate * 100), hjust = 1) #+
  #annotate("text", x = max(income_data$log_income), y = 20, label = sprintf("Sen Inequality Index: %.4f", sen_index), hjust = 1)

ggsave("log_inc.png",res=2)

# Create the CDF plot
cdf_plot <- ggplot(income_data, aes(x = log_income)) +
  stat_ecdf(geom = "step", color = "blue") +
  geom_vline(aes(xintercept = log(poverty_line)), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = log(poverty_line)-0.5, y = 0.05, label = sprintf("Line: %.1f", log(poverty_line)), hjust = -0.5, color = "red") +
  labs(title = "Income with Poverty Line",
       x = "Log Income",
       y = "Cumulative Share") +
  theme_minimal() + geom_hline(yintercept = headcount_poverty_rate,linetype="dotted")

# Adding annotations for headcount poverty rate and Sen inequality index
cdf_plot <- cdf_plot +
  annotate("text", x = max(income_data$log_income), y = 1.15, label = sprintf("Headcount Poverty Rate: %.1f%%", headcount_poverty_rate * 100), hjust = 1.1) #+
  #annotate("text", x = max(income_data$log_income), y = 0.90, label = sprintf("Sen Inequality Index: %.4f", sen_index), hjust = 1.1)

# Print the plot
print(cdf_plot)

ggsave("log_inc_CDF.png",res=2)

### Now add the income distribution

income_data[, `:=` (sorted_log_income = sort(log_income),
                    sorted_income = sort(income))]
income_data[, cumulative_income := cumsum(sorted_income)]
total_income <- sum(income_data$income)
income_data[, cumulative_income_share := cumulative_income / total_income]

# Create the CDF plot for cumulative income share
weighted_cdf_plot <- ggplot(income_data, aes(x = sorted_log_income, y = cumulative_income_share)) +
  geom_line(color = "blue") +
  geom_vline(aes(xintercept = log(poverty_line)), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = log(poverty_line), y = 0.05, label = sprintf("Log Poverty Line: %.2f", log(poverty_line)), hjust = -0.5, color = "red") +
  labs(title = "Weighted CDF of Log Income with Poverty Line",
       x = "Log Income",
       y = "Cumulative Share of Total Income") +
  theme_minimal()

# Adding annotations for headcount poverty rate and Sen inequality index
weighted_cdf_plot <- weighted_cdf_plot +
  annotate("text", x = max(income_data$sorted_log_income), y = 0.95, label = sprintf("Headcount Poverty Rate: %.2f%%", headcount_poverty_rate * 100), hjust = 1.1) +
  annotate("text", x = max(income_data$sorted_log_income), y = 0.90, label = sprintf("Sen Inequality Index: %.4f", sen_index), hjust = 1.1)

# Print the plot
print(weighted_cdf_plot)


## CDF in level terms

# # Create the CDF plot
# cdf_plot2 <- ggplot(income_data, aes(x = income)) +
#   stat_ecdf(geom = "step", color = "blue") +
#   geom_vline(aes(xintercept = (poverty_line)), color = "red", linetype = "dashed", size = 1) +
#   annotate("text", x = (poverty_line), y = 0.05, label = sprintf("Poverty Line: %.2f", (poverty_line)), hjust = -0.5, color = "red") +
#   labs(title = "CDF of Income with Poverty Line",
#        x = "Income",
#        y = "Cumulative Probability") +
#   theme_minimal()
# 
# # Adding annotations for headcount poverty rate and Sen inequality index
# cdf_plot2 <- cdf_plot2 +
#   annotate("text", x = max(income_data$log_income), y = 0.95, label = sprintf("Headcount Poverty Rate: %.2f%%", headcount_poverty_rate * 100), hjust = 1.1) +
#   annotate("text", x = max(income_data$log_income), y = 0.90, label = sprintf("Sen Inequality Index: %.4f", sen_index), hjust = 1.1)
# 
# # Print the plot
# print(cdf_plot2)
