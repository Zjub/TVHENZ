### Initial two stage commitment game to explain what we are thinking about with the paper

gc()
rm(list=ls())
library(haven)
library(data.table)
library(tidyverse)
library(gtsummary)
library(zoo)
library(ggplot2)
library(theme61)
library(fixest)
library(skimr)
library(modelsummary)


set.seed(123)

# Parameters for the model
gamma <- 1.5  # Intercept for inflation
alpha <- 2    # Intercept for output
bG <- 0.5     # Sensitivity of output to fiscal policy
bB <- 0.3     # Sensitivity of output to monetary policy
dG <- 0.4     # Sensitivity of inflation to fiscal policy
dB <- 0.6     # Sensitivity of inflation to monetary policy

mu <- 0.7     # Weight on inflation in the loss function (shared by both players)

# Target inflation and output
pi_G_star <- 2
pi_B_star <- 1.5
y_G_star <- 3
y_B_star <- 2

# Precommitment costs
cost_G_precommit <- 0.2
cost_B_precommit <- 0.2

# Best response functions for government and central bank

# Government's best response to monetary policy choice by the central bank
BR_G <- function(m) {
  numerator <- 2 * bB * (1 - mu) * (y_G_star - alpha) - 2 * dB * mu * (pi_G_star - gamma)
  denominator <- 2 * ((bG^2) * (1 - mu) + (dG^2) * mu)
  f_star <- numerator / denominator
  return(f_star)
}

# Central bank's best response to fiscal policy choice by the government
BR_B <- function(f) {
  numerator <- 2 * bG * (1 - mu) * (y_B_star - alpha) - 2 * dG * mu * (pi_B_star - gamma)
  denominator <- 2 * ((bB^2) * (1 - mu) + (dB^2) * mu)
  m_star <- numerator / denominator
  return(m_star)
}

# Nash equilibrium without precommitment (solving the system iteratively)
nash_equilibrium <- function(tol = 1e-6, max_iter = 1000) {
  f <- runif(1, -2, 2)  # Random initial guess for fiscal policy
  m <- runif(1, -2, 2)  # Random initial guess for monetary policy
  for (i in 1:max_iter) {
    f_new <- BR_G(m)
    m_new <- BR_B(f)
    if (abs(f_new - f) < tol && abs(m_new - m) < tol) {
      break
    }
    f <- f_new
    m <- m_new
  }
  return(list(fiscal = f, monetary = m))
}

# Simulating the Nash equilibrium
nash_result <- nash_equilibrium()
cat("Nash Equilibrium without Precommitment:\n")
print(nash_result)

# Adding Precommitment stage
# Here we assume that players can invest to influence each other's choices
# We model precommitment as simply a modification of their choices by reducing costs

simulate_precommitment <- function(precommit_G = TRUE, precommit_B = TRUE) {
  if (precommit_G) {
    f_star <- nash_result$fiscal - cost_G_precommit
  } else {
    f_star <- nash_result$fiscal
  }
  
  if (precommit_B) {
    m_star <- nash_result$monetary - cost_B_precommit
  } else {
    m_star <- nash_result$monetary
  }
  
  return(list(fiscal = f_star, monetary = m_star))
}

# Simulating with both players precommitting
precommit_result <- simulate_precommitment(precommit_G = TRUE, precommit_B = TRUE)
cat("\nEquilibrium with Precommitment:\n")
print(precommit_result)

# Plotting results for comparison
library(ggplot2)

df <- data.frame(
  Policy = c("Nash Fiscal", "Nash Monetary", "Precommit Fiscal", "Precommit Monetary"),
  Value = c(nash_result$fiscal, nash_result$monetary, precommit_result$fiscal, precommit_result$monetary)
)

ggplot(df, aes(x = Policy, y = Value, fill = Policy)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Policy Choices: Nash vs Precommitment", y = "Policy Tightness", x = "Policy Type")
