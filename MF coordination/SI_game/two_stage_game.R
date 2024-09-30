### Initial two stage commitment game to explain what we are thinking about with the paper

gc()
rm(list=ls())
library(haven)
library(data.table)
library(tidyverse)
library(gtsummary)
library(zoo)
library(ggplot2)
library(theme61) # comment out of non-e61 employee
library(fixest)
library(skimr)
library(modelsummary)
library(optimx)

set.seed(123)

# Parameters for the model
gamma <- 1.5  # Intercept for inflation (replaces c)
alpha <- 2    # Intercept for output
bG <- 0.5     # Sensitivity of output to fiscal policy
bB <- 0.5     # Sensitivity of output to monetary policy
dG <- 0.5     # Sensitivity of inflation to fiscal policy
dB <- 0.5     # Sensitivity of inflation to monetary policy

# New separate weights for inflation in the loss functions
mu_G <- 0.9  # Government's weight on inflation
mu_B <- 0.9  # Central bank's weight on inflation

# Target inflation and output
pi_G_star <- 2
pi_B_star <- 2
y_G_star <- 3
y_B_star <- 3

# Precommitment costs
cost_G_precommit <- 0.5
cost_B_precommit <- 0.5

# Government's best response to monetary policy choice by the central bank
BR_G <- function(m) {
  numerator <- 2 * bB * (1 - mu_G) * (y_G_star - alpha) - 2 * dB * mu_G * (pi_G_star - gamma)
  denominator <- 2 * ((bG^2) * (1 - mu_G) + (dG^2) * mu_G)
  f_star <- numerator / denominator
  return(f_star)
}

# Central bank's best response to fiscal policy choice by the government
BR_B <- function(f) {
  numerator <- 2 * bG * (1 - mu_B) * (y_B_star - alpha) - 2 * dG * mu_B * (pi_B_star - gamma)
  denominator <- 2 * ((bB^2) * (1 - mu_B) + (dB^2) * mu_B)
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

# Compute inflation and output for a given fiscal and monetary policy
compute_inflation_output <- function(f, m) {
  y <- alpha + bG * f + bB * m
  pi <- gamma + dG * f + dB * m
  return(list(output = y, inflation = pi))
}

# Simulating the Nash equilibrium
nash_result <- nash_equilibrium()
nash_outcomes <- compute_inflation_output(nash_result$fiscal, nash_result$monetary)
cat("Nash Equilibrium Outcomes:\n")
print(nash_outcomes)

# Precommitment outcome (both players precommit)
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

precommit_result <- simulate_precommitment(precommit_G = TRUE, precommit_B = TRUE)
precommit_outcomes <- compute_inflation_output(precommit_result$fiscal, precommit_result$monetary)
cat("\nPrecommitment Outcomes:\n")
print(precommit_outcomes)

# Cooperative outcome: maximizing joint welfare
cooperative_utility <- function(f, m) {
  uG <- -mu_G * (pi_G_star - (gamma + dG * f + dB * m))^2 - (1 - mu_G) * (y_G_star - (alpha + bG * f + bB * m))^2
  uB <- -mu_B * (pi_B_star - (gamma + dG * f + dB * m))^2 - (1 - mu_B) * (y_B_star - (alpha + bG * f + bB * m))^2
  return(uG + uB)
}

# Find cooperative fiscal and monetary policy (can be done via optimization)

cooperative_opt <- optimx(
  par = c(f = 0, m = 0), 
  fn = function(x) -cooperative_utility(x[1], x[2]),  # Negative because optimx minimizes
  method = "BFGS"
)

# Extract cooperative fiscal and monetary policies
f_cooperative <- cooperative_opt$f
m_cooperative <- cooperative_opt$m
cooperative_outcomes <- compute_inflation_output(f_cooperative, m_cooperative)
cat("\nCooperative Outcomes:\n")
print(cooperative_outcomes)

# Compare outcomes
outcomes_df <- data.frame(
  Scenario = c("Nash", "Precommitment", "Cooperative"),
  Fiscal_Policy = c(nash_result$fiscal, precommit_result$fiscal, f_cooperative),
  Monetary_Policy = c(nash_result$monetary, precommit_result$monetary, m_cooperative),
  Output = c(nash_outcomes$output, precommit_outcomes$output, cooperative_outcomes$output),
  Inflation = c(nash_outcomes$inflation, precommit_outcomes$inflation, cooperative_outcomes$inflation)
)


# Reshape fiscal and monetary policy data for graphs
policy_df_long <- outcomes_df %>%
  pivot_longer(cols = c(Fiscal_Policy, Monetary_Policy), 
               names_to = "Policy_Type", values_to = "Value")

# Plot fiscal and monetary policies using position = "dodge"
ggplot(policy_df_long, aes(x = Scenario, y = Value, fill = Policy_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(title = "Fiscal and Monetary Policy Across Scenarios", 
       y = "Policy Tightness", x = "Scenario") +
  theme_minimal()

save_e61("Relative_tightness.png",pad_width = 1,res=2)

# Reshape output and inflation data for graphs
outcome_df_long <- outcomes_df %>%
  pivot_longer(cols = c(Output, Inflation), 
               names_to = "Outcome_Type", values_to = "Value")

# Plot output and inflation using position = "dodge"
ggplot(outcome_df_long, aes(x = Scenario, y = Value, fill = Outcome_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(title = "Output and Inflation Across Scenarios", 
       y = "Outcome Values", x = "Scenario") +
  theme_minimal()

save_e61("Outcomes.png",pad_width = 1,res=2)
