### A game where there are two periods of choices, but it is costly to adjust choices.
## This is not working, derive the BR functions manually.

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

# Parameters for the model (two periods)
gamma <- 1.5  # Intercept for inflation
alpha <- 2    # Intercept for output
bG <- 0.5     # Sensitivity of output to fiscal policy
bB <- 0.5     # Sensitivity of output to monetary policy
dG <- 0.5     # Sensitivity of inflation to fiscal policy
dB <- 0.5     # Sensitivity of inflation to monetary policy

# Weights for inflation in the loss functions
mu_G <- 0.9  # Government's weight on inflation
mu_B <- 0.9  # Central bank's weight on inflation

# Target inflation and output
pi_G_star <- 2
pi_B_star <- 2
y_G_star <- 3
y_B_star <- 3

# Precommitment costs for adjusting between periods
cost_G_precommit <- 0.5
cost_B_precommit <- 0.5

# Compute inflation and output for a given fiscal and monetary policy in a period
compute_inflation_output <- function(f, m) {
  y <- alpha + bG * f + bB * m
  pi <- gamma + dG * f + dB * m
  return(list(output = y, inflation = pi))
}

### 1. Loss Function Definitions ###
# Government's loss function over two periods
government_loss <- function(f1, f2, m1, m2) {
  # Compute outputs and inflation for both periods
  outcomes1 <- compute_inflation_output(f1, m1)
  outcomes2 <- compute_inflation_output(f2, m2)
  
  # Government loss across two periods plus cost for changing fiscal policy
  L_G <- mu_G * ((pi_G_star - outcomes1$inflation)^2 + (pi_G_star - outcomes2$inflation)^2) + 
    (1 - mu_G) * ((y_G_star - outcomes1$output)^2 + (y_G_star - outcomes2$output)^2) + 
    cost_G_precommit * (f2 - f1)^2
  return(L_G)
}

# Central bank's loss function over two periods
central_bank_loss <- function(f1, f2, m1, m2) {
  # Compute outputs and inflation for both periods
  outcomes1 <- compute_inflation_output(f1, m1)
  outcomes2 <- compute_inflation_output(f2, m2)
  
  # Central bank loss across two periods plus cost for changing monetary policy
  L_B <- mu_B * ((pi_B_star - outcomes1$inflation)^2 + (pi_B_star - outcomes2$inflation)^2) + 
    (1 - mu_B) * ((y_B_star - outcomes1$output)^2 + (y_B_star - outcomes2$output)^2) + 
    cost_B_precommit * (m2 - m1)^2
  return(L_B)
}

### 2. Best Response Functions for Two Periods ###
### Redefining Central Bank's Best Response Function Over Two Periods
BR_B_two_periods <- function(f1, f2) {
  # Central bank optimizes for both m1 and m2 given f1 and f2
  effective_loss_B <- function(m) {
    # Compute the central bank's loss function
    return(central_bank_loss(f1, f2, m[1], m[2]))
  }
  
  # Optimizing monetary policies m1 and m2 given f1 and f2
  optim_B <- optim(par = c(0, 0), fn = effective_loss_B, method = "BFGS")
  m1_star <- optim_B$par[1]
  m2_star <- optim_B$par[2]
  
  return(list(m1 = m1_star, m2 = m2_star))
}

### Redefining Government's Best Response Function Over Two Periods
BR_G_two_periods <- function(m1, m2) {
  # Government optimizes for both f1 and f2 given m1 and m2
  effective_loss_G <- function(f) {
    # Compute the government's loss function
    return(government_loss(f[1], f[2], m1, m2))
  }
  
  # Optimizing fiscal policies f1 and f2 given m1 and m2
  optim_G <- optim(par = c(0, 0), fn = effective_loss_G, method = "BFGS")
  f1_star <- optim_G$par[1]
  f2_star <- optim_G$par[2]
  
  return(list(f1 = f1_star, f2 = f2_star))
}

### 3. Nash Equilibrium over Two Periods ###
nash_equilibrium_two_periods <- function(tol = 1e-6, max_iter = 1000) {
  # Random initial guesses for fiscal and monetary policies in both periods
  f1 <- runif(1, -2, 2); f2 <- runif(1, -2, 2)
  m1 <- runif(1, -2, 2); m2 <- runif(1, -2, 2)
  
  for (i in 1:max_iter) {
    # Government optimizes given the central bank's monetary policies in both periods
    f_new <- BR_G_two_periods(m1, m2)
    
    # Central bank optimizes given the government's fiscal policies in both periods
    m_new <- BR_B_two_periods(f_new$f1, f_new$f2)
    
    # Check for convergence
    if (max(abs(c(f_new$f1 - f1, f_new$f2 - f2, m_new$m1 - m1, m_new$m2 - m2))) < tol) {
      break
    }
    
    # Update policies
    f1 <- f_new$f1; f2 <- f_new$f2
    m1 <- m_new$m1; m2 <- m_new$m2
  }
  
  return(list(fiscal1 = f1, fiscal2 = f2, monetary1 = m1, monetary2 = m2))
}

### 4. Precommitment Scenarios ###
## Add second stage best responses
# Government's best response to monetary policy choice by the central bank
BR_G <- function(m) {
  numerator <- bG * (1 - mu_G) * (y_G_star - alpha + bB*m) - dG * mu_G * (pi_G_star - gamma + dB*m)
  denominator <- ((bG^2) * (1 - mu_G) + (dG^2) * mu_G)
  f_star <- numerator / denominator
  return(f_star)
}

# Central bank's best response to fiscal policy choice by the government
BR_B <- function(f) {
  numerator <- bB * (1 - mu_B) * (y_B_star - alpha + bG*f) - dB * mu_B * (pi_B_star - gamma + dG*f)
  denominator <- ((bB^2) * (1 - mu_B) + (dB^2) * mu_B)
  m_star <- numerator / denominator
  return(m_star)
}


# Precommitment: Government precommits to both periods' policies
precommitment_government_two_periods <- function() {
  # Define the effective loss function for the government across two periods
  effective_loss_G <- function(f) {
    # Central bank's response to the fiscal policy choice f
    m1_star <- BR_B(f[1])
    m2_star <- BR_B(f[2])
    
    # Government's loss function given f and the central bank's response m_star
    return(government_loss(f[1], f[2], m1_star, m2_star))
  }
  
  # Government optimizes its fiscal policies for both periods, knowing the central bank's response
  optim_G <- optim(par = c(0, 0), fn = effective_loss_G, method = "BFGS")
  f_star <- optim_G$par
  
  # Central bank's best response to the government's precommitted policies
  m_star <- BR_B_two_periods(f_star[1], f_star[2])
  
  return(list(fiscal1 = f_star[1], fiscal2 = f_star[2], monetary1 = m_star$m1, monetary2 = m_star$m2))
}

# Precommitment: Central bank precommits to both periods' policies
precommitment_central_bank_two_periods <- function() {
  # Define the effective loss function for the central bank across two periods
  effective_loss_B <- function(m) {
    # Government's response to the monetary policy choice m
    f1_star <- BR_G(m[1])
    f2_star <- BR_G(m[2])
    
    # Central bank's loss function given m and the government's response f_star
    return(central_bank_loss(f1_star, f2_star, m[1], m[2]))
  }
  
  # Central bank optimizes its monetary policies for both periods, knowing the government's response
  optim_B <- optim(par = c(0, 0), fn = effective_loss_B, method = "BFGS")
  m_star <- optim_B$par
  
  # Government's best response to the central bank's precommitted policies
  f_star <- BR_G_two_periods(m_star[1], m_star[2])
  
  return(list(fiscal1 = f_star$f1, fiscal2 = f_star$f2, monetary1 = m_star[1], monetary2 = m_star[2]))
}

# Precommitment Game: Both players precommit independently to both periods' policies
precommit_both_independent_two_periods <- function(tol = 1e-6, max_iter = 1000) {
  # Initialize fiscal and monetary policy guesses
  f1 <- runif(1, -2, 2); f2 <- runif(1, -2, 2)
  m1 <- runif(1, -2, 2); m2 <- runif(1, -2, 2)
  
  for (i in 1:max_iter) {
    # Government optimizes fiscal policy given central bank's monetary policies
    f_new <- BR_G_two_periods(m1, m2)
    
    # Central bank optimizes monetary policy given government's fiscal policies
    m_new <- BR_B_two_periods(f_new$f1, f_new$f2)
    
    # Check for convergence
    if (max(abs(c(f_new$f1 - f1, f_new$f2 - f2, m_new$m1 - m1, m_new$m2 - m2))) < tol) {
      break
    }
    
    # Update policies
    f1 <- f_new$f1; f2 <- f_new$f2
    m1 <- m_new$m1; m2 <- m_new$m2
  }
  
  return(list(fiscal1 = f1, fiscal2 = f2, monetary1 = m1, monetary2 = m2))
}

### 5. Cooperative Outcome ###
cooperative_utility_two_periods <- function(f, m) {
  return(government_loss(f[1], f[2], m[1], m[2]) + central_bank_loss(f[1], f[2], m[1], m[2]))
}

cooperative_outcome_two_periods <- function() {
  # Optimize the joint loss function
  optim_coop <- optim(par = c(0, 0, 0, 0), fn = function(x) cooperative_utility_two_periods(c(x[1], x[2]), c(x[3], x[4])), method = "BFGS")
  f_star <- optim_coop$par[1:2]
  m_star <- optim_coop$par[3:4]
  return(list(fiscal1 = f_star[1], fiscal2 = f_star[2], monetary1 = m_star[1], monetary2 = m_star[2]))
}

### 6. Simulate and Compare Outcomes ###
# Nash Equilibrium
nash_result <- nash_equilibrium_two_periods()

# Government Precommits
precommit_result_gov <- precommitment_government_two_periods()

# Central Bank Precommits
precommit_result_cb <- precommitment_central_bank_two_periods()

# Both Players Precommit Independently
precommit_result_both <- precommit_both_independent_two_periods()

# Cooperative Outcome
cooperative_result <- cooperative_outcome_two_periods()

### 7. Create DataFrame for Outcomes Comparison ###
outcomes_df <- data.frame(
  Scenario = c("Nash", "Government Precommits", "Central Bank Precommits", "Both Precommit", "Cooperative"),
  Fiscal_Policy_Period1 = c(nash_result$fiscal1, precommit_result_gov$fiscal1, precommit_result_cb$fiscal1, precommit_result_both$fiscal1, cooperative_result$fiscal1),
  Fiscal_Policy_Period2 = c(nash_result$fiscal2, precommit_result_gov$fiscal2, precommit_result_cb$fiscal2, precommit_result_both$fiscal2, cooperative_result$fiscal2),
  Monetary_Policy_Period1 = c(nash_result$monetary1, precommit_result_gov$monetary1, precommit_result_cb$monetary1, precommit_result_both$monetary1, cooperative_result$monetary1),
  Monetary_Policy_Period2 = c(nash_result$monetary2, precommit_result_gov$monetary2, precommit_result_cb$monetary2, precommit_result_both$monetary2, cooperative_result$monetary2)
)

# Print the outcomes for comparison
print(outcomes_df)
