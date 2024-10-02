### Initial two stage commitment game to explain what we are thinking about with the paper
## Doesn't seem quite right
## This game intuitively won't solve - need a "cost of adjusting tightness" component to the loss function.

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
library(nleqslv)

set.seed(123)

# Parameters for the model
gamma <- 1.5  # Intercept for inflation [setting this below alpha creates a trade-off where inflation is lower than output]
alpha <- 1.5    # Intercept for output
bG <- 0.5     # Sensitivity of output to fiscal policy
bB <- 0.5     # Sensitivity of output to monetary policy
dG <- 0.5     # Sensitivity of inflation to fiscal policy
dB <- 0.5     # Sensitivity of inflation to monetary policy
eG <- 0.5     # Penalty factor on change in fiscal policy
eB <- 0.5     # Penalty factor on change in monetary policy

# New separate weights for inflation in the loss functions
mu_G <- 0  # Government's weight on inflation
mu_B <- 1  # Central bank's weight on inflation
theta_G_star <- 0.5 # Government's weight on instrument
theta_B_star <- 0.5 # Central bank's weight on instrument

# Target inflation and output and instrument
pi_G_star <- 2 # Government's target inflation
pi_B_star <- 2 # Central bank's target inflation
y_G_star <- 2 # Government's target output
y_B_star <- 2 # Central bank's target output
beta_G_star <- 0 # Government's preferred f
beta_B_star <- 0 # Central bank's preferred m


# Precommitment costs - not used, using the preference beta's to start. Transition to this idea
# cost_G_precommit <- 0.5
# cost_B_precommit <- 0.5

# Government's best response to monetary policy choice by the central bank
BR_G <- function(m) {
  numerator <- -(bG * (1 - mu_G) * (y_G_star - alpha + bB*m) + dG * mu_G * (pi_G_star - gamma + dB*m) - theta_G_star*eG*beta_G_star) 
  denominator <- ((bG^2) * (1 - mu_G) + (dG^2) * mu_G + theta_G_star*eG)
  f_star <- numerator / denominator
  return(f_star)
}

# Central bank's best response to fiscal policy choice by the government
BR_B <- function(f) {
  numerator <- -(bB * (1 - mu_B) * (y_B_star - alpha + bG*f) + dB * mu_B * (pi_B_star - gamma + dG*f) - theta_B_star*eB*beta_B_star) 
  denominator <- ((bB^2) * (1 - mu_B) + (dB^2) * mu_B + theta_B_star*eB)
  m_star <- numerator / denominator
  return(m_star)
}

# Generate values for plotting
m_values <- seq(-4, 4, by = 0.05)  # Range for monetary policy
f_values_gov <- sapply(m_values, BR_G)  # Fiscal response for given m

f_values <- seq(-4, 4, by = 0.05)  # Range for fiscal policy
m_values_cb <- sapply(f_values, BR_B)  # Monetary response for given f

# Create data frames for plotting
plot_df_gov <- data.frame(m_values = m_values, f_values_gov = f_values_gov)
plot_df_cb <- data.frame(f_values = f_values, m_values_cb = m_values_cb)

# Plot the best response functions
ggplot() +
  geom_line(data = plot_df_gov, aes(x = m_values, y = f_values_gov), color = "blue", size = 1, linetype = "solid") +
  geom_line(data = plot_df_cb, aes(x = m_values_cb, y = f_values), color = "red", size = 1, linetype = "dashed") +
  labs(x = "Monetary Policy (m)", y = "Fiscal Policy (f)", 
       title = "Best Response Functions: Government vs. Central Bank") +
  annotate("text", x = 0, y = 0, label = "Intersection = Nash Equilibrium", vjust = -1, hjust = -0.5) +
  theme_minimal() +
  xlim(-4, 4) + ylim(-4, 4)

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

# # Nash equilibrium analytically
# system_of_eqs <- function(vars) {
#   f <- vars[1]
#   m <- vars[2]
# 
#   # System of equations (f - BR_G(m) = 0 and m - BR_B(f) = 0)
#   eq1 <- f - BR_G(m)
#   eq2 <- m - BR_B(f)
#   
#   return(c(eq1, eq2))
# }
# 
# nash_equilibrium <- function() {
#   # Try multiple initial guesses
#   initial_guess <- c(-1,-1)  # Random initial guess for f and m between -2 and 2
#   
#   # Use the nleqslv solver with a different method and relaxed tolerance
#   solution <- nleqslv(initial_guess, system_of_eqs, control = list(xtol = 1e-6, ftol = 1e-6), method = "Newton")
#   
#   # Check for convergence
#   if (solution$termcd == 1) {  # Termcd == 1 means success
#     f_star <- solution$x[1]  # Fiscal policy equilibrium
#     m_star <- solution$x[2]  # Monetary policy equilibrium
#     
#     # Return results as a list
#     return(list(fiscal = f_star, monetary = m_star))
#   } else {
#     stop("The system did not converge to a solution.")
#   }
# }

# Compute inflation and output for a given fiscal and monetary policy
compute_inflation_output <- function(f, m) {
  y <- alpha - bG * f - bB * m
  pi <- gamma - dG * f - dB * m
  return(list(output = y, inflation = pi))
}

# Simulate the Nash equilibrium
nash_result <- nash_equilibrium()
nash_outcomes <- compute_inflation_output(nash_result$fiscal, nash_result$monetary)

## This isn't really precommitment - instead since the second mover will simply set their own target, the first mover is at a disadvantage.
# Precommitment: Government precommits, knowing the central bank's response
precommitment_government <- function() {
  effective_loss_G <- function(f) {
    m_star <- BR_B(f)
    L_G <- mu_G * (pi_G_star - (gamma - dG * f - dB * m_star))^2 + 
      (1 - mu_G) * (y_G_star - (alpha - bG * f - bB * m_star))^2 +
      theta_G_star*(beta_G_star - eG*f)^2
    return(L_G)
  }
  
  optim_G <- optim(par = 0, fn = effective_loss_G, method = "BFGS")
  f_star <- optim_G$par
  m_star <- BR_B(f_star)
  return(list(fiscal = f_star, monetary = m_star))
}

# Precommitment: Central bank precommits, knowing the government's response
precommitment_central_bank <- function() {
  effective_loss_B <- function(m) {
    f_star <- BR_G(m)
    L_B <- mu_B * (pi_B_star - (gamma - dG * f_star - dB * m))^2 + 
      (1 - mu_B) * (y_B_star - (alpha - bG * f_star - bB * m))^2 +
      theta_B_star*(beta_B_star - eB*m)^2
    return(L_B)
  }
  
  optim_B <- optim(par = 0, fn = effective_loss_B, method = "BFGS")
  m_star <- optim_B$par
  f_star <- BR_G(m_star)
  return(list(fiscal = f_star, monetary = m_star))
}

# Precommitment Game (Both Precommit): Each player simultaneously chooses their strategy [this just sets to the NE - need to correct]
precommit_both_independent <- function(tol = 1e-6, max_iter = 1000) {
  f <- runif(1, -2, 2)
  m <- runif(1, -2, 2)
  for (i in 1:max_iter) {
    f_new <- BR_G(m)
    m_new <- BR_B(f_new)
    if (abs(f_new - f) < tol && abs(m_new - m) < tol) {
      break
    }
    f <- f_new
    m <- m_new
  }
  return(list(fiscal = f, monetary = m))
}

# Simulate outcomes for different precommitment scenarios
precommit_result_gov <- precommitment_government()
precommit_outcomes_gov <- compute_inflation_output(precommit_result_gov$fiscal, precommit_result_gov$monetary)

precommit_result_cb <- precommitment_central_bank()
precommit_outcomes_cb <- compute_inflation_output(precommit_result_cb$fiscal, precommit_result_cb$monetary)

precommit_result_both_independent <- precommit_both_independent()
precommit_outcomes_both_independent <- compute_inflation_output(precommit_result_both_independent$fiscal, precommit_result_both_independent$monetary)

# Cooperative outcome: maximizing joint welfare
utility <- function(f, m) {
  uG <- -(mu_G * (pi_G_star - (gamma - dG * f - dB * m))^2 + (1 - mu_G) * (y_G_star - (alpha - bG * f - bB * m))^2 + theta_G_star*(beta_G_star - eG*f)^2)
  uB <- -(mu_B * (pi_B_star - (gamma - dG * f - dB * m))^2 + (1 - mu_B) * (y_B_star - (alpha - bG * f - bB * m))^2 + theta_B_star*(beta_B_star - eB*m)^2)
  return(uG + uB)
}

cooperative_opt <- optimx(
  par = c(f = 0, m = 0), 
  fn = function(x) -utility(x[1], x[2]),
  method = "BFGS"
)
f_cooperative <- cooperative_opt$f
m_cooperative <- cooperative_opt$m
cooperative_outcomes <- compute_inflation_output(f_cooperative, m_cooperative)

# Compare outcomes for all scenarios
outcomes_df <- data.frame(
  Scenario = c("Nash", "Government Precommits", "Central Bank Precommits", "Both Precommit", "Cooperative"),
  Fiscal_Policy = c(nash_result$fiscal, precommit_result_gov$fiscal, precommit_result_cb$fiscal, precommit_result_both_independent$fiscal, f_cooperative),
  Monetary_Policy = c(nash_result$monetary, precommit_result_gov$monetary, precommit_result_cb$monetary, precommit_result_both_independent$monetary, m_cooperative),
  Output = c(nash_outcomes$output, precommit_outcomes_gov$output, precommit_outcomes_cb$output, precommit_outcomes_both_independent$output, cooperative_outcomes$output),
  Inflation = c(nash_outcomes$inflation, precommit_outcomes_gov$inflation, precommit_outcomes_cb$inflation, precommit_outcomes_both_independent$inflation, cooperative_outcomes$inflation),
  Total_Utility = c(utility(nash_result$fiscal,nash_result$monetary),utility(precommit_result_gov$fiscal,precommit_result_gov$monetary),utility(precommit_result_cb$fiscal,precommit_result_cb$monetary),utility(precommit_result_both_independent$fiscal,precommit_result_both_independent$monetary),utility(f_cooperative,m_cooperative))
)

# Reshape fiscal and monetary policy data for graphs
policy_df_long <- outcomes_df %>%
  pivot_longer(cols = c(Fiscal_Policy, Monetary_Policy), 
               names_to = "Policy_Type", values_to = "Value")

# Plot fiscal and monetary policies using position = "dodge"
ggplot(policy_df_long, aes(x = Scenario, y = Value, fill = Policy_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(title = "Fiscal and Monetary Policy Across Scenarios", 
       y = "Policy Looseness", x = "Scenario") +
  theme_minimal()

#save_e61("Relative_Looseness.png", pad_width = 1, res = 2)

ggplot(policy_df_long, aes(x = Scenario, y = Total_Utility, fill = Policy_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(title = "Loss Across Scenarios", 
       y = "Policy Looseness", x = "Scenario") +
  theme_minimal()

# Reshape output and inflation data for graphs
outcome_df_long <- outcomes_df %>%
  pivot_longer(cols = c(Output, Inflation), 
               names_to = "Outcome_Type", values_to = "Value")

# Plot output and inflation using position = "dodge"
ggplot(outcome_df_long, aes(x = Scenario, y = Value, fill = Outcome_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(title = "Output and Inflation Across Scenarios", 
       y = "Outcome Values", x = "Scenario") +
  theme_minimal() + geom_hline(yintercept = pi_B_star,linetype = "dashed")

#save_e61("Outcomes.png", pad_width = 1, res = 2)

