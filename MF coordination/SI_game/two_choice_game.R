gc()
rm(list=ls())
library(haven)
library(data.table)
library(tidyverse)
library(gtsummary)
library(zoo)
library(ggplot2)
#library(theme61) # comment out of non-e61 employee
library(fixest)
library(skimr)
library(modelsummary)
library(optimx)
library(nleqslv)

set.seed(123)

# Parameters for the model
gamma <- 1.5  # Intercept for inflation
alpha <- 2.0  # Intercept for output
bG <- 0.5     # Sensitivity of output to fiscal policy
bB <- 0.5     # Sensitivity of output to monetary policy
dG <- 0.5     # Sensitivity of inflation to fiscal policy
dB <- 0.5     # Sensitivity of inflation to monetary policy
eG <- 0.1     # Penalty factor on change in fiscal policy
eB <- 0.1     # Penalty factor on change in monetary policy

# Weights for inflation in the loss functions
mu_G <- 0  # Government's weight on inflation
mu_B <- 1  # Central bank's weight on inflation
theta_G_star <- 0.5  # Government's weight on instrument preference
theta_B_star <- 0.5  # Central bank's weight on instrument preference

# Target inflation and output
pi_G_star <- 2  # Government's target inflation
pi_B_star <- 2  # Central bank's target inflation
y_G_star <- 2   # Government's target output
y_B_star <- 2   # Central bank's target output

# Preferred policies
beta_G_star <- 0  # Government's preferred fiscal policy
beta_B_star <- 0  # Central bank's preferred monetary policy

# Adjustment cost parameters
lambda_G <- 0.5  # Penalty factor for changes in fiscal policy
lambda_B <- 0.5  # Penalty factor for changes in monetary policy

# Stage 2: Best response functions for f2 and m2 with adjustment costs

# Government's best response for f2, given m2 and f1
BR_G_stage2 <- function(m2, f1) {
  numerator <- bG * (1 - mu_G) * (y_G_star - alpha + bB * m2) + 
    dG * mu_G * (pi_G_star - gamma + dB * m2) + 
    theta_G_star * beta_G_star + 
    lambda_G * f1
  
  denominator <- bG^2 * (1 - mu_G) + dG^2 * mu_G + theta_G_star + lambda_G
  f2_star <- numerator / denominator
  return(f2_star)
}

# Central bank's best response for m2, given f2 and m1
BR_B_stage2 <- function(f2, m1) {
  numerator <- bB * (1 - mu_B) * (y_B_star - alpha + bG * f2) + 
    dB * mu_B * (pi_B_star - gamma + dG * f2) + 
    theta_B_star * beta_B_star + 
    lambda_B * m1
  
  denominator <- bB^2 * (1 - mu_B) + dB^2 * mu_B + theta_B_star + lambda_B
  m2_star <- numerator / denominator
  return(m2_star)
}

# Government's best response for f1, considering its impact on f2 and m2
BR_G_stage1 <- function(m1) {
  # Minimize total loss for government (including second stage's responses)
  effective_loss_G <- function(f1) {
    # Start with an initial guess for f2
    f2_guess <- BR_G_stage2(0, f1)  # Initial guess for f2, with m2 = 0
    m2 <- BR_B_stage2(f2_guess, m1)  # Calculate m2 based on initial f2
    
    # Iterate to update f2 and m2 until convergence
    tolerance <- 1e-6  # Set tolerance level for convergence
    max_iter <- 100    # Maximum number of iterations allowed
    iter <- 0
    while (iter < max_iter) {
      f2_new <- BR_G_stage2(m2, f1)  # Update f2 based on the current m2
      m2_new <- BR_B_stage2(f2_new, m1)  # Update m2 based on the new f2
      
      # Check for convergence
      if (abs(f2_new - f2_guess) < tolerance && abs(m2_new - m2) < tolerance) {
        break  # If both f2 and m2 converge, stop the loop
      }
      
      # Update f2 and m2 for the next iteration
      f2_guess <- f2_new
      m2 <- m2_new
      iter <- iter + 1
    }
    
    # Stage 1 loss
    L_G_stage1 <- mu_G * (pi_G_star - (gamma - dG * f1 - dB * m1))^2 +
      (1 - mu_G) * (y_G_star - (alpha - bG * f1 - bB * m1))^2 +
      theta_G_star * (beta_G_star - eG*f1)^2
    
    # Stage 2 loss
    L_G_stage2 <- mu_G * (pi_G_star - (gamma - dG * f2_guess - dB * m2))^2 +
      (1 - mu_G) * (y_G_star - (alpha - bG * f2_guess - bB * m2))^2 +
      theta_G_star * (beta_G_star - eG*f2_guess)^2 +
      lambda_G * (f2_guess - f1)^2  # Adjustment cost
    
    return(L_G_stage1 + L_G_stage2)
  }
  
  # Find f1 that minimizes total loss
  opt_result <- optim(par = 0, fn = effective_loss_G, method = "BFGS")
  return(opt_result$par)
}

# Central bank's best response for m1, considering its impact on f2 and m2
BR_B_stage1 <- function(f1) {
  # Minimize total loss for central bank (including second stage's responses)
  effective_loss_B <- function(m1) {
    # Start with an initial guess for f2
    f2_guess <- BR_G_stage2(0, f1)  # Initial guess for f2, with m2 = 0
    m2 <- BR_B_stage2(f2_guess, m1)  # Calculate m2 based on initial f2
    
    # Iterate to update f2 and m2 until convergence
    tolerance <- 1e-6  # Set tolerance level for convergence
    max_iter <- 1000    # Maximum number of iterations allowed
    iter <- 0
    while (iter < max_iter) {
      f2_new <- BR_G_stage2(m2, f1)  # Update f2 based on the current m2
      m2_new <- BR_B_stage2(f2_new, m1)  # Update m2 based on the new f2
      
      # Check for convergence
      if (abs(f2_new - f2_guess) < tolerance && abs(m2_new - m2) < tolerance) {
        break  # If both f2 and m2 converge, stop the loop
      }
      
      # Update f2 and m2 for the next iteration
      f2_guess <- f2_new
      m2 <- m2_new
      iter <- iter + 1
    }
    
    # Stage 1 loss
    L_B_stage1 <- mu_B * (pi_B_star - (gamma - dG * f1 - dB * m1))^2 +
      (1 - mu_B) * (y_B_star - (alpha - bG * f1 - bB * m1))^2 +
      theta_B_star * (beta_B_star - eB*m1)^2
    
    # Stage 2 loss
    L_B_stage2 <- mu_B * (pi_B_star - (gamma - dG * f2_guess - dB * m2))^2 +
      (1 - mu_B) * (y_B_star - (alpha - bG * f2_guess - bB * m2))^2 +
      theta_B_star * (beta_B_star - eB*m2)^2 +
      lambda_B * (m2 - m1)^2  # Adjustment cost
    
    return(L_B_stage1 + L_B_stage2)
  }
  
  # Find m1 that minimizes total loss
  opt_result <- optim(par = 0, fn = effective_loss_B, method = "BFGS")
  return(opt_result$par)
}


# Dynamic loss function for the government, considering both stages and adjustment costs
effective_loss_G_dynamic <- function(f1, m1, f2, m2) {
  # Stage 1 loss
  L_G_stage1 <- mu_G * (pi_G_star - (gamma - dG * f1 - dB * m1))^2 +
    (1 - mu_G) * (y_G_star - (alpha - bG * f1 - bB * m1))^2 +
    theta_G_star * (beta_G_star - eG*f1)^2
  
  # Stage 2 loss with adjustment costs
  L_G_stage2 <- mu_G * (pi_G_star - (gamma - dG * f2 - dB * m2))^2 +
    (1 - mu_G) * (y_G_star - (alpha - bG * f2 - bB * m2))^2 +
    theta_G_star * (beta_G_star - eG*f2)^2 +
    lambda_G * (f2 - f1)^2  # Adjustment cost for fiscal policy
  
  return(list(total_loss = L_G_stage1 + L_G_stage2, stage1_loss = L_G_stage1, stage2_loss = L_G_stage2))
}

# Dynamic loss function for the central bank, considering both stages and adjustment costs
effective_loss_B_dynamic <- function(m1, f1, m2, f2) {
  # Stage 1 loss
  L_B_stage1 <- mu_B * (pi_B_star - (gamma - dG * f1 - dB * m1))^2 +
    (1 - mu_B) * (y_B_star - (alpha - bG * f1 - bB * m1))^2 +
    theta_B_star * (beta_B_star - eB*m1)^2
  
  # Stage 2 loss with adjustment costs
  L_B_stage2 <- mu_B * (pi_B_star - (gamma - dG * f2 - dB * m2))^2 +
    (1 - mu_B) * (y_B_star - (alpha - bG * f2 - bB * m2))^2 +
    theta_B_star * (beta_B_star - eB*m2)^2 +
    lambda_B * (m2 - m1)^2  # Adjustment cost for monetary policy
  
  return(list(total_loss = L_B_stage1 + L_B_stage2, stage1_loss = L_B_stage1, stage2_loss = L_B_stage2))
}


# Dynamic game solution: Solve for the first-stage policies and compute second-stage responses -- DO THIS ANALYTICALLY NEXT TIME (just low on time)
dynamic_game_solution <- function() {
  # Set initial guesses for f1, m1, f2, and m2
  f1_guess <- 0  # Initial guess for f1
  m1_guess <- 0  # Initial guess for m1
  f2_guess <- runif(1, -1, 1)  # Random initial guess for f2
  m2_guess <- runif(1, -1, 1)  # Random initial guess for m2
  
  # Iteration control
  tolerance <- 1e-5
  max_iter <- 100
  iter <- 0
  converged <- FALSE
  
  while (iter < max_iter && !converged) {
    # Update f1 based on the current m1
    f1_new <- BR_G_stage1(m1_guess)
    
    # Update m1 based on the current f1
    m1_new <- BR_B_stage1(f1_new)
    
    # Update f2 based on the current m2 and f1
    f2_new <- BR_G_stage2(m2_guess, f1_new)
    
    # Update m2 based on the current f2 and m1
    m2_new <- BR_B_stage2(f2_new, m1_new)
    
    # Check for convergence
    if (abs(f1_new - f1_guess) < tolerance &&
        abs(m1_new - m1_guess) < tolerance &&
        abs(f2_new - f2_guess) < tolerance &&
        abs(m2_new - m2_guess) < tolerance) {
      converged <- TRUE  # All four terms have converged
    }
    
    # Update guesses for the next iteration
    f1_guess <- f1_new
    m1_guess <- m1_new
    f2_guess <- f2_new
    m2_guess <- m2_new
    iter <- iter + 1
  }
  
  if (!converged) {
    warning("Solution did not converge within the maximum number of iterations")
  }
  
  # Compute output and inflation for both stages after convergence
  output1 <- alpha - bG * f1_guess - bB * m1_guess
  inflation1 <- gamma - dG * f1_guess - dB * m1_guess
  
  output2 <- alpha - bG * f2_guess - bB * m2_guess
  inflation2 <- gamma - dG * f2_guess - dB * m2_guess
  
  # Create a data frame for inflation and output results
  results_df <- data.frame(
    Stage = c("Stage 1", "Stage 2"),
    Output = c(output1, output2),
    Inflation = c(inflation1, inflation2)
  )
  
  # Compute losses for both agents after convergence
  loss_G <- effective_loss_G_dynamic(f1_guess, m1_guess,f2_guess, m2_guess)
  loss_B <- effective_loss_B_dynamic(m1_guess, f1_guess,m2_guess, f2_guess)
  
  return(list(fiscal1 = f1_guess, monetary1 = m1_guess, fiscal2 = f2_guess, monetary2 = m2_guess,
              output1 = output1, inflation1 = inflation1, output2 = output2, inflation2 = inflation2,
              loss_G = loss_G, loss_B = loss_B, iterations = iter, converged = converged))
}

# Cooperative utility function (combined loss of the government and the central bank)
cooperative_loss <- function(params) {
  f1 <- params[1]
  m1 <- params[2]
  f2 <- params[3]
  m2 <- params[4]
  
  # Government's total loss
  gov_loss <- effective_loss_G_dynamic(f1, m1, f2, m2)$total_loss
  # Central bank's total loss
  cb_loss <- effective_loss_B_dynamic(m1, f1, m2, f2)$total_loss
  
  # The cooperative loss is the sum of both losses
  return(gov_loss + cb_loss)
}

# Solve for the cooperative outcome
solve_cooperative <- function() {
  # Initial guesses for f1, m1, f2, and m2
  initial_guess <- c(0, 0, 0, 0)
  
  # Use optim to minimize the combined loss function
  opt_result <- optim(par = initial_guess, fn = cooperative_loss, method = "BFGS")
  
  # Extract the optimized values
  f1_coop <- opt_result$par[1]
  m1_coop <- opt_result$par[2]
  f2_coop <- opt_result$par[3]
  m2_coop <- opt_result$par[4]
  
  # Compute output and inflation for both stages
  output1_coop <- alpha - bG * f1_coop - bB * m1_coop
  inflation1_coop <- gamma - dG * f1_coop - dB * m1_coop
  
  output2_coop <- alpha - bG * f2_coop - bB * m2_coop
  inflation2_coop <- gamma - dG * f2_coop - dB * m2_coop
  
  # Compute losses for both agents after convergence
  loss_G_coop <- effective_loss_G_dynamic(f1_coop, m1_coop, f2_coop, m2_coop)
  loss_B_coop <- effective_loss_B_dynamic(m1_coop, f1_coop, m2_coop, f2_coop)
  
  return(list(fiscal1 = f1_coop, monetary1 = m1_coop, fiscal2 = f2_coop, monetary2 = m2_coop,
              output1 = output1_coop, inflation1 = inflation1_coop, output2 = output2_coop, inflation2 = inflation2_coop,
              loss_G = loss_G_coop, loss_B = loss_B_coop))
}

# Loss function for the government in static Nash
static_loss_G <- function(f_static, m_static) {
  L_G_static <- mu_G * (pi_G_star - (gamma - dG * f_static - dB * m_static))^2 +
    (1 - mu_G) * (y_G_star - (alpha - bG * f_static - bB * m_static))^2 +
    theta_G_star * (beta_G_star - eG*f_static)^2
  return(L_G_static)
}

# Loss function for the central bank in static Nash
static_loss_B <- function(f_static, m_static) {
  L_B_static <- mu_B * (pi_B_star - (gamma - dG * f_static - dB * m_static))^2 +
    (1 - mu_B) * (y_B_star - (alpha - bG * f_static - bB * m_static))^2 +
    theta_B_star * (beta_B_star - eB*m_static)^2
  return(L_B_static)
}


# Best response functions for static Nash (without adjustment costs)
BR_G_static <- function(m) {
  numerator <- -(bG * (1 - mu_G) * (y_G_star - alpha + bB*m) + dG * mu_G * (pi_G_star - gamma + dB*m) - theta_G_star*eG*beta_G_star) 
  denominator <- ((bG^2) * (1 - mu_G) + (dG^2) * mu_G + theta_G_star*(eG)^2)
  f_star <- numerator / denominator
  return(f_star)
}

# Central bank's best response to fiscal policy choice by the government
BR_B_static <- function(f) {
  numerator <- -(bB * (1 - mu_B) * (y_B_star - alpha + bG*f) + dB * mu_B * (pi_B_star - gamma + dG*f) - theta_B_star*eB*beta_B_star) 
  denominator <- ((bB^2) * (1 - mu_B) + (dB^2) * mu_B + theta_B_star*(eB)^2)
  m_star <- numerator / denominator
  return(m_star)
}

# Solve the static Nash equilibrium iteratively
solve_static_nash <- function() {
  # Initial guesses for f and m
  f_guess <- 0
  m_guess <- 0
  
  tolerance <- 1e-6
  max_iter <- 100
  iter <- 0
  converged <- FALSE
  
  while (iter < max_iter && !converged) {
    # Update f based on current m
    f_new <- BR_G_static(m_guess)
    
    # Update m based on current f
    m_new <- BR_B_static(f_new)
    
    # Check for convergence
    if (abs(f_new - f_guess) < tolerance && abs(m_new - m_guess) < tolerance) {
      converged <- TRUE
    }
    
    # Update guesses for the next iteration
    f_guess <- f_new
    m_guess <- m_new
    iter <- iter + 1
  }
  
  if (!converged) {
    warning("Static Nash equilibrium did not converge within the maximum number of iterations")
  }
  
  # Compute output and inflation for static Nash
  output_static <- alpha - bG * f_guess - bB * m_guess
  inflation_static <- gamma - dG * f_guess - dB * m_guess
  
  # Compute losses for static Nash
  loss_G_static <- static_loss_G(f_guess, m_guess)
  loss_B_static <- static_loss_B(f_guess, m_guess)
  
  return(list(fiscal_static = f_guess, monetary_static = m_guess, 
              output_static = output_static, inflation_static = inflation_static,
              loss_G_static = loss_G_static, loss_B_static = loss_B_static))
}

# Solve the dynamic game
result_dynamic <- dynamic_game_solution()

# Solve for the cooperative outcome
result_cooperative <- solve_cooperative()

# Solve for the static Nash equilibrium
result_static_nash <- solve_static_nash()

# Prepare data for loss comparison, including static Nash and cooperative outcome
loss_df <- data.frame(
  Agent = rep(c("Government", "Central Bank"), each = 4),
  Stage = rep(c("Stage 1", "Stage 2", "Cooperative", "Static Nash"), 2),
  Loss = c(result_dynamic$loss_G$stage1_loss, result_dynamic$loss_G$stage2_loss, 
           result_cooperative$loss_G$total_loss, result_static_nash$loss_G_static,
           result_dynamic$loss_B$stage1_loss, result_dynamic$loss_B$stage2_loss, 
           result_cooperative$loss_B$total_loss, result_static_nash$loss_B_static)
)

# Plot the losses for each agent in each stage, including cooperative and static Nash
ggplot(loss_df, aes(x = Stage, y = Loss, fill = Agent)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, na.rm = TRUE) +
  labs(title = "Losses for Government and Central Bank (Nash vs Cooperative vs Static Nash)", 
       y = "Loss Value", x = "Stage") +
  theme_minimal()

# Prepare data for policy comparison including static Nash and cooperative outcome
policy_df <- data.frame(
  Stage = c("Stage 1", "Stage 2", "Cooperative", "Static Nash"),
  Fiscal_Policy = c(result_dynamic$fiscal1, result_dynamic$fiscal2, 
                    result_cooperative$fiscal1, result_static_nash$fiscal_static),
  Monetary_Policy = c(result_dynamic$monetary1, result_dynamic$monetary2, 
                      result_cooperative$monetary1, result_static_nash$monetary_static)
)

# Reshape for plotting
policy_df_long <- policy_df %>%
  pivot_longer(cols = c(Fiscal_Policy, Monetary_Policy), names_to = "Policy_Type", values_to = "Value")

# Plot first-stage and second-stage policies, including cooperative and static Nash
ggplot(policy_df_long, aes(x = Stage, y = Value, fill = Policy_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(title = "Fiscal and Monetary Policy (Nash vs Cooperative vs Static Nash)", 
       y = "Policy Value", x = "Stage") +
  theme_minimal()

# Prepare data for inflation and output comparison including static Nash and cooperative outcome
results_df <- data.frame(
  Stage = c("Stage 1", "Stage 2", "Cooperative", "Static Nash"),
  Output = c(result_dynamic$output1, result_dynamic$output2, result_cooperative$output1, result_static_nash$output_static),
  Inflation = c(result_dynamic$inflation1, result_dynamic$inflation2, result_cooperative$inflation1, result_static_nash$inflation_static)
)

# Reshape for plotting inflation and output
results_long <- results_df %>%
  pivot_longer(cols = c(Output, Inflation), names_to = "Outcome_Type", values_to = "Value")

# Plot inflation and output outcomes, including cooperative and static Nash
ggplot(results_long, aes(x = Stage, y = Value, fill = Outcome_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(title = "Output and Inflation (Nash vs Cooperative vs Static Nash)", 
       y = "Outcome Value", x = "Stage") +
  theme_minimal()
