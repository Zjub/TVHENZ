# Load necessary libraries
library(ggplot2)

# Define parameters with realistic scaling and ensuring weight differences are reflected
alpha <- 1      # Constant for output
gamma <- 1      # Constant for inflation
bG <- 0.6       # Sensitivity of output to fiscal policy
bB <- 0.6       # Sensitivity of output to monetary policy
dG <- 0.6       # Sensitivity of inflation to fiscal policy
dB <- 0.6       # Sensitivity of inflation to monetary policy
muG <- 0.3      # Government weight on inflation
muB <- 0.8      # Central bank weight on inflation (different from muG)

# Target values
yG_star <- 2
yB_star <- 2
piG_star <- 2
piB_star <- 2

# Government's best response function
best_response_gov <- function(m) {
  num <- -muG * dG * (piG_star - gamma + dB * m) - (1 - muG) * bG * (yG_star - alpha + bB * m)
  den <- muG * dG^2 + (1 - muG) * bG^2
  return(num / den)
}

# Central bank's best response function
best_response_cb <- function(f) {
  num <- -muB * dB * (piB_star - gamma + dG * f) - (1 - muB) * bB * (yB_star - alpha + bG * f)
  den <- muB * dB^2 + (1 - muB) * bB^2
  return(num / den)
}

# Solve for Nash Equilibrium by iterating best response functions
nash_equilibrium <- function(tol = 1e-4, max_iter = 500) {
  f_old <- 0.5   # Initial guess for fiscal policy
  m_old <- -0.5  # Initial guess for monetary policy
  
  for (i in 1:max_iter) {
    f_new <- best_response_gov(m_old)
    m_new <- best_response_cb(f_new)
    
    if (abs(f_new - f_old) < tol && abs(m_new - m_old) < tol) {
      cat("Converged after", i, "iterations.\n")
      return(c(f_new, m_new))
    }
    
    f_old <- f_new
    m_old <- m_new
  }
  
  stop("Did not converge after", max_iter, "iterations.")
}

# Calculate Nash Equilibrium
nash_eq <- nash_equilibrium()
f_ne <- nash_eq[1]
m_ne <- nash_eq[2]
cat("Nash Equilibrium:\n Fiscal Policy (f*) =", f_ne, "\n Monetary Policy (m*) =", m_ne, "\n")

# Generate data for plotting the best response functions
m_values <- seq(-1, 1, by = 0.05)
f_values_gov <- sapply(m_values, best_response_gov)
f_values <- seq(-1, 1, by = 0.05)
m_values_cb <- sapply(f_values, best_response_cb)

plot_df_gov <- data.frame(m_values, f_values_gov)
plot_df_cb <- data.frame(f_values, m_values_cb)

# Plot the best response functions and the Nash equilibrium
ggplot() +
  geom_line(data = plot_df_gov, aes(x = f_values_gov, y = m_values), color = "blue", size = 1, linetype = "solid") +
  geom_line(data = plot_df_cb, aes(x = f_values, y = m_values_cb), color = "red", size = 1, linetype = "dashed") +
  geom_point(aes(x = m_ne, y = f_ne), color = "black", size = 3) +
  labs(x = "Monetary Policy (m)", y = "Fiscal Policy (f)", 
       title = "Best Response Functions and Nash Equilibrium (With Expanded Range)") +
  annotate("text", x = m_ne, y = f_ne, label = "Nash Equilibrium", vjust = -1, hjust = -0.5) +
  theme_minimal() 
