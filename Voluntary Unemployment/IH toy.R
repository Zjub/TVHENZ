# Define parameters
L <- 0.5
d <- 0.5 # Probability of wage decline
w_new <- 1
b <- 0.5
delta <- 0.95
decline_rate <- 0.1 # Wage decline rate
C <- 0.2
mc <- 0.4

# Adjusted cost function
c_e <- function(e) {
  ifelse(e > 0, C + mc*e^2, 0)
}

# Define the state space for the wage while staying
wage_grid <- seq(0, 2, by = 0.1) # Example wage grid from 0 to 2 - adjust based on values set for w_stay and w_new
V_stay <- rep(0, length(wage_grid)) # Initialize value function for staying
V_leave <- rep(0, length(wage_grid)) # Initialize value function for leaving

# Bellman update function - these are only two period (and are also incorrect for that due to the wrong return on job loss), so need to adjust
bellman_update <- function(V_stay, V_leave, w_stay, e) {
  # Staying update
  stay_utility <- (w_stay - c_e(e) + delta * ((1 - L) * w_new + L * e * w_new))
  # Leaving update
  leave_utility <- (b - C + delta * (e * w_new + (1 - e) * b))
  return(list(stay = stay_utility, leave = leave_utility))
}

# Value function iteration
tolerance <- 1e-6
max_iter <- 1000
converged <- FALSE
iter <- 1

while(!converged && iter <= max_iter) {
  V_stay_old <- V_stay
  V_leave_old <- V_leave

  for (i in 1:length(wage_grid)) {
    # For each wage level, find the effort level that maximizes utility
    w_stay <- wage_grid[i]

    # Optimize effort for staying
    optimize_result_stay <- optimize(function(e) -bellman_update(V_stay, V_leave, w_stay, e)$stay, c(0, 1))
    V_stay[i] <- -optimize_result_stay$objective

    # Optimize effort for leaving
    optimize_result_leave <- optimize(function(e) -bellman_update(V_stay, V_leave, w_stay, e)$leave, c(0, 1))
    V_leave[i] <- -optimize_result_leave$objective
  }

  # Check convergence
  if(max(abs(V_stay - V_stay_old)) < tolerance && max(abs(V_leave - V_leave_old)) < tolerance) {
    converged <- TRUE
  }

  iter <- iter + 1
}

# Policy function (which choice is better for each wage level)
policy <- ifelse(V_stay >= V_leave, 1, 0)

# Simulation over periods
periods <- 300
choices <- numeric(periods) # 1 for stay, 0 for leave
efforts <- numeric(periods) # Optimal effort
w_stay <- 1 # Initial wage for staying

# Now, we can derive the optimal policy (effort level) for each wage grid point

optimal_effort_stay <- sapply(1:length(wage_grid), function(idx) {
  optimize(function(e) -bellman_update(V_stay, V_leave, wage_grid[idx], e)$stay, c(0, 1))$minimum
})

optimal_effort_leave <- sapply(1:length(wage_grid), function(idx) {
  optimize(function(e) -bellman_update(V_stay, V_leave, wage_grid[idx], e)$leave, c(0, 1))$minimum
})

# Simulation over periods
for (t in 1:periods) {
  # Find closest wage level in the wage grid
  closest_wage_index <- which.min(abs(wage_grid - w_stay))

  # Decision to stay or leave based on the policy function
  decision <- policy[closest_wage_index]
  choices[t] <- decision

  # Determine optimal effort based on the decision to stay or leave
  if (decision == 1) { # Stay
    efforts[t] <- optimal_effort_stay[closest_wage_index] # Optimal effort for staying
    # Update wage for next period based on probability of decline
    w_stay <- ifelse(runif(1) < d, w_stay * (1 - decline_rate), w_stay)
  } else { # Leave
    efforts[t] <- optimal_effort_leave[closest_wage_index] # Optimal effort for leaving
    # As effort defines the probability of being rehired, update wage accordingly
    if(runif(1) < efforts[t]) {
      w_stay <- w_new # If new job found
    } else {
      w_stay <- b # If no new job found, fall back to outside option
    }
  }
}


# Plotting
par(mfrow = c(2, 1)) # Set up plot area for two plots
plot(1:periods, choices, type = 'h', lwd = 2, col = 'blue', ylab = "Choice (1=Stay, 0=Leave)", xlab = "Period", main = "Choice Over Time")
plot(1:periods, efforts, type = 'l', lwd = 2, col = 'red', ylab = "Optimal Effort", xlab = "Period", main = "Optimal Effort Over Time",ylim = c(0,1))
