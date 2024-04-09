# Date created: 9/04/2024
# Initial author: Matt Nolan
# Date last updated: 9/04/2024
# Last edit: Matt Nolan

# Initial Parameters
set.seed(123) # For reproducibility
initial_wage <- 1
b <- 0.4
delta <- 0.95
C <- 0.1
c <- 0.25
N <- 10
cost_leave <- 1
depreciation <- 0.1 # Wage decline through time

# Probability of job loss in each period, could be a vector of probabilities
L <- rep(0.2, N) # Example: Constant probability; can be replaced with a varying vector

# Function to calculate cost
c_e <- function(e, state) {
  if (state == "stay") {
    return(C*ifelse(e == 0,0,1) + c * e^2)
  } else { # state == "leave"
    return((C*ifelse(e == 0,0,1) + 0 * e^2) * cost_leave)
  }
}

# Function to simulate one period's decision and outcome
simulate_period <- function(current_wage, L, period, N) {
  # Define a sequence of e values from 0 to 1 for optimizing effort
  if (period < N) {
    e_values <- seq(0, 1, by = 0.01)
  } else {
    # No effort in the final period
    e_values <- 0
  }

  max_utility <- -Inf
  optimal_e <- 0
  optimal_decision <- "stay"
  new_wage <- current_wage

  for (e in e_values) {
    stay_utility <- current_wage + delta * ((1 - L) * current_wage * (1 - depreciation) + L * (e * current_wage * (1 - depreciation) + (1 - e) * b)) - c_e(e, "stay")
    leave_utility <- b + delta * (e * initial_wage + (1 - e) * b) - c_e(e, "leave")

    if (stay_utility > max_utility) {
      max_utility <- stay_utility
      optimal_e <- e
      optimal_decision <- "stay"
      new_wage <- current_wage * (1 - depreciation)
    }

    if (leave_utility > max_utility) {
      max_utility <- leave_utility
      optimal_e <- e
      optimal_decision <- "leave"
      new_wage <- initial_wage
    }
  }

  # Adjust for job loss
  job_loss_event <- runif(1) < L
  if (optimal_decision == "stay" && job_loss_event) {
    new_wage <- b
  }

  return(list(decision = optimal_decision, new_wage = new_wage, effort = optimal_e, job_lost = job_loss_event))
}

# Use the adjusted function in the simulation loop
for (period in 1:N) {
  period_result <- simulate_period(current_wage, L[period], period, N)
  decision_path[[period]] <- period_result
  current_wage <- period_result$new_wage # Update wage for the next period
  cat(sprintf("Period %d: Decision = %s, Wage = %.2f, Effort = %.2f, Job Lost = %s\n",
              period, period_result$decision, period_result$new_wage, period_result$effort,
              ifelse(period_result$job_lost, "Yes", "No")))
}

# Simulate the decision path over N periods
# current_wage <- initial_wage
# decision_path <- list()
# for (period in 1:N) {
#   period_result <- simulate_period(current_wage, L[period])
#   decision_path[[period]] <- period_result
#   current_wage <- period_result$new_wage # Update wage for the next period
#   cat(sprintf("Period %d: Decision = %s, Wage = %.2f, Effort = %.2f, Job Lost = %s\n",
#               period, period_result$decision, period_result$new_wage, period_result$effort,
#               ifelse(period_result$job_lost, "Yes", "No")))
# }

library(ggplot2)
library(dplyr)

# Setting up decision paths to plot
decision_df <- do.call(rbind, lapply(1:N, function(i) {
  data.frame(
    Period = i,
    Decision = decision_path[[i]]$decision,
    Wage = decision_path[[i]]$new_wage,
    Effort = decision_path[[i]]$effort,
    JobLost = decision_path[[i]]$job_lost
  )
}))

decision_df$JobLost <- as.factor(decision_df$JobLost)

ggplot(decision_df, aes(x = Period)) +
  geom_line(aes(y = Wage, colour = "Wage")) +
  geom_line(aes(y = Effort, colour = "Effort")) +
  scale_colour_manual("",
                      breaks = c("Wage", "Effort"),
                      values = c("Wage" = "blue", "Effort" = "red")) +
  labs(y = "Value", title = "Wage and Effort Over Time") +
  theme_minimal()

ggplot(decision_df, aes(x = Period, y = Wage)) +
  geom_line() +
  geom_point(aes(color = JobLost), size = 3) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  labs(y = "Wage", title = "Wage Over Time with Job Loss Events") +
  theme_minimal()



