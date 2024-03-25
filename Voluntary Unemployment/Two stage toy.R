# Define parameters
L <- 0.5
w1 <- 1
w2 <- 1
b <- 0.7
delta <- 1

# Define the cost function to allow discontinuous choice for the fixed cost of work
c_e <- function(e) {
  if (e > 0) {
    return(0.1 + 0.25 * e^2)
  } else {
    return(0)
  }
}

# Define a sequence of e values from 0 to 1
e_values <- seq(0, 1, by = 0.01)

# Calculate V_stay(e) and V_leave(e) for each e
V_stay <- numeric(length(e_values))
V_leave <- numeric(length(e_values))

for (i in 1:length(e_values)) {
  e <- e_values[i]
  V_stay[i] <- w1 + (1 - L) * w2 + L * e * w2 - c_e(e)
  V_leave[i] <- b + e * w2 - c_e(e)*0.0
}

# Plot V_stay(e) and V_leave(e)
plot(e_values, V_stay, type = "l", col = "blue", ylim = range(c(V_stay, V_leave)), ylab = "Value", xlab = "Search Effort (e)", main = "Value of Staying vs. Leaving")
lines(e_values, V_leave, col = "red")
legend("bottomright", legend = c("Stay", "Leave"), col = c("blue", "red"), lty = 1)

# Optimisation is based on comparing the maximum value across both - but want to "print" based on whether the staying case maximises outcomes at 0 or at positive effort
