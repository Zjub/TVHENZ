# Define parameters
L <- 0.5 # Probability of losing a job between periods 1 and 2
w1 <- 1 # Wage of first period job
w2 <- 1 # Wage of second period job
b <- 0.7 # Unemployment benefit - is a fixed sum not a proportion of earnings
delta <- 1 # Discount rate
C <- 0.1 # Fixed cost of search
c <- 0.25 # Marginal cost of search
cost_leave <- 1 # Set to 0 if the fixed cost of search is also removed when leaving.

Work_decision <- function(L = 0.5,w1=1,w2=1,b=0.7,delta=1,C=0.1,c=0.25,cost_leave=1){
  # Define the cost function to allow discontinuous choice for the fixed cost of work
  c_e <- function(e,state) {
    if (e > 0 & state == "stay") {
      return(C + c * e^2)
    } else if (e > 0 & state == "leave") {
      return(C + 0 * e^2)
    }
    else{
      return(0)
    }
  }
  
  # Define a sequence of e values from 0 to 1 - implicit is that the probability of reemployment is equal to these values (so a full effort of 1 implies a 100% probability of employment in period 2)
  e_values <- seq(0, 1, by = 0.01)
  
  # Calculate V_stay(e) and V_leave(e) for each e
  V_stay <- numeric(length(e_values))
  V_leave <- numeric(length(e_values))
  
  for (i in 1:length(e_values)) {
    e <- e_values[i]
    V_stay[i] <- w1 + (1 - L) * w2 + L * (e * w2 + (1-e) * b) - c_e(e,"stay")
    V_leave[i] <- b + e * w2 + (1-e) * b - c_e(e,"leave")*cost_leave
  }
  
  # Plot V_stay(e) and V_leave(e)
  plot(e_values, V_stay, type = "l", col = "blue", ylim = range(c(V_stay, V_leave)), ylab = "Value", xlab = "Search Effort (e)", main = "Value of Staying vs. Leaving")
  lines(e_values, V_leave, col = "red")
  legend("bottomright", legend = c("Stay", "Leave"), col = c("blue", "red"), lty = 1)
  
  # Optimisation is based on comparing the maximum value across both - but want to "print" based on whether the staying case maximises outcomes at 0 or at positive effort
  
  value_stay_nosearch <- V_stay[1]
  value_stay_search <- max(V_stay[2:length(V_stay)])
  stay_effort_value <- e_values[which.max(V_stay)]
  value_leave <- max(V_leave)
  leave_effort_value <- e_values[which.max(V_leave)]
  
  choice_value <- max(value_stay_nosearch, value_stay_search, value_leave)
  
  if (choice_value == value_stay_nosearch) {
    print(paste("The maximum utility value is", choice_value, "and it is associated with staying without search."))
  } else if (choice_value == value_stay_search) {
    print(paste("The maximum utility value is", choice_value, "and it is associated with staying with search and", stay_effort_value,"search effort."))
  } else if (choice_value == value_leave) {
    print(paste("The maximum utility value is", choice_value, "and it is associated with leaving and", leave_effort_value,"search effort."))
  }
}

Work_decision()

