# Date created: 9/04/2024
# Initial author: Matt Nolan
# Date last updated: 9/04/2024
# Last edit: Matt Nolan


# Parameter description
L # Probability of losing a job between periods 1 and 2
w1 # Wage of first period job
w2 # Wage of second period job - this is the same wage whether the person has switched jobs or not! (i.e. no direct wage gain from J2J)
b # Unemployment benefit - is a fixed sum not a proportion of earnings
delta # Discount rate
C # Fixed cost of search
c # Marginal cost of search
cost_leave # Set to 0 if the fixed cost of search is also removed when leaving.

Work_decision <- function(L = 0.5,w1=0.64,w2=1,b=0.4,delta=1,C=0.1,c=0.25,cost_leave=1){
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
    V_stay[i] <- w1 + delta*((1 - L) * w2 + L * (e * w2 + (1-e) * b)) - c_e(e,"stay")
    V_leave[i] <- b + delta*(e * w2 + (1-e) * b) - c_e(e,"leave")*cost_leave
  }
  
  ## Plot V_stay(e) and V_leave(e)
  # plot(e_values, V_stay, type = "l", col = "blue", ylim = range(c(V_stay, V_leave)), ylab = "Value", xlab = "Search Effort (e)", main = "Value of Staying vs. Leaving")
  # lines(e_values, V_leave, col = "red")
  # legend("bottomright", legend = c("Stay", "Leave"), col = c("blue", "red"), lty = 1)
  
  # Optimisation is based on comparing the maximum value across both - but want to "print" based on whether the staying case maximises outcomes at 0 or at positive effort
  
  value_stay_nosearch <- V_stay[1]
  value_stay_search <- max(V_stay[2:length(V_stay)])
  stay_effort_value <- e_values[which.max(V_stay)]
  value_leave <- max(V_leave)
  leave_effort_value <- e_values[which.max(V_leave)]
  
  choice_value <- max(value_stay_nosearch, value_stay_search, value_leave)
  
  choice_desc <- if (choice_value == value_stay_nosearch) {
    print(paste("The maximum utility value is", choice_value, "and it is associated with staying without search."))
  } else if (choice_value == value_stay_search) {
    print(paste("The maximum utility value is", choice_value, "and it is associated with staying with search and", stay_effort_value,"search effort."))
  } else if (choice_value == value_leave) {
    print(paste("The maximum utility value is", choice_value, "and it is associated with leaving and", leave_effort_value,"search effort."))
  }
  
  choice <- if (choice_value == value_stay_nosearch) {
    return(list("Staying without search",0))
  } else if (choice_value == value_stay_search) {
    return(list("Staying with search",stay_effort_value))
  } else if (choice_value == value_leave) {
    return(list("Leaving",leave_effort_value))
  }
  
  return(choice)
}

a <- Work_decision(L = 0.5,c=0.5,w1=0.5)

## Job loss probability plot
L_values <- seq(0, 1, by = 0.01)

effort_by_L <- numeric(length = length(L_values))
effort_cat_L <- character(length = length(L_values))

for (i in 1:length(L_values)){
  temp <- Work_decision(L = L_values[i])
  effort_by_L[i] <- temp[[2]]
  effort_cat_L[i] <- temp[[1]]
}

plot_effort <- data.table(L_values,effort_by_L,effort_cat_L)

ggplot(plot_effort,aes(x=L_values,y=effort_by_L,colour=effort_cat_L))+geom_point() + scale_y_continuous_e61(limits=c(-0.25,1.25,0.25)) + labs_e61(title = "Search effort by probability of job loss",y="Effort",x="Probability of job loss") + theme_e61(legend = "right") # Very knife edge example, but shows the case.

## Marginal costs plot
c_values <- seq(0, 1, by = 0.01)

effort_by_c <- numeric(length = length(c_values))
effort_cat_c <- character(length = length(c_values))

for (i in 1:length(c_values)){
  temp <- Work_decision(c = c_values[i])
  effort_by_c[i] <- temp[[2]]
  effort_cat_c[i] <- temp[[1]]
}

plot_mc <- data.table(c_values,effort_by_c,effort_cat_c)

ggplot(plot_mc,aes(x=c_values,y=effort_by_c,colour=effort_cat_c))+geom_point() + scale_y_continuous_e61(limits=c(-0.25,1.25,0.25)) + labs_e61(title = "Search effort by marginal cost of search",y="Effort",x="Marginal Cost") + theme_e61(legend = "right") 
