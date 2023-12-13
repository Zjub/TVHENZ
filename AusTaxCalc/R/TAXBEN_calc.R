## Pulls in functions and policies to generate income and tax measures ----
# Last update: 26/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan



#TAXBEN_calc <- function(){ # Needs to be a function of all the options we have in the shiny
remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")  

if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  data.table,
  theme61)

library(theme61) # Having some specific issues pulling this package in.

# Include functions ----
setwd("C:/Users/MattNolan/Github/TVHENZ/AusTaxCalc/R") # Make this global environment
scripts <- list.files(pattern = "calc_")
for (script in scripts){
  source(script)
}

# Import parameters for the given quarter ----

policy_date1 <- "Q1_2023"

# policy_parameters(policy_date1) # This is not importing and assigning. We can assigned the object it returns manually and create the parameters in this file.

policy_parameters_1 <- policy_parameters(policy_date1)

# Assign policy object
for (i in 1:nrow(policy_parameters_1)){
  assign(policy_parameters_1$Parameter[i],policy_parameters_1$Value[i])
}

# Income tax

tax_brackets <- c(0, tax_free, tax_threshold_1, tax_threshold_2, tax_threshold_3)
tax_scale <- c(0, tax_rate_1, tax_rate_2, tax_rate_3, tax_rate_4)

# Benefit elements

JSP_S_ND_arate <- c(0,JSP_S_ND_arate_1,JSP_S_ND_arate_2) 
JSP_S_ND_athresh <- c(0,JSP_S_ND_athresh_1,JSP_S_ND_athresh_2)

#Add defaults for initial testing ----
  partnered <- 0
  partner_earnings <- 0
  over_60 <- 0
  living_alone <- 0
  Have_dep <- 1
  Main_carer_dep <- 1
  child_age <- 12
  
  Numb_dep <- 1
  young_child <- 12
  
  # Individual characteristics
  Carer <- 0
  Disability <- 0
  
  # Housing status
  Home_owner <- 0
  Rent <- 500 # Fortnightly rent amount
  
  # Labour market conditions
  wage <- 25
  max_hours <- 60
  
  # Meets conditions to access benefit on a personal level
  ben_eligibility <- 1
  
  
    

## Setup the individuals characteristics based on user inputs ----
# Define family characteristics
  partnered <- as.numeric(partnered)
  partner_earnings <- partner_earnings
  over_60 <- as.numeric(over_60)
  living_alone <- as.numeric(living_alone)
  Have_dep <- as.numeric(Have_dep)
  child_age <- child_ages
  if (Main_carer_dep == 0){
    child_age <- 99
  }
  
  Numb_dep <- length(child_age[child_age < 20])
  young_child <- min(child_age)
  
  PPeligible <- ifelse((young_child <= 6 | (young_child <= 8 & partnered == 0)) & Main_carer_dep == 1,1,0)
  
  # Individual characteristics
  Carer <- 0
  Disability <- 0
  
  # Housing status
  Home_owner <- 0
  Rent <- Rent # Fortnightly rent amount
  
  # Labour market conditions
  wage <- wage
  max_hours <- max_hours
  
  # Meets conditions to access benefit on a personal level
  ben_eligibility <- 1
  
  
  ## Set up income plot and text ----
  
  net_income_calculator <- function(wage_rate) {
    net_income_profile <- numeric((max_hours +1))
    for (hours in 0:max_hours) {
      net_income_profile[hours + 1] <- calc_net_income(wage_rate, hours)[["net_income"]]
    }
    return(net_income_profile)
  }
  
  net_incomes_data <- net_income_calculator(wage)
  
  incomesdf <- data.frame(Hours = seq(0,max_hours,by=1), "Take home income" = net_incomes_data) %>% mutate("Work Income" = wage*Hours*52) %>% pivot_longer(!Hours,names_to = "variable",values_to = "value")
  
  plot_income <- ggplot(incomesdf,aes(x=Hours,y=value/1000,colour=variable)) + geom_line() + theme_e61(legend="bottom") + labs_e61(
    title = "Net Income",
    subtitle = "Annual income",
    y = "(000s)",
    x = "Hours worked",
    sources = c("e61", cite)
  ) + scale_y_continuous(labels = scales::dollar,limits=c(0,max(incomesdf$value/1000)*1.2)) + scale_colour_e61(n=2) + add_e61_logo() # Looks like the version of theme61 being pulled in is very old and doesn't work

  plot_gross_inc_components # Make this benefits + family benefits + wage income initially.

## Calculate tax concepts ----








#}