# Application of a "smooth consumption rule". Involves a rewrite of the prior tool that starts with tax and benefit calculators that are integrated.  Provides a labour income path.  And then has an individual save an borrow to get smooth consumption (even with the superannuation contribution).  This will also allow more variety about when people earn.

calculate_smooth_spend <- function(initial_labour_earnings = 40000, initial_wealth = 0, wage_growth = 0.03, youth_wageg=0.02, mid_wageg=0, old_wageg=-0.02, retirement_pension = 1064*26, JSP = 701.9*26, interest_rate = 0.02, comp_super_rate = 0.105, labour_start_year = 20, retirement_year = 67, death_year = 90, cite="") {
  library(theme61)
  library(tidyverse)
  
  # Calculate the number of years from the start of labour earnings to retirement
  num_years_labour <- retirement_year - labour_start_year
  num_years_alive <- death_year - labour_start_year
  
  # Initialize variables
  total_super <- 0.0
  total_tax <- 0.0
  total_gincome_labour <- 0.0
  total_income <- 0.0
  tax_free <- 18200
  threshold1 <- 45000
  threshold2 <- 120000
  threshold3 <- 180000
  rate1 <- 0.19
  rate2 <- 0.325
  rate3 <- 0.37
  rate4 <- 0.45
  supertax <- 0.15
  bthresh1 <- 150*26
  bthresh2 <- 256*26
  bassthresh <- 504500
  bassabate <- 0.003*26
  abate1 <- 0.5
  abate2 <- 0.6
  assthresh <- 504500
  assabate <- 0.003*26 # Declines by $3 per fortnight for every $1000 over threshold.
  incretthresh <- 190*26 # Pension income threshold
  incretabate <- 0.5 # Pension income abatement
  young_age <- 30
  mid_age <- 45
  old_age <- retirement_year
  
  # tax calculator
  taxcalc <- function(inc) {
    if (inc <= tax_free) {
      return(0)
    } else if (inc <= threshold1) {
      return((inc - tax_free) * rate1)
    } else if (inc <= threshold2) {
      return((inc - threshold1) * rate2 + (threshold1 - tax_free) * rate1)
    } else if (inc <= threshold3) {
      return((inc - threshold2) * rate3 + (threshold2 - threshold1) * rate2 + (threshold1 - tax_free) * rate1)
    } else {
      return((inc - threshold3) * rate4 + (threshold3 - threshold2) * rate3 + (threshold2 - threshold1) * rate2 + (threshold1 - tax_free) * rate1)
    }
  }
  
  # calculate benefits
  bencalc <- function(inc,total_assets,other_wealth,age) {
    if (age < 67) {
      threshold <- bassthresh
      reduction_rate <- bassabate
      if (inc <= bthresh1 & other_wealth <= bassthresh) {
        return(JSP)
      } else if (inc <= bthresh2) {
        return(min(JSP - (inc - bthresh1)*abate1),max(JSP - reduction_rate*(other_wealth - threshold),0))
      } else {
        return(min(max(JSP - (inc - bthresh2)*abate2 - (bthresh2 - bthresh1)*abate1,0),max(JSP - reduction_rate*(other_wealth - threshold),0)))
      }
    }
    else {
      threshold <- assthresh
      incthreshold <- incretthresh
      reduction_rate <- assabate
      red_inc_rate <- incretabate
      max_pension <- retirement_pension
      pension_gross_inc <- total_assets*interest_rate
      if (age < 67) { 
        pension <- 0
      } 
      else if (total_assets >= threshold | pension_gross_inc > incthreshold) {
          pension <- min(max(max_pension - reduction_rate * (total_assets - threshold), 0),max(max_pension - red_inc_rate*(pension_gross_inc - incthreshold),0) )
      } 
      else {
          pension <- max_pension
      }
      return(pension)
    }
  }
  
  # Calculate labour earnings. Note: No labour supply behaviour, so this is exogenous - the user determines the retirement age and works from there.
  # Note, when making superannuation a binding constraint, will need to add an additional loop to stop individuals borrowing against, or taking from, super prior to 67.
  # Have issue of other_wealth possibly going zero prior to 67.  Will need to recode to allow for.
  
  labour_earnings <- initial_labour_earnings
  other_wealth <- initial_wealth
  total_super <- 0
  total_incomes <- numeric(length = (num_years_alive))
  work_gincomes <- numeric(length = (num_years_alive))
  super_balance <- numeric(length = (num_years_alive))
  other_wealth_balance <- numeric(length = (num_years_alive))
  
  get_assets_income <- function(total_super,other_wealth, interest_rate, num_years_alive, consumption) {
    # Define initial values
    
    for (year in 1:(num_years_alive)) {
      age <- labour_start_year + year - 1
      if (year < (num_years_alive-labour_start_year)) {
        # Calculate the income and tax for the current year
        wage_g <- ifelse(age == labour_start_year,0,ifelse(age <= young_age,wage_growth+youth_wageg,ifelse(age <= mid_age,wage_growth + mid_wageg,wage_growth + old_wageg)))
        labour_earnings <- labour_earnings*(1+wage_g)
        capital_income <- other_wealth*interest_rate
        benefit <- bencalc(labour_earnings+capital_income,other_wealth+total_super,other_wealth,age) # "earnings" from super are not counted as income, but the asset base is included in the eligibility rules for the pension.
        income <- labour_earnings + capital_income + benefit
        tax <- taxcalc(income)
        
        # Update the total super, other_wealths, tax, and income from non-super sources
        total_tax <- total_tax + tax
        
        total_gincome_labour <- total_gincome_labour + labour_earnings
        total_income <- total_income + income - tax
        total_super <- total_super + (labour_earnings * comp_super_rate + total_super * interest_rate) * (1 - supertax) # Involves tax on the contribution, but no interest on the contribution that year.
        other_wealth <- other_wealth + income - tax - consumption
        
        work_gincomes[year] <- labour_earnings
        total_incomes[year] <- income - tax
        super_balance[year] <- total_super
        other_wealth_balance[year] <- other_wealth + income - tax - consumption
      }
      else if (age < 67) {
        labour_earnings <- 0
        capital_income <- other_wealth*interest_rate
        benefit <- bencalc(labour_earnings+capital_income,other_wealth+total_super,other_wealth,age) # "earnings" from super are not counted as income, but the asset base is included in the eligibility rules for the pension.
        income <- labour_earnings + capital_income + benefit
        tax <- taxcalc(income)
        
        # Update the total super, other_wealths, tax, and income from non-super sources
        total_tax <- total_tax + tax
        
        total_gincome_labour <- total_gincome_labour + labour_earnings
        total_income <- total_income + income - tax
        total_super <- total_super + (labour_earnings * comp_super_rate + total_super * interest_rate) * (1 - supertax) # Involves tax on the contribution, but no interest on the contribution that year.
        other_wealth <- other_wealth + income - tax - consumption
        
        work_gincomes[year] <- labour_earnings
        total_incomes[year] <- income - tax
        super_balance[year] <- total_super
        other_wealth_balance[year] <- other_wealth + income - tax - consumption
      }
      else {
        labour_earnings <- 0
        capital_income <- other_wealth*interest_rate
        benefit <- bencalc(labour_earnings+capital_income,other_wealth+total_super,other_wealth,age) # "earnings" from super are not counted as income, but the asset base is included in the eligibility rules for the pension.
        income <- labour_earnings + capital_income + benefit
        tax <- taxcalc(income)
        
        # Update the total super, other_wealths, tax, and income from non-super sources
        total_tax <- total_tax + tax
        total_gincome_labour <- total_gincome_labour + labour_earnings
        total_income <- total_income + income - tax
        
        if (other_wealth <= 0){
          total_super <- total_super - consumption + (labour_earnings * comp_super_rate + total_super * interest_rate) * (1 - supertax) + other_wealth
          other_wealth <- 0
        } 
        else if (consumption > other_wealth) {
          total_super <- total_super - (consumption - other_wealth) + (labour_earnings * comp_super_rate + total_super * interest_rate) * (1 - supertax)
          other_wealth <- 0
        } 
        else {
        total_super <- total_super - consumption + (labour_earnings * comp_super_rate + total_super * interest_rate) * (1 - supertax) # Involves tax on the contribution, but no interest on the contribution that year.
        other_wealth <- other_wealth + income - tax - consumption
        }
        work_gincomes[year] <- labour_earnings
        total_incomes[year] <- income - tax
        super_balance[year] <- total_super
        other_wealth_balance[year] <- other_wealth
      }
    }
    total_assets = total_super + other_wealth
    return(total_assets)
  }
  
  # Function to estimate the "smoothed" lifetime consumption amount.
  get_consumption <- function(total_super,other_wealth, interest_rate, num_years_alive, epsilon=1) {
    # Define function to find final asset value for a given consumption amount
    f <- function(c) {
      return(get_assets_income(total_super,other_wealth, interest_rate, num_years_alive, c))
    }
    
    # Use binary search to find consumption amount that results in zero final assets
    lower <- 5000
    upper <- initial_labour_earnings*(wage_growth+youth_wageg)^(num_years_labour) # Upper bound is the peak labour income if the young growth rate was maintained.
    while (upper - lower > 0.01) {
      mid <- (lower + upper) / 2
      final_assets <- get_assets_income(total_super,other_wealth, interest_rate, num_years_alive,mid)
      if (final_assets < 0) {
        upper <- mid
      } else {
        lower <- mid
      }
    }
    return((lower + upper) / 2)
  }
  
  # Calculate retirement consumption amount
  consumption <- get_consumption(total_super = 0,other_wealth = initial_wealth, interest_rate, num_years_alive)
  
  return(list(consumption,total_incomes,work_gincomes,total_tax,super_balance,other_wealth_balance))
}


calculate_smooth_spend()