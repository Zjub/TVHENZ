# Based on January 2023 Service Australia: https://www.servicesaustralia.gov.au/sites/default/files/2023-03/co029-2301.pdf
## Calculator is soley for JSP access and the relative structure, as at January 2023.
## Parenting payment not yet included
### Add carer and other payments later
## Before extending note this was a very quick construction and is quite unreadable - would suggest rewriting to make annual, weekly, and fortnightly sums clear, and limit hardcoding.

rm(return)
.libPaths(new = 'C:/Rpackage')

calculate_EMTR <- function(wage = 25, hour_interest = 20, partnered = 0, partner_earnings = 0, over_60 = 0, living_alone = 0, Have_dep = 1, Main_carer_dep = 0, child_age = c("4,11"),Rent=200) {
  
  .libPaths(new = 'C:/Rpackage') # Don't do this for main version - laptop issues
  library(tidyverse)
  library(theme61)
  
  # Define family characteristics
  partnered <- as.numeric(partnered)
  partner_earnings <- partner_earnings
  over_60 <- as.numeric(over_60)
  living_alone <- as.numeric(living_alone)
  Have_dep <- as.numeric(Have_dep)
  Main_carer_dep <- as.numeric(Main_carer_dep)
  numeric_values <- strsplit(child_age, split = ",")[[1]]
  child_age <- ifelse(Main_carer_dep == 0,"99",as.numeric(numeric_values))
  Numb_dep <- length(child_age[child_age < 20])
  young_child <- min(child_age)
  
  
  # Individual characteristics
  Carer <- 0
  Disability <- 0
  
  # Housing status
  Home_owner <- 0
  Rent <- Rent # Fortnightly rent amount
  
  # Labour market conditions
  wage <- wage
  hour_interest = hour_interest
  
  ##### Income tax block
  # Define tax brackets and rates - note, might be better to put as a single thing as they will be "optional" hidden adjustments, so the people doing it will know what they are doing.
  tax_free <- 18200
  threshold1 <- 45000
  threshold2 <- 120000
  threshold3 <- 180000
  rate1 <- 0.19
  rate2 <- 0.325
  rate3 <- 0.37
  rate4 <- 0.45
  
  tax_brackets <- c(0, tax_free, threshold1, threshold2, threshold3)
  tax_scale <- c(0, rate1, rate2, rate3, rate4)
  
  # Function to calculate income tax
  calc_income_tax <- function(gross_income) {
    tax <- 0
    tempinc <- gross_income
    for (i in length(tax_brackets):2) {
      if (gross_income > tax_brackets[i]) {
        tax <- tax + (tempinc - tax_brackets[i]) * tax_scale[i]
        tempinc = tax_brackets[i]
      }
    }
    return(tax)
  }
  
  #### Non-family benefits block
  # Function to calculate benefit payment (adding first tier and subs) - note the subs all appear to add to the base payment, which is then abated, so have coded that way.
  # Have hardcoded thresholds - can replace with variables later if we want to add to options.
  
  calc_rent_assistance <- function(Rent,partnered,Numb_dep,living_alone){
    rent_assistance = 0
    if (Numb_dep == 0){
      rent_assistance = ifelse(living_alone == 1,max((min(337.84,Rent)-135.4)*0.75,0),ifelse(partnered == 0,max((min(271.6,Rent)-135.4)*0.75,0),max((min(409.6,Rent)-219.2)*0.75,0)))
    }
    else if (Numb_dep < 3) {
      rent_assistance = ifelse(partnered == 0,max((min(415.62,Rent)-177.8)*0.75,0),max((min(500.6,Rent)-262.78)*0.75,0))
    }
    else {
      rent_assistance = ifelse(partnered == 0,max((min(446.23,Rent)-177.8)*0.75,0),max((min(531.21,Rent)-262.78)*0.75,0))
    }
  }
  
  
  calc_energy_supp <- function(partnered,Have_dep,over_60){
    # This needs to be updated to include the family benefit amounts
    energy_supp = 0
    if (partnered == 1){
      energy_supp = 7.9
    }
    else if (Have_dep == 0 & over_60 == 0){
      energy_supp = 8.8
    }
    else {
      energy_supp = 9.5
    }
    return(energy_supp)
  }
  
  # Define benefit rates and abatement rates on a fortnightly basis
  ## Still need to add JSP for other family types
  JSP_S_ND_pay <- 668.40
  JSP_S_D_pay <- 720.40
  JSP_C_ND_pay <- 562.80
  JSP_C_D_pay <- 612.60
  JSP_S_ND_arate <- c(0,0.5,0.6) 
  JSP_S_ND_athresh <- c(0,150,256)
  PP_S_pay <- 889.2
  PP_C_pay <- 608.7
  PP_S_athresh <- 202.6 + 24.6*(Numb_dep - 1)
  PP_S_arate <- 0.4
  
  PPeligible <- ifelse((young_child <= 6 | (young_child <= 8 & partnered == 0)) & Main_carer_dep == 1,1,0)
  
  # Calculation of the gross first tier benefit payment - at present only JSP and parenting payment
  calc_main_benefit <- function(Numb_dep,partnered,Main_carer_dep) {
  JSP_pay <- 0
  PP_pay <- 0
  if (PPeligible == 1){
    PP_pay <- ifelse(partnered == 1,PP_C_pay,PP_S_pay)
    }
  else {
    JSP_pay <- ifelse(Numb_dep == 0 & partnered == 0,JSP_S_ND_pay,ifelse(Numb_dep > 0 & partnered == 0, JSP_S_D_pay,ifelse(Numb_dep == 0,JSP_C_ND_pay,JSP_C_D_pay)))
    }
  return(list(PP_pay = PP_pay,JSP_pay = JSP_pay))
  }
  
  Main_pay <- calc_main_benefit(Numb_dep,partnered,Main_carer_dep)
  JSP_pay <- Main_pay[["JSP_pay"]]
  PP_pay <- Main_pay[["PP_pay"]]
  
  #JSP_arate # These do not vary at this stage.
  #JSP_athresh # These do not vary at this stage.
  
  # Now calculate the gross benefit amount
  
  calc_benefit_gross <- function(){
    gross_benefit = JSP_pay + PP_pay + calc_rent_assistance(Rent,partnered,Numb_dep,living_alone) + calc_energy_supp(partnered,Have_dep,over_60)
    return(gross_benefit)
  }
  
  # Function to calculate benefit abatement
  
  partner_thresh <- 1196
  partner_thresh_dep <- 1201
  partner_abate <- 0.6
  
  calc_benefit_abated <- function(work_income,gross_benefit) {
    # Abate down based on partner income first.
    abate = 0
    abate <- ifelse(Have_dep == 1, max((partner_earnings/26 - partner_thresh_dep)*0.6,0),max((partner_earnings/26 - partner_thresh)*0.6,0))
    tempwi <- work_income/26 # Fortnightly pay
    for (i in length(JSP_S_ND_athresh):2) {
      if (work_income/26 > JSP_S_ND_athresh[i]){
        abate <- abate + (tempwi - JSP_S_ND_athresh[i])*JSP_S_ND_arate[i]
        tempwi <- JSP_S_ND_athresh[i]
      }
    }
    # Add a check that the PP is smaller than the abated benefit - otherwise switch
    net_benefit <- gross_benefit - abate
    return(max(net_benefit,0))
  }
  
  #### Family support block
  # This is purely a supplement on top of the benefit - there are differential JSP payments AND a family benefit payment when on JSP.
  # One off payments (i.e. end of year supplement) are not included.
  # Family benefits ARE NOT in taxable income.  Therefore we assess this after determining standard gross income (what is called adjusted income)
  
  fam_base_threshold <- 103368
  fam_max_threshold <- 58108
  fam_basic_rate <- 63.56
  
  calc_family_benefit <- function(child_age,gross_fam_income){
    net_fam_a = 0
    numb_child_12 <- length(child_age[child_age < 13])
    numb_child_19 <- length(child_age[child_age <20 & child_age > 12])
    fam_a <- 197.96*numb_child_12 + 257.46*numb_child_19
    if (gross_fam_income < fam_base_threshold & Numb_dep > 0){
      net_fam_a <- max((fam_a*26 - max((gross_fam_income-fam_max_threshold),0)*0.2 - max((gross_fam_income - fam_base_threshold),0)*0.1)/26,fam_basic_rate*(numb_child_19+numb_child_12))
    }
    else if (Numb_dep > 0) {
      net_fam_a <- max(max((fam_a*26 - max((gross_fam_income-fam_max_threshold),0)*0.2 - max((gross_fam_income - fam_base_threshold),0)*0.1)/26,(fam_basic_rate*(numb_child_19+numb_child_12)*26 - 0.3*(gross_fam_income - fam_base_threshold))/26),0)
    }
    else {
      net_fam_a = 0
    }
    return(net_fam_a)
  }
  
  ####### EMTR block
  # Function to calculate effective marginal tax rate
  calc_effective_marginal_tax_rate <- function(wage_rate, hours) {
    work_income <- wage_rate * hours * 52
    gross_benefit <- calc_benefit_gross() # Run into an issue here with the PP 
    abated_benefit <- calc_benefit_abated(work_income,gross_benefit)
    clawback <- gross_benefit - abated_benefit
    gross_income = work_income + (abated_benefit)*26
    income_tax <- calc_income_tax(gross_income)
    gross_fam_income = gross_income + partner_earnings
    fam_a_income = calc_family_benefit(child_age,gross_fam_income)*26
    net_income <- gross_income - income_tax + fam_a_income
    
    if (hours > 0) {
      additional_income <- wage_rate*52
      
      new_benefit_payment <- calc_benefit_abated(work_income + additional_income,gross_benefit)*26 
      
      new_income_tax <- calc_income_tax(work_income + additional_income + new_benefit_payment)
      
      new_family_payment <- calc_family_benefit(child_age,work_income + additional_income + new_benefit_payment + partner_earnings)*26 
      
      new_net_income <- (work_income + additional_income) - new_income_tax + new_benefit_payment + new_family_payment
      
      marginal_tax_rate <- (new_net_income - net_income) / additional_income
      return(1 - marginal_tax_rate)
    }
    
    return(0)
  }
  
  calc_net_income <- function(wage_rate, hours) {
    work_income <- wage_rate * hours * 52
    gross_benefit <- calc_benefit_gross() # Run into an issue here with the PP 
    abated_benefit <- calc_benefit_abated(work_income,gross_benefit)
    clawback <- gross_benefit - abated_benefit
    gross_income = work_income + (abated_benefit)*26
    income_tax <- calc_income_tax(gross_income) # This tax calculation is stopping when someone has a
    gross_fam_income = gross_income + partner_earnings
    fam_a_income = calc_family_benefit(child_age,gross_fam_income)*26
    net_income <- gross_income - income_tax + fam_a_income
    
    return(net_income)
  }
  
  # Main function to calculate tax rates for each hour of work from 0 to 60
  marginal_tax_rate_calculator <- function(wage_rate) {
    tax_rates <- numeric(61)
    for (hours in 0:60) {
      tax_rates[hours + 1] <- calc_effective_marginal_tax_rate(wage_rate, hours)
    }
    return(tax_rates)
  }
  
  average_tax_rate_calculator <- function(wage_rate) {
    EATR <- numeric(61)
    initial_gross_benefit <- calc_benefit_gross()
    initial_gross_income <- calc_benefit_abated(0,initial_gross_benefit)*26
    initial_gross_fam_income = initial_gross_income + partner_earnings
    initial_income <- initial_gross_income - calc_income_tax(initial_gross_income) + calc_family_benefit(child_age,initial_gross_fam_income)*26
    for (hours in 1:60) {
      EATR[hours + 1] <- 1 - (calc_net_income(wage_rate,hours) - initial_income)/(wage_rate*hours*52)
    }
    return(EATR)
  }
  
  net_income_calculator <- function(wage_rate) {
    net_income_profile <- numeric(61)
    #net_income_profile <- data.frame(numeric(61),numeric(61),numeric(61),numeric(61))
    for (hours in 0:60) {
      net_income_profile[hours + 1] <- calc_net_income(wage_rate, hours)
    }
    return(net_income_profile)
  }
  
  # Calculation:
  tax_rates <- marginal_tax_rate_calculator(wage)
  taxdf <- data.frame(Hours = seq(0,60,by=1), EMTR = tax_rates)
  
  plot <- ggplot(taxdf,aes(x=Hours,y=EMTR)) + geom_line(colour="#008080") + theme_e61() + labs_e61(
    title = "Tax paid when working an extra hour",
    subtitle = "Effective Marginal Tax Rate",
    y = "     ",
    x = "Hours worked"
  ) + scale_y_continuous(labels=scales::percent,limits=c(0,1.1),breaks=seq(0,1,by=0.2)) + geom_hline(yintercept = 1,size=1.5,colour="red") + theme_e61()
  
  paragraph1 <- paste("The above figure shows what is called the effective marginal tax rate. This refers to the proportion of any income that is given to government - either through tax payments, or a reduction in government benefits received - when you decide to work a little bit more.  For example, if you were regularly working ",hour_interest," hours per week and wanted to increase this to ", hour_interest + 1, " hours this person would earn an additional $",format(round(wage*52,2), big.mark = ",", decimal.mark = ".", nsmall = 2)," per year.  However, they would only take home an extra $", format(round(wage*52*(1-taxdf[taxdf$Hours == hour_interest,]$EMTR),2), big.mark = ",", decimal.mark = ".", nsmall = 2),".",sep="")
  
  net_incomes_data <- net_income_calculator(wage)

  incomesdf <- data.frame(Hours = seq(0,60,by=1), "Take home income" = net_incomes_data) %>% mutate("Work Income" = wage*Hours*52) %>% pivot_longer(!Hours,names_to = "variable",values_to = "value")
  
  plot_income <- ggplot(incomesdf,aes(x=Hours,y=value/1000,colour=variable)) + geom_line() + theme_e61(legend="bottom") + labs_e61(
    title = "Net Income",
    y = "(000s)",
    x = "Hours worked"
  ) + scale_y_continuous(labels = scales::dollar) + scale_colour_e61(n=2)
  
  EATR_data <- average_tax_rate_calculator(wage)
  
  EATRdf <- data.frame(Hours = seq(0,60,by=1),EATR = EATR_data,EMTR = tax_rates) %>% pivot_longer(!Hours,names_to = "variable",values_to = "values") 
  
  print(EATRdf)
  
  plot_EATR <- ggplot(EATRdf,aes(x=Hours,y=values,colour=variable)) + geom_line() + labs_e61(
    title = "Proportion of earnings paid to government",
    subtitle = "Effective Average Tax Rate",
    y = "     ",
    x = "Hours worked"
  ) + scale_y_continuous(labels=scales::percent,limits=c(0,1.1),breaks=seq(0,1,by=0.2)) + geom_hline(yintercept = 1,size=1.5,colour="red") + theme_e61()
  
  
  # The EATR write-up
  paragraph2 <- paste("The above figure shows what is called the effective average tax rate. This refers to the proportion of any income that is given to government - either through tax payments, or a reduction in government benefits received - when you decide to enter the workforce in a job at that hours level.  For example, if you started a job regularly working ",hour_interest," hours per week this person would have earned $",format(round(wage*52*hour_interest,2), big.mark = ",", decimal.mark = ".", nsmall = 2)," per year at work.  However, they would only take home an extra $", format(round(wage*hour_interest*52*(1-EATRdf[EATRdf$Hours == hour_interest & EATRdf$variable == "EATR",]$values),2), big.mark = ",", decimal.mark = ".", nsmall = 2),".",sep="", " compared to not taking on the job.")
  
  paragraph3 <- paste("The above shoes the income the person earns and the income you take home for different hours of work. At ", hour_interest," hours of work this individuals earns $",format(round(wage*52*hour_interest,2), big.mark = ",", decimal.mark = ".", nsmall = 2)," per year at work.  Their take-home pay, after subtracting tax and adding benefits receive would be $", format(round(calc_net_income(wage,hour_interest),2), big.mark = ",", decimal.mark = ".", nsmall = 2),".",sep="")

  checks = c(Numb_dep,length(child_age))  
  #return(plot)
  return(list(plot = plot,plot_EATR = plot_EATR,plot_income = plot_income, paragraph1 = paragraph1,paragraph2=paragraph2,paragraph3=paragraph3,checks=checks))
}

return <- calculate_EMTR(wage = 25, partnered = 0, partner_earnings = 0, over_60 = 0, living_alone = 0, Have_dep = 1, Main_carer_dep = 1, child_age = c("4,9,11"), Rent=200)

return$plot

return$plot_EATR

return$plot_income

return$paragraph1

return$paragraph2

return$paragraph3

return$checks