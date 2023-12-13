# This calculator is looking at someone who is a renter, with insufficent assets to trigger the asset test.

library(tidyverse)
library(theme61)
# Load the necessary packages
library(shiny)

ui <- fluidPage(
  # Custom edits
  #tags$head()
  
  # Side panel title
  titlePanel("Financial Disincentives to Work"),
  
  sidebarLayout(
    sidebarPanel(
      # Style details
      style = "background-color: #ADD8E6;",
      
      # Button to update the graph and paragraph
      actionButton("update", "Update"),
      
      # Input widgets
      numericInput("input1", "What is this person's hourly wage ($, excluding compulsory superannuation contributions)", value = 25),
      numericInput("input1b", "What number of hours does the person work/want to work per week?", value = 20),
      numericInput("input1c", "What is the maximum number of hours they may consider working?", value = 60),
      checkboxInput("input2", "Do they have a partner?", value = FALSE),
      conditionalPanel(
        condition = "input.input2",
        numericInput("input3", "How much does their partner earn annually ($, excluding compulsory superannuation contributions)", value = 0)
      ),
      checkboxInput("input6", "Do they have dependent children", value = FALSE),
      conditionalPanel(
        condition = "input.input6",
        checkboxInput("input6b", "Are they the main carer of these children", value = TRUE)
        ),
      conditionalPanel(
        condition = "input.input6",
        textInput("input7", "Enter the ages of the children (seperated by a comma)", value="11,4,9")
        ),
      checkboxInput("input4", "Is this person 60 or older", value = FALSE),
      checkboxInput("input5", "Is this person living alone", value = FALSE),
      numericInput("input8", "How much does this person pay in rent per fortnight", value = 500),
      checkboxInput("input9b","Do you want to put your name on these graphs?",value = FALSE),
      conditionalPanel(
        condition = "input.input9b",        
        textInput("input9","What is your name:", value = "youtube.com/@tvhe")
        ),
      
      # Button to update the graph and paragraph
      actionButton("update2", "Update")
    ),
    
    mainPanel(
      # Style settings

      # Output elements
      tags$div(style = "text-align: center; font-style: italic;", "Alpha Version 0.1. Personal income of a renter aged 25-64, with no significant assets - medicare excluded, (LMITO assumed cancelled)"),
      tags$br(),
      tags$div(style = "text-align: left;","In the graphs below we will investigate the financial disincentives to work for the person you have entered details about on the left. Click update on the left of this screen to construct the graphs."),
      tags$br(),      
      htmlOutput("paragraph4"),
      tags$br(),
      plotOutput("plot"),
      tags$br(),
      htmlOutput("paragraph1"),
      tags$br(),
      plotOutput("plot_EATR"),
      tags$br(),
      htmlOutput("paragraph2"),
      tags$br(),
      plotOutput("plot_income"),
      tags$br(),
      htmlOutput("paragraph3"),
      tags$br(),
      plotOutput("plot_TaxAbate")

    )
  )
)


# Define the server
server <- function(input, output, session) {
  
  observeEvent(c(input$update, input$update2), {
  
calculate_EMTR <- function(wage = 25,hour_interest = 20, max_hours = 60, partnered = 0, partner_earnings = 0, over_60 = 0, living_alone = 0, Have_dep = 0, Main_carer_dep = 0, child_ages = "99",Rent=200,cite="") {
  
  library(tidyverse)
  library(theme61)
  
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
  
  # Individual characteristics
  Carer <- 0
  Disability <- 0
  
  # Housing status
  Home_owner <- 0
  Rent <- Rent # Fortnightly rent amount
  
  # Labour market conditions
  wage <- wage
  max_hours <- max_hours
  
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
  LITO_on <- 1
  LMITO_on <- 0
  
  tax_brackets <- c(0, tax_free, threshold1, threshold2, threshold3)
  tax_scale <- c(0, rate1, rate2, rate3, rate4)
  
  # Function to calculate income tax
  # LITO and LITMO here
  calc_income_tax <- function(gross_income,work_income) {
    tax_nc <- 0
    tempinc <- gross_income
    for (i in length(tax_brackets):2) {
      if (gross_income > tax_brackets[i]) {
        tax_nc <- tax_nc + (tempinc - tax_brackets[i]) * tax_scale[i]
        tempinc <- tax_brackets[i]
      }
    }
    LITO_credit <- 0
    if (LITO_on == 1){
      LITO_credit <- max(0,700 - max(0,(gross_income - 37500)*0.005) - max(0,(gross_income - 45000)*0.015))
    }
    BTO_credit <- 0
    benefit_paid <- calc_benefit_abated(work_income,partner_earnings)[["net_benefit"]]*26
    if (benefit_paid > 6000){
      BTO_credit <- 0.15*(benefit_paid - 6000)
    }
    tax <- max(0,(tax_nc - LITO_credit - BTO_credit))
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
  JSP_C_ND_pay <- 562.8
  JSP_C_D_pay <- 612.6
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
  
  # Now calculate the gross benefit amount - this is the amount that is "abated". 
  # Supplement added to payment, but not clear if part of abatement. Base info suggests it is a cliff-face, but OECD documentation points to gradual abatement of all.  Switch the "calc_benefit_abated" call for benefit type to change the treatment. file:///C:/Users/MattNolan/AppData/Local/Temp/Temp1_Australia.zip/Australia_2022.pdf 
  # Current treatment - cliff face
  # Note: Supplements are non-taxable, although they form part of the abating benefit.
  
  calc_benefit_gross <- function(){
    gross_benefit = JSP_pay + PP_pay + calc_rent_assistance(Rent,partnered,Numb_dep,living_alone) + calc_energy_supp(partnered,Have_dep,over_60)
    taxable_benefit = JSP_pay + PP_pay
    supp_benefit = gross_benefit - taxable_benefit
    return(list(gross_benefit = gross_benefit,taxable_benefit = taxable_benefit,supp_benefit = supp_benefit))
  }
  
  # Function to calculate benefit abatement
  
  partner_thresh <- 1196
  partner_thresh_dep <- 1201
  partner_abate <- 0.6
  
  calc_benefit_abated <- function(work_income,partner_earnings) {
    # Abate down based on partner income first.
    taxable_benefit = calc_benefit_gross()[["taxable_benefit"]] 
    gross_benefit = calc_benefit_gross()[["gross_benefit"]] 
    abate = 0
    abate <- ifelse(Have_dep == 1, max((partner_earnings/26 - partner_thresh_dep)*partner_abate,0),max((partner_earnings/26 - partner_thresh)*partner_abate,0))
    tempwi <- work_income/26 # Fortnightly pay
    if (PPeligible == 1 & partnered == 0){
      if (work_income/26 > PP_S_athresh){
        abate <- abate + (tempwi - PP_S_athresh)*PP_S_arate
      }
    }
    else {
      for (i in length(JSP_S_ND_athresh):2) {
        if (work_income/26 > JSP_S_ND_athresh[i]){
          abate <- abate + (tempwi - JSP_S_ND_athresh[i])*JSP_S_ND_arate[i]
          tempwi <- JSP_S_ND_athresh[i]
        }
      }
    }
    taxable_benefit <- taxable_benefit - abate
    net_benefit <- gross_benefit - abate
    return(list(taxable_benefit = max(taxable_benefit,0), net_benefit = max(net_benefit,0)))
  }
  
  #### Family support block
  # This is purely a supplement on top of the benefit - there are differential JSP payments AND a family benefit payment when on JSP.
  # One off payments (i.e. end of year supplement) are not included.
  # Family benefits ARE NOT in taxable income.  Therefore we assess this after determining standard gross income (what is called adjusted income)
  # The family benefit tests do include compulsory superannuation contributions - have to calculate these for both the partner and the individual.
  
  fam_base_threshold <- 103368
  fam_max_threshold <- 58108
  fam_basic_pay <- 63.56
  fam_b_threshold <- 104432
  fam_b_low_thresh <- 6059
  super_cont_rate <- 0.15 # The employer contribution is included in the test for family assistance
  
  calc_family_benefit <- function(child_age,gross_fam_income,work_income,taxable_benefit){
    # Note, the LITSO is not counted in the super information.
    super_contributions = super_cont_rate*(work_income + partner_earnings)
    ATI = gross_fam_income + super_contributions
    net_fam_a = 0
    net_fam_b = 0
    net_fam = 0
    numb_child_12 <- length(child_age[child_age < 13])
    numb_child_19 <- length(child_age[child_age <20 & child_age > 12])
    fam_a <- 197.96*numb_child_12 + 257.46*numb_child_19
    if (ATI < fam_base_threshold & Numb_dep > 0){
      net_fam_a <- max((fam_a*26 - max((ATI-fam_max_threshold),0)*0.2 - max((ATI - fam_base_threshold),0)*0.1)/26,fam_basic_pay*(numb_child_19+numb_child_12))
    }
    else if (Numb_dep > 0) {
      net_fam_a <- max(max((fam_a*26 - max((ATI-fam_max_threshold),0)*0.2 - max((ATI - fam_base_threshold),0)*0.1)/26,(fam_basic_pay*(numb_child_19+numb_child_12)*26 - 0.3*(ATI - fam_base_threshold))/26),0)
    }
    else {
      net_fam_a = 0
    }
    # Add Family Benefit B
    numb_child_5 <- length(child_age[child_age < 5])
    numb_child_18 <- length(child_age[child_age < 19 & child_age > 4])
    fam_b <- 168.28*numb_child_5 + 117.46*numb_child_18
    if (partnered == 0){
      net_fam_b = ifelse(ATI > fam_b_threshold,0,fam_b) # The documentation appears to suggest no abatement, just a cliff face!
    }
    else{
      net_fam_b = ifelse(max(partner_earnings,work_income)*(1+super_cont_rate) > fam_b_threshold,0,max(fam_b-0.2*(min(partner_earnings*(1+super_cont_rate),work_income*(1+super_cont_rate)+taxable_benefit*26)),0))
    }
    net_fam = net_fam_a + net_fam_b
    return(list(net_fam = net_fam,gross_fam = fam_a + fam_b))
  }
  
  ####### EMTR block
  # Function to calculate net incomes
  calc_net_income <- function(wage_rate, hours) {
    work_income <- wage_rate * hours * 52
    abated_benefit <- calc_benefit_abated(work_income,partner_earnings)[["net_benefit"]]
    taxable_benefit <- calc_benefit_abated(work_income,partner_earnings)[["taxable_benefit"]]
    gross_income = work_income + (taxable_benefit)*26
    income_tax <- calc_income_tax(gross_income,work_income)
    gross_fam_income = gross_income + partner_earnings # The supplements are not part of adjusted taxable income
    fam_a_income = calc_family_benefit(child_age,gross_fam_income,work_income,taxable_benefit)[["net_fam"]]*26
    net_income <- gross_income - income_tax + fam_a_income + abated_benefit - taxable_benefit
    
    return(list(net_income =net_income,income_tax = income_tax))
  }
  
  # Function to calculate effective marginal tax rate
  calc_effective_marginal_tax_rate <- function(wage_rate, hours) {
   MTR <- (1 - (calc_net_income(wage_rate,(hours + 1))[["net_income"]] - calc_net_income(wage_rate,hours)[["net_income"]])/(wage_rate*52))
   return(MTR)
  }
  
  # Main function to calculate tax rates for each hour of work from 0 to 60
  marginal_tax_rate_calculator <- function(wage_rate) {
    tax_rates <- numeric((max_hours +1))
    for (hours in 0:max_hours) {
      tax_rates[hours + 1] <- calc_effective_marginal_tax_rate(wage_rate, hours)
    }
    return(tax_rates)
  }
  
  average_tax_rate_calculator <- function(wage_rate,partner_earnings) {
    EATR <- numeric((max_hours +1))
    tax_paid <- numeric((max_hours +1))
    abatement_amount <- numeric((max_hours +1))
    # initial_gross_income <- calc_benefit_abated(0,partner_earnings)[["taxable_benefit"]]*26
    # initial_gross_fam_income = initial_gross_income + partner_earnings
    # initial_income <- calc_benefit_abated(0,partner_earnings)[["net_benefit"]]*26 - calc_income_tax(initial_gross_income,0) + calc_family_benefit(child_age,initial_gross_fam_income,0,calc_benefit_abated(0,partner_earnings)[["taxable_benefit"]])*26
    
    for (hours in 1:max_hours) {
    EATR[hours + 1] <- 1 - (calc_net_income(wage_rate,hours)[["net_income"]] - calc_net_income(wage_rate,0)[["net_income"]])/(wage_rate*hours*52)
    tax_paid[hours + 1] <- calc_net_income(wage_rate,hours)[["income_tax"]]
    abatement_amount[hours + 1] <- calc_benefit_gross()[["gross_benefit"]]*26 - calc_benefit_abated(wage_rate*hours*52,partner_earnings)[["net_benefit"]]*26
    }
    return(list(EATR = EATR,tax_paid = tax_paid,abatement_amount = abatement_amount))
  }
  
  net_income_calculator <- function(wage_rate) {
    net_income_profile <- numeric((max_hours +1))
    for (hours in 0:max_hours) {
      net_income_profile[hours + 1] <- calc_net_income(wage_rate, hours)[["net_income"]]
    }
    return(net_income_profile)
  }
  
  # Calculation of EMTRs:
  tax_rates <- marginal_tax_rate_calculator(wage)
  EMTRspike <- as.numeric(tax_rates > 1)
  for (i in 2:length(tax_rates)){
    tax_rates[i] <- ifelse(tax_rates[i] > 1.1,tax_rates[i-1],tax_rates[i])
  }
    
  taxdf <- data.frame(Hours = seq(0,max_hours,by=1), EMTR = tax_rates)
  
  # Create a data frame with the x-intercepts
  x_intercepts_df <- data.frame(x_intercept = (which(EMTRspike==1)-1))
  
  # Plot of EMTRs
  plot <- ggplot(taxdf,aes(x=Hours,y=EMTR)) + geom_line(colour="#008080") + theme_e61() + labs_e61(
    title = "Tax paid when working an extra hour",
    subtitle = "Effective Marginal Tax Rate",
    y = "     ",
    x = "Hours worked",
    sources = c("e61", cite)
  ) + scale_y_continuous(labels=scales::percent,limits=c(0,1.1),breaks=seq(0,1,by=0.2)) + geom_hline(yintercept = 1,size=1.2,colour="red",linetype="dashed") + theme_e61() + add_e61_logo() + annotate(x=10,y=1.1,label="Losing money working more",vjust=2,geom="label", colour="red") + annotate(x=50,y=taxdf[taxdf$Hours == 50,]$EMTR + 0.2,label="EMTR",vjust=2,geom="label", colour="#008080") + geom_vline(data = x_intercepts_df, aes(xintercept = x_intercept), linetype = "solid", color = "black") 
  
  # Description of EMTRs
  paragraph1 <- paste(
    "The above figure shows what is called the effective marginal tax rate. This refers to the proportion of any income that is given to government - either through tax payments, or a reduction in government benefits received - when you decide to work a little bit more. For example, if you were regularly working ",
    "<span style='color: red;'>", hour_interest, "</span>",
    " hours per week and wanted to increase this to ",
    "<span style='color: red;'>", hour_interest + 1, "</span>",
    " hours this person would earn an additional ",
    "<span style='color: red;'>", paste0("$",format(round(wage * 52, 2), big.mark = ",", decimal.mark = ".", nsmall = 2)), "</span>",
    " per year. However, they would only take home an extra ",
    "<span style='color: red;'>", paste0("$",format(round(wage * 52 * (1 - taxdf[taxdf$Hours == hour_interest,]$EMTR), 2), big.mark = ",", decimal.mark = ".", nsmall = 2)), "</span>",
    "."
  )
  
  # The net income and earning income profiles
  net_incomes_data <- net_income_calculator(wage)
  
  incomesdf <- data.frame(Hours = seq(0,max_hours,by=1), "Take home income" = net_incomes_data) %>% mutate("Work Income" = wage*Hours*52) %>% pivot_longer(!Hours,names_to = "variable",values_to = "value")
  
  # Income plot
  plot_income <- ggplot(incomesdf,aes(x=Hours,y=value/1000,colour=variable)) + geom_line() + theme_e61(legend="bottom") + labs_e61(
    title = "Net Income",
    subtitle = "Annual income",
    y = "(000s)",
    x = "Hours worked",
    sources = c("e61", cite)
  ) + scale_y_continuous(labels = scales::dollar,limits=c(0,max(incomesdf$value/1000)*1.2)) + scale_colour_e61(n=2) + add_e61_logo()
  
  # The EATR data
  EATR_data <- average_tax_rate_calculator(wage,partner_earnings)[["EATR"]]
  
  EATRdf <- data.frame(Hours = seq(0,max_hours,by=1),EATR = EATR_data,EMTR = tax_rates) %>% pivot_longer(!Hours,names_to = "variable",values_to = "values") 
  
  # The EATR plot
  plot_EATR <- ggplot(EATRdf,aes(x=Hours,y=values,colour=variable)) + geom_line() + labs_e61(
    title = "Proportion of earnings paid to government",
    subtitle = "Effective Average Tax Rate",
    y = "     ",
    x = "Hours worked",
    sources = c("e61", cite)
  ) + scale_y_continuous(labels=scales::percent,limits=c(0,1.1),breaks=seq(0,1,by=0.2)) + geom_hline(yintercept = 1,size=1.2,colour="red",linetype="dashed") + theme_e61(legend = "bottom") + add_e61_logo() + scale_colour_e61(n=2)
  
  # Tax paid data
  tax_paid <- average_tax_rate_calculator(wage,partner_earnings)[["tax_paid"]]
  abatement <- average_tax_rate_calculator(wage,partner_earnings)[["abatement_amount"]]
  Taxpaiddf <- data.frame(Hours = seq(0,max_hours,by=1), tax_paid = tax_paid, abatement = abatement) 
  Taxpaiddf <- Taxpaiddf %>% pivot_longer(!Hours,names_to = "variable",values_to = "values")
  
  # Tax paid plot
  plot_TaxAbate <- ggplot(Taxpaiddf,aes(x=Hours,y=values/1000,colour=variable)) + geom_line() + labs_e61(
    title = "Tax and abatement",
    subtitle = "Amounts",
    y = "(000s)",
    x = "Hours worked",
    sources = c("e61", cite),
    footnotes = "Excludes the removal of rental and energy supplements."
  ) + scale_y_continuous(labels = scales::dollar,limits=c(0,max(Taxpaiddf$values/1000) + 5)) + theme_e61(legend = "bottom") + add_e61_logo() + scale_colour_e61(n=2)
  
  # The EATR write-up
  paragraph2 <- paste(
    "The above figure shows what is called the effective average tax rate. This refers to the proportion of any income that is given to government - either through tax payments, or a reduction in government benefits received - when you decide to enter the workforce in a job at that hours level. For example, if you started a job regularly working ",
    "<span style='color: red;'>", hour_interest, "</span>",
    " hours per week this person would have earned ",
    "<span style='color: red;'>", paste0("$",format(round(wage * 52 * hour_interest, 2), big.mark = ",", decimal.mark = ".", nsmall = 2)), "</span>",
    " per year at work. However, they would only take home an extra ",
    "<span style='color: red;'>", paste0("$",format(round(wage * hour_interest * 52 * (1 - EATRdf[EATRdf$Hours == hour_interest & EATRdf$variable == "EATR",]$values), 2), big.mark = ",", decimal.mark = ".", nsmall = 2)), "</span>",
    " compared to not taking on the job."
  )
  
  # The incomes write-up
  paragraph3 <- paste(
    "The above shows the income the person earns and the income you take home for different hours of work. At ",
    "<span style='color: red;'>", hour_interest, "</span>",
    " hours of work this individual earns ",
    "<span style='color: red;'>", paste0("$",format(round(wage * 52 * hour_interest, 2), big.mark = ",", decimal.mark = ".", nsmall = 2)), "</span>",
    " per year at work. Their take-home pay, after subtracting tax and adding benefits received, would be ",
    "<span style='color: red;'>", paste0("$",format(round(calc_net_income(wage, hour_interest)[["net_income"]], 2), big.mark = ",", decimal.mark = ".", nsmall = 2)), "</span>",
    "."
  )
  
  # Family description
  paragraph4 <- paste(
    "The individual income you have modelled is in a family ", ifelse(partnered == 1,"with a partner","without a partner,"),"with ",
    "<span style='color: blue;'>", Numb_dep, " dependent ", ifelse(Numb_dep == 1,"child.","children."),"</span>"," A black line indicates that take home pay will decline if you work an extra hour of work at this point. When not working they are eligible for a fortnightly non-family assistance from government of ",paste0("$",format(calc_benefit_gross()[["gross_benefit"]], big.mark = ",", decimal.mark = ".", nsmall = 2))," and family assistance of ", paste0("$",format(calc_family_benefit(child_age,calc_benefit_gross()[["taxable_benefit"]] + partner_earnings*(1+super_cont_rate),0,calc_benefit_gross()[["taxable_benefit"]])[["net_fam"]], big.mark = ",", decimal.mark = ".", nsmall = 2)),"."
  )
  

  #return(list(plot=plot))
  return(list(plot = plot, plot_EATR = plot_EATR, plot_income = plot_income, paragraph1 = paragraph1, paragraph2 = paragraph2, paragraph3 = paragraph3, paragraph4 = paragraph4, plot_TaxAbate = plot_TaxAbate))
}
 
numeric_values <- unlist(strsplit(input$input7, split = ",")[[1]])
child_age <- as.numeric(numeric_values)

  #ifelse(input$input6 == FALSE,"99",
  
partner_earnings <- ifelse(input$input2 == FALSE,0,input$input3)

Main_carer_dep <- ifelse(input$input6 == FALSE,FALSE,as.numeric(input$input6b))

Rent <- ifelse(input$input8 == "",0,input$input8) # button still broken

result <- calculate_EMTR(wage = input$input1, hour_interest = input$input1b, max_hours = input$input1c, partnered = as.numeric(input$input2), partner_earnings = partner_earnings, over_60 = as.numeric(input$input4), living_alone = as.numeric(input$input5), Have_dep = as.numeric(input$input6), Main_carer_dep = Main_carer_dep, child_ages = child_age, Rent=Rent, cite=input$input9)

#input$input7

output$paragraph4 <- renderText({result$paragraph4})
output$plot <- renderPlot({ result$plot })
output$paragraph1 <- renderText({result$paragraph1})
output$plot_EATR <- renderPlot ({ result$plot_EATR})
output$paragraph2 <- renderText({result$paragraph2})
output$plot_income <- renderPlot({ result$plot_income })
output$paragraph3 <- renderText({result$paragraph3})
output$plot_TaxAbate <- renderPlot({result$plot_TaxAbate})

  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
