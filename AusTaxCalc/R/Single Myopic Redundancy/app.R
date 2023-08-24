# Load the necessary packages
library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Income Shocks and Superannuation Access"),
  
  sidebarLayout(
    sidebarPanel(
      # Input widgets
      numericInput("input1", "Earnings in first job ($000s):", value = 40),
      numericInput("input2", "Wage growth (%):", value = 3),
      numericInput("input3", "Added wage growth when under 30 (%)", value = 2),
      numericInput("input5", "Added wage growth when over 45 (%)", value = -2),
      numericInput("input8", "Real return on superannuation (%)", value = 2),
      numericInput("input10", "Age starting work", value = 20),
      numericInput("input11", "Retirement Age", value = 67),
      numericInput("input12", "End of life", value = 90),
      textInput("input14", "Enter years when you face a income shock (seperated by a comma), starting from the start of work life", value="10,20,30"),
      textInput("input15", "What proportion of gross income do you keep during each shock (i.e. 0.3 is 70% income loss)", value ="0.5,0,0.5"),
      checkboxInput("input16", "Do we allow superwithdrawals?", value = TRUE),
      textInput("input13","Your name:", value = "youtube.com/@tvhe"),
      
      # Button to update the graph and paragraph
      actionButton("update", "Update")
    ),
    
    mainPanel(
      # Output elements
      plotOutput("plot"),
      textOutput("paragraph")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  observeEvent(input$update, {
    # Your R function that returns a graph and a paragraph
    # Extend the single tool to evaluate income shocks - and visualise them
    calculate_lifetime_spend_myopia_shocks <- function(initial_labour_earnings = 40000, wage_growth = 0.03, youth_wageg=0.02, mid_wageg=0, old_wageg=-0.02, retirement_pension = 1064*26, JSP = 701.9*26, interest_rate = 0.02, comp_super_rate = 0.105, labour_start_year = 20, retirement_year = 67, death_year = 90, cite="",shock_periods = c(10,20,30),income_reduction = c(0.5,0,0.5),superwithdraw = TRUE) {
      library(theme61)
      library(tidyverse)
      
      # Calculate the number of years from the start of labour earnings to retirement
      num_years_labour <- retirement_year - labour_start_year
      
      # Initialize variables
      total_super <- 0.0
      total_super_non_withdraw <- 0.0
      total_tax <- 0.0
      total_income_labour <- 0.0
      total_inc_labour_with <- 0.0
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
      abate1 <- 0.5
      abate2 <- 0.6
      assthresh <- 504500
      assabate <- 0.003*26 # Declines by $3 per fortnight for every $1000 over threshold.
      incretthresh <- 190*26 # Pension income threshold
      incretabate <- 0.5 # Pension income abatement
      young_age <- 30
      mid_age <- 45
      old_age <- retirement_year
      
      work_incomes <- numeric(length = num_years_labour)
      non_with_work_incomes <- numeric(length = num_years_labour)
      non_with_super_balance <- numeric(length = num_years_labour)
      super_balance <- numeric(length = num_years_labour)
      withdraws <- numeric(length = num_years_labour)
      
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
      
      bencalc <- function(inc) {
        if (inc <= bthresh1) {
          return(JSP)
        } else if (inc <= bthresh2) {
          return(JSP - (inc - bthresh1)*abate1)
        } else {
          return(max(JSP - (inc - bthresh2)*abate2 - (bthresh2 - bthresh1)*abate1,0))
        }
      }
      
      shock <- rep(1,times=num_years_labour)
      shock[shock_periods] <- shock[shock_periods]*income_reduction
      
      # Loop through each year from the start of labour earnings to the year of retirement
      labour_earnings <- initial_labour_earnings
      labour_earnings_trend <- initial_labour_earnings
      for (year in 1:num_years_labour) {
        # Calculate the income and tax for the current year
        age <- labour_start_year + year -1
        wage_g <- ifelse(age == labour_start_year,0,ifelse(age <= young_age,wage_growth+youth_wageg,ifelse(age <= mid_age,wage_growth + mid_wageg,wage_growth + old_wageg)))
        labour_earnings_trend <- labour_earnings_trend*(1+wage_g) # The base income track that an individual returns to after a shock
        labour_earnings <- labour_earnings_trend*shock[year]
        benefit <- bencalc(labour_earnings)
        income <- labour_earnings + benefit
        tax <- taxcalc(income)
        
        # Allow superwithdrawals of up to $10,000 towards matching "target" disposable income if true - the sum is untaxed, and does not influence benefit eligibility.  Allow balances to go negative.
        
        withdraw = ifelse(shock[year] != 1 & superwithdraw == TRUE,min(10000,labour_earnings_trend+ bencalc(labour_earnings_trend) - taxcalc(labour_earnings_trend) - (income-tax)),0)
        withdraws[year] <- withdraw
        
        # Update the total savings, tax, and income from labour
        total_super <- total_super + (labour_earnings * comp_super_rate + total_super * interest_rate) * (1 - supertax) - withdraw
        total_super_non_withdraw <- total_super_non_withdraw + (labour_earnings * comp_super_rate + total_super_non_withdraw * interest_rate) * (1 - supertax)
        total_tax <- total_tax + tax
        total_income_labour <- total_income_labour + income - tax
        total_inc_labour_with <- total_inc_labour_with + income - tax + withdraw
        non_with_work_incomes[year] <- income - tax
        non_with_super_balance[year] <- total_super_non_withdraw
        work_incomes[year] <- income - tax + withdraw
        super_balance[year] <- total_super
      }
      
      # Calculate total assets given superannuation and other savings
      
      ret_total_assets = total_super
      ret_total_assets_nowithdrawal = total_super_non_withdraw
      
      # Calculate the number of years from retirement to death
      num_years_retirement <- death_year - retirement_year
      
      # Calculate the amount the can be evenly allocated by year based on a certain year of death
      
      # Function that applies the assets and income tests on the pension.  Income used is currently gross interest income only.
      
      get_pension <- function(total_assets,age) {
        threshold <- assthresh
        incthreshold <- incretthresh
        reduction_rate <- assabate
        red_inc_rate <- incretabate
        max_pension <- retirement_pension
        pension_gross_inc <- total_assets*interest_rate
        if (age < 67) { 
          pension <- 0
        } else if (total_assets >= threshold) {
          pension <- max(max_pension - reduction_rate * (total_assets - threshold), 0)
        } else if (pension_gross_inc > incthreshold){
          pension <- max(max_pension - red_inc_rate*(pension_gross_inc - incthreshold),0) # For the final version run both asset and income tests together and select the lowest amount of them (vs zero).
        } else {
          pension <- max_pension
        }
        return(pension)
      }
      
      rpension <- numeric(length=num_years_retirement)
      rassets <- numeric(length=num_years_retirement)
      
      # Define function to calculate final asset value given a consumption amount
      get_final_assets <- function(total_assets, interest_rate, num_years_retirement, consumption) {
        for (i in 1:num_years_retirement) {
          age <- retirement_year + i - 1
          pension <- get_pension(total_assets,age)
          rpension[i] <- pension
          total_assets <- total_assets + total_assets * (interest_rate*(1-supertax)) + pension - consumption
          rassets[i] <- total_assets
        }
        return(total_assets)
      }
      
      # Use recursive algorithm to find consumption amount that results in zero final assets
      get_consumption <- function(total_assets, interest_rate, num_years_retirement, epsilon=1) {
        # Define function to find final asset value for a given consumption amount
        f <- function(c) {
          return(get_final_assets(total_assets, interest_rate, num_years_retirement, c))
        }
        
        # Use binary search to find consumption amount that results in zero final assets
        lower <- retirement_pension
        upper <- (total_assets * interest_rate*(1-supertax) * (1 + interest_rate*(1-supertax))^num_years_retirement) / (((1 + interest_rate*(1-supertax))^num_years_retirement) - 1) + retirement_pension # Upper bound is a PV split of assets through time plus the full pension.
        while (upper - lower > 0.01) {
          mid <- (lower + upper) / 2
          final_assets <- get_final_assets(total_assets, interest_rate, num_years_retirement, mid)
          if (final_assets < 0) {
            upper <- mid
          } else {
            lower <- mid
          }
        }
        return((lower + upper) / 2)
      }
      
      # Calculate retirement consumption amount
      consumption <- get_consumption(ret_total_assets, interest_rate, num_years_retirement)
      
      ## Checks aren't populating - need to look into that
      # Check the calculations - if all zeros then the functions haven't populated anything
      #plot(rpension)
      #plot(rassets)
      
      avg_income_labour = total_inc_labour_with/num_years_labour
      total_income_retirement = consumption*num_years_retirement
      avg_income_retirement = consumption
      lifetime_income = total_inc_labour_with + total_income_retirement
      savingsretire = total_super
      
      tempincs <- c(work_incomes,rep(consumption,times=num_years_retirement))
      tempwlavg <- rep(avg_income_labour,times=death_year-labour_start_year)
      tempages <- seq(labour_start_year,death_year-1,by=1)
      graphdf <- data.frame(tempincs = tempincs,tempwlavg =tempwlavg,tempages = tempages)
      graphdf <- pivot_longer(graphdf,!tempages,names_to = "variable",values_to = "value")
      
      if (superwithdraw == FALSE){
        Assumption <- paste("FY2023 Tax-Transfers, initial income of $",format(round(initial_labour_earnings), big.mark = ",", decimal.mark = ".", nsmall = 0),", wage growth of ",wage_growth*100,"%,interest rates at ",round(interest_rate*100,2),"%",sep="")
        
        plot <- ggplot(graphdf,aes(x=tempages,y=value,colour=variable,linetype=variable)) + geom_line() + labs_e61(
          title = "Lifetime annual income profile",
          subtitle = "Single Myopic Individual, no children",
          y="    ",
          x="Age",
          sources = c("e61", cite),
          footnotes = c("All incomes are net of taxes and transfers (disposable incomes) ex rent assistance",Assumption)
        ) + scale_y_continuous(labels=scales::dollar, limits = c(min(tempincs)-5000,max(tempincs + 5000))) + theme_e61() + geom_vline(xintercept = retirement_year,colour="red",linetype="dotted") + add_e61_logo() + scale_colour_manual(values=c(e61_orangedark,e61_tealdark)) + scale_linetype_manual(values=c("tempincs" = "solid","tempwlavg" = "dashed")) + annotate(x=retirement_year,y=min(tempincs)-1000,label="Retirement Time",vjust=2,geom="label", colour="red") + annotate(x=death_year-12,y=ifelse(avg_income_retirement > avg_income_labour, avg_income_labour,avg_income_labour + 5000),label="Average Worklife Earnings",vjust=2,geom="label", colour=e61_tealdark) + annotate(x=(labour_start_year + retirement_year)/2,y=max(work_incomes)+5000,label="Annual Earnings",vjust=2,geom="label", colour=e61_orangedark)
        
        paragraph <- paste("Pre-retirement this person annually spends $", format(round(avg_income_labour, 2), big.mark = ",", decimal.mark = ".", nsmall = 2),
                           " while post-retirement they spend $", format(round(avg_income_retirement,2), big.mark = ",", decimal.mark = ".", nsmall = 2),
                           ". This is $", format(round(avg_income_labour - avg_income_retirement,2), big.mark = ",", decimal.mark = ".", nsmall = 2),
                           " more. They retire with $", format(savingsretire, big.mark = ",", decimal.mark = ".", nsmall = 2), " in savings due to compulsory contributions.",sep="")
        
        return(list(plot = plot, paragraph = paragraph))
      }
      
      else {
        # The purpose of this else statement is also to construct a "third" line that shows the case where there aren't withdrawals against those that do.
        consumption_nowithdrawal <- get_consumption(ret_total_assets_nowithdrawal, interest_rate, num_years_retirement)
        
        avg_income_labour_with = total_income_labour/num_years_labour
        total_income_retirement_nowith = consumption_nowithdrawal*num_years_retirement
        avg_income_retirement = consumption_nowithdrawal
        lifetime_income = total_income_labour + total_income_retirement_nowith
        savingsretire_nowith = total_super_non_withdraw
        
        tempincs_nowith <- c(non_with_work_incomes,rep(consumption_nowithdrawal,times=num_years_retirement))
        tempwlavg_nowith <- rep(avg_income_labour_with,times=death_year-labour_start_year)
        tempages <- seq(labour_start_year,death_year-1,by=1)
        graphdf <- data.frame(tempincs = tempincs,tempwlavg_nowith =tempwlavg_nowith,tempages = tempages,tempincs_nowith = tempincs_nowith)
        graphdf <- pivot_longer(graphdf,!tempages,names_to = "variable",values_to = "value")
        Assumption <- paste("[To be corrected] FY2023 Tax-Transfers, initial income of $",format(round(initial_labour_earnings), big.mark = ",", decimal.mark = ".", nsmall = 0),", wage growth of ",wage_growth*100,"%,interest rates at ",round(interest_rate*100,2),"%",sep="")
        
        plot <- ggplot(graphdf %>% filter(variable != tempwlavg_nowith),aes(x=tempages,y=value,colour=variable,linetype=variable)) + geom_line() + labs_e61(
          title = "Income shocks and Superannuation access",
          subtitle = "Single Myopic Individual, no children",
          y="    ",
          x="Age",
          sources = c("e61", cite),
          footnotes = c("All incomes are net of taxes and transfers (disposable incomes) ex rent assistance",Assumption)
        ) + scale_y_continuous(labels=scales::dollar, limits = c(min(tempincs_nowith)-5000,max(tempincs + 5000))) + theme_e61() + geom_vline(xintercept = retirement_year,colour="red",linetype="dotted") + add_e61_logo() + scale_colour_manual(values=c(e61_orangedark,e61_tealdark,e61_teallight)) + scale_linetype_manual(values=c("tempincs" = "solid","tempincs_nowith" = "dashed")) + annotate(x=retirement_year,y=min(tempincs_nowith)-1000,label="Retirement Time",vjust=2,geom="label", colour="red") + annotate(x=(labour_start_year + retirement_year)/(2),y=min(work_incomes)-1500,label="Annual Earnings + Withdrawals",vjust=2,geom="label", colour=e61_orangedark) + annotate(x=(labour_start_year + retirement_year)/2,y=max(work_incomes)+5000,label="Annual Earnings No Withdrawals",vjust=2,geom="label", colour=e61_tealdark)
        
        paragraph <- paste("The difference between peak retirement savings due to withdrawals is $", format(round(savingsretire_nowith - savingsretire,2), big.mark = ",", decimal.mark = ".", nsmall = 2), ". Total withdrawals were $", format(round(sum(withdraws),2), big.mark = ",", decimal.mark = ".", nsmall = 2),".")
        
        
        return(list(plot = plot, paragraph = paragraph))
      }
    }
    
    
    # Defaults: initial_labour_earnings = 40000, wage_growth = 0.03, youth_wageg=0.02, mid_wageg=0, old_wageg=-0.02, retirement_pension = 1064*26, JSP = 700*26, interest_rate = 0.02, comp_super_rate = 0.105, labour_start_year = 20, retirement_year = 65, death_year = 90, cite = ""
    
    numeric_values <- strsplit(input$input14, split = ",")[[1]]
    shocks <- as.numeric(numeric_values)
    numeric_values <- strsplit(input$input15, split = ",")[[1]]
    inc_loss <- as.numeric(numeric_values)
    
    result <- calculate_lifetime_spend_myopia_shocks(initial_labour_earnings = input$input1*1000, wage_growth = input$input2/100, youth_wageg = input$input3/100, old_wageg = input$input5/100, interest_rate = input$input8/100, labour_start_year = input$input10, retirement_year = input$input11, death_year = input$input12, cite = input$input13, shock_periods = shocks,income_reduction = inc_loss,superwithdraw = input$input16)
    
    output$plot <- renderPlot({
      result$plot
    })
    
    output$paragraph <- renderPrint({
      result$paragraph
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
