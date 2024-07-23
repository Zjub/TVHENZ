## Pulls in functions and policies to generate income and tax measures ----
# Last update: 26/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan

#setwd("C:/Users/OEM/Documents/Github/TVHENZ/AusTaxCalc/R/Stage 3 Tax Cuts") # Make this global environment

# Load the necessary packages
library(shiny)

ui <- fluidPage(
  # Custom edits
  #tags$head()

  # Side panel title
  titlePanel("Stage 3 Tax cuts (for a 25-54 year old)"),

  sidebarLayout(
    sidebarPanel(
      # Style details
      style = "background-color: #ADD8E6;",

      # Button to update the graph and paragraph
      actionButton("update", "Update"),

      # Input widgets
      numericInput("input1", "What is this person's hourly wage ($, excluding compulsory superannuation contributions)", value = 25),
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
      checkboxInput("input5", "Is this person living alone", value = FALSE),
      numericInput("input8", "How much does this person pay in rent per fortnight", value = 500),
      textInput("input11", "Enter your tax rates (seperated by a comma) - i.e 0.19,0.325,0.37,0.45 for the current tax rates", value="0.19,0.325,0.37,0.45"),
      textInput("input12", "Enter your tax thresholds (seperated by a comma) - i.e 18200,45000,120000,180000 for the current tax scales", value="18200,45000,120000,180000"),


      # Button to update the graph and paragraph
      actionButton("update2", "Update")
    ),

    mainPanel(
      # Style settings

      # Output elements
      tags$div(style = "text-align: center; font-style: italic;", "Alpha Version 0.1. Stage 3 tax cuts for a renter aged 25-54, with no significant assets - medicare excluded"),
      tags$bOr(),
      tags$div(style = "text-align: left;","In the graphs below we will look at your suggested tax scale. Click update on the left of this screen to construct the graphs."),
      tags$br(),
      plotOutput("plot"),
      plotOutput("plot2")

    )
  )
)


# Define the server
server <- function(input, output, session) {

  observeEvent(c(input$update, input$update2), {



TAXBEN_calc <- function(wage = 25,hour_interest = 20, max_hours = 60, partnered = 0, partner_earnings = 0, over_60 = 0, living_alone = 0, Have_dep = 0, Main_carer_dep = 0, child_ages = "99",Rent=200,cite="tvhe.co.nz"){
#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  data.table,
  theme61)

library(theme61) # Having some specific issues pulling this package in.
# Include functions ----

scripts <- list.files(pattern = "calc_")
for (script in scripts){
  source(script)
}

# Import parameters for the given quarter ----

policy_date1 <- "Q4_2023"

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

  # Individual characteristics turned off
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

  # Citation
  cite <- "tvhe.co.nz"


  ## Set up income plot and text ----

  net_income_calculator <- function(wage_rate) {
    net_income_profile <- numeric((max_hours +1))
    for (hours in 0:max_hours) {
      net_income_profile[hours + 1] <- calc_net_income(wage_rate, hours)[["net_income"]]
    }
    return(net_income_profile)
  }

  net_incomes_data <- net_income_calculator(wage)

  incomesdf <- data.frame(Hours = seq(0,max_hours,by=1)) %>% mutate("Take home income" = net_incomes_data) %>% mutate("Work Income" = wage*Hours*52) %>% pivot_longer(!Hours,names_to = "variable",values_to = "value")

  plot_income <- ggplot(incomesdf,aes(x=Hours,y=value/1000,colour=variable)) + geom_line() + theme_e61(legend="bottom") + labs_e61(
    title = paste0("Net Income at $", wage," hourly wage"),
    subtitle = "Annual income",
    y = "(000s)",
    x = "Hours worked",
    sources = c(cite)
  ) + scale_y_continuous(labels = scales::dollar,limits=c(0,max(incomesdf$value/1000)*1.2))

## Add new tax rates and compare incomes ----
  # Note: This writes over the prior brackets - for a future version would prefer to have the functions read in a file for brackets and scales, which allows us to pass different ones through the function.
  tax_free <- 18200
  tax_threshold_1 <- 45000
  tax_threshold_2 <- 120000
  tax_threshold_3 <- 200000

  tax_rate_1 <- 0.19
  tax_rate_2 <- 0.30
  tax_rate_3 <- 0.30
  tax_rate_4 <- 0.45

  tax_brackets <- c(0, tax_free, tax_threshold_1, tax_threshold_2, tax_threshold_3)
  tax_scale <- c(0, tax_rate_1, tax_rate_2, tax_rate_3, tax_rate_4)

  net_incomes_new_data <- net_income_calculator(wage)

  incomesdf_new <- data.frame(Hours = seq(0,max_hours,by=1)) %>% mutate("Take home income (new)" = net_incomes_new_data) %>% mutate("Work Income (new)" = wage*Hours*52) %>% pivot_longer(!Hours,names_to = "variable",values_to = "value")

  plot_income_new <- ggplot(incomesdf_new,aes(x=Hours,y=value/1000,colour=variable)) + geom_line() + theme_e61(legend="bottom") + labs_e61(
    title = "Net Income (after tax changes)",
    subtitle = "Annual income",
    y = "(000s)",
    x = "Hours worked",
    sources = c(cite)
  ) + scale_y_continuous(labels = scales::dollar,limits=c(0,max(incomesdf_new$value/1000)*1.2))

  plot_income
  plot_income_new

  comparison_df <- data.frame(Hours = seq(0,max_hours,by=1)) %>% mutate("Take home income (new)" = net_incomes_new_data) %>% mutate("Take home income (current)" = net_incomes_data) %>% pivot_longer(!Hours,names_to = "variable",values_to = "value")

  comparison_graph <- ggplot(comparison_df,aes(x=Hours,y=value/1000,colour=variable)) + geom_line() + theme_e61(legend="bottom") + labs_e61(
    title = paste0("Net Income comparison at $",wage," hourly wage"),
    subtitle = "Annual income",
    y = "(000s)",
    x = "Hours worked",
    sources = c(cite)
  ) + scale_y_continuous(labels = scales::dollar,limits=c(0,max(comparison_df$value/1000)*1.2))

  comparison_graph


  return(list(plot = plot_income, plot2 = comparison_graph))
}


  numeric_values <- unlist(strsplit(input$input7, split = ",")[[1]])
  child_age <- as.numeric(numeric_values)

  partner_earnings <- ifelse(input$input2 == FALSE,0,input$input3)

  Main_carer_dep <- ifelse(input$input6 == FALSE,FALSE,as.numeric(input$input6b))

  Rent <- ifelse(input$input8 == "",0,input$input8)

  result <- TAXBEN_calc(wage = input$input1, max_hours = input$input1c, partnered = as.numeric(input$input2), partner_earnings = partner_earnings, living_alone = as.numeric(input$input5), Have_dep = as.numeric(input$input6), Main_carer_dep = Main_carer_dep, child_ages = child_age, Rent=Rent)

  output$plot <- renderPlot({ result$plot })
  output$plot2 <- renderPlot({ result$plot2 })

  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
