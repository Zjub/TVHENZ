
library(shiny)

shiny::runApp() # To test the app

library(rsconnect)
# rsconnect::deployApp('C:/Users/OEM/Documents/GitHub/TVHENZ/AusTaxCalc/R/Stage 3 alternatives tool') # To upload the app

rsconnect::deployApp("C:/Users/OEM/Documents/GitHub/TVHENZ/AusTaxCalc/R/Stage 3 alternatives tool") # To upload the app
