#

library(shiny)

setwd("C:/Users/OEM/e61 Institute Ltd/Data Sets - Documents/SIH/Code/Matt N/SIH data/JSS/Retire Tools/Single Myopic Redundancy")

shiny::runApp() # To test the app

library(rsconnect)
rsconnect::deployApp('C:/Users/OEM/e61 Institute Ltd/Data Sets - Documents/SIH/Code/Matt N/SIH data/JSS/Retire Tools/Single Myopic Redundancy') # To upload the app
