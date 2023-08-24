#

.libPaths(new = 'C:/Rpackage')

library(shiny)
# 
# setwd("C:/Users/OEM/e61 Institute Ltd/Data Sets - Documents/SIH/Code/Matt N/SIH data/JSS/Retire Tools/EMTRs")

setwd("C:/Users/MattNolan/OneDrive - e61 Institute Ltd/Documents - Data Sets/SIH/Code/Matt N/SIH data/JSS/Retire Tools/EMTRs")


shiny::runApp() # To test the app

library(rsconnect)
# rsconnect::deployApp('C:/Users/OEM/e61 Institute Ltd/Data Sets - Documents/SIH/Code/Matt N/SIH data/JSS/Retire Tools/EMTRs') # To upload the app

rsconnect::deployApp("C:/Users/MattNolan/OneDrive - e61 Institute Ltd/Documents - Data Sets/SIH/Code/Matt N/SIH data/JSS/Retire Tools/EMTRs") # To upload the app
