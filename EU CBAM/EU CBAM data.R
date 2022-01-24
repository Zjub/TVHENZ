# An exercise pulling data related to the EU CBAM suggested policy.

rm(list=ls()) # Clear prior memory

library(tidyverse)
library(readxl)
library(cowplot)

#EUCBAMcat <- c(25231000,25232100,25232900,25239000,27160000,28080000,2814,28342100,3102,3105,31056000,72,7202,7204,7301,7302,730300,7304,7305,7306,7307,7308,7309,7310,7311,7601,7603,7604,7605,7606,7607,7608,76090000) 
EUCBAMcat <- c(25231000,25232100,25232900,25239000,27160000,28080000,2814,28342100,3102,3105,31056000,72,7202,7204,7301,7302,730300,7304,7305,7306,7307,7308,7309,7310,7311,7601,7603,7604,7605,7606,7607,7608,76090000) # The CN codes needed to create the EU CBAM proposal as per https://ec.europa.eu/info/sites/default/files/carbon_border_adjustment_mechanism_0.pdf

#url <- ("https://www.stats.govt.nz/assets/Uploads/overseas-merchandise-trade-datasets/omt-datasets-november-2021/Aug-2021-Imports-HS10.csv") # Where data is available. This is the latest month of final data for testing the initial code.

#Importdata <- read.csv(url)

#url("https://www3.stats.govt.nz/HS10_by_Country/2020_Imports_HS10.zip?_ga=2.96332828.1424037466.1643047466-1124821197.1627786616") # 2020 year zipped file

#Importdata <- read.csv(url) # Importing directly from this url does not work
#download.file("https://www3.stats.govt.nz/HS10_by_Country/2020_Imports_HS10.zip?_ga=2.96332828.1424037466.1643047466-1124821197.1627786616","data.zip")
#unzip("data.zip") # zip file is too large and so downloads corrupted.  Have to download amd umzip manually from your workspace.

Importdata2020 <- read.csv("2020_Imports_HS10.csv")



# Drop everything with a leading zero (i.e. with 9 characters)

for (i in 1:length(EUCBAMcat)){
  nam <- paste("CBAMdata",i,sep="")
  info <- Importdata[as.numeric(substr(Importdata$Harmonised.System.Code,1,nchar(EUCBAMcat[i]))) == EUCBAMcat[i],]
  assign(nam,info)
}

Cement <- rbind(CBAMdata1,CBAMdata2,CBAMdata3,CBAMdata4)

Electricity <- CBAMdata5

Fertilisers <- rbind(CBAMdata6,CBAMdata7,CBAMdata8,CBAMdata9,CBAMdata10) # Note that 11 is subtracted from the total ,CBAMdata11

Fertilisers <- filter(Fertilisers, !(Harmonised.System.Code %in% CBAMdata11$Harmonised.System.Code))

IronSteel <- rbind(CBAMdata12,CBAMdata15,CBAMdata16,CBAMdata17,CBAMdata18,CBAMdata19,CBAMdata20,CBAMdata21,CBAMdata22,CBAMdata23,CBAMdata24,CBAMdata25) # Note that CBAMdata13, CBAMdata14

IronSteel <- filter(IronSteel, !(Harmonised.System.Code %in% CBAMdata13$Harmonised.System.Code))
IronSteel <- filter(IronSteel, !(Harmonised.System.Code %in% CBAMdata14$Harmonised.System.Code))

Aluminium <- rbind(CBAMdata26,CBAMdata27,CBAMdata28,CBAMdata29,CBAMdata30,CBAMdata31,CBAMdata32,CBAMdata33)

# Select from each category - as there are no total categories the subsetting needs to be dependent on the length of the numeric string for category)

XXX