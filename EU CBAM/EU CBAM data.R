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
Importdata2019 <- read.csv("2019_Imports_HS10.csv")
Importdata2018 <- read.csv("2018_Imports_HS10.csv")

Exportdata2020 <- read.csv("2020_Exports_HS10.csv")
Exportdata2019 <- read.csv("2019_Exports_HS10.csv")
Exportdata2018 <- read.csv("2018_Exports_HS10.csv")

# Drop everything with a leading zero (i.e. with 9 characters)

#Importdata <- filter(Importdata, !(nchar(Harmonised.System.Code) < 10))

Importdata2020 <- filter(Importdata2020, !(nchar(Harmonised.System.Code) < 10))
Importdata2019 <- filter(Importdata2019, !(nchar(Harmonised.System.Code) < 10))
Importdata2018 <- filter(Importdata2018, !(nchar(Harmonised.System.Code) < 10))
Importdata <- rbind(Importdata2018,Importdata2019,Importdata2020)

Exportdata2020 <- filter(Exportdata2020, !(nchar(Harmonised.System.Code) < 10))
Exportdata2019 <- filter(Exportdata2019, !(nchar(Harmonised.System.Code) < 10))
Exportdata2018 <- filter(Exportdata2018, !(nchar(Harmonised.System.Code) < 10))
Exportdata <- rbind(Exportdata2018,Exportdata2019,Exportdata2020)

# Select from each category - as there are no total categories the subsetting needs to be dependent on the length of the numeric string for category)

for (i in 1:length(EUCBAMcat)){
  nam <- paste("CBAMdataImp",i,sep="")
  info <- Importdata[as.numeric(substr(Importdata$Harmonised.System.Code,1,nchar(EUCBAMcat[i]))) == EUCBAMcat[i],]
  assign(nam,info)
}

for (i in 1:length(EUCBAMcat)){
  nam <- paste("CBAMdataExp",i,sep="")
  info <- Exportdata[as.numeric(substr(Importdata$Harmonised.System.Code,1,nchar(EUCBAMcat[i]))) == EUCBAMcat[i],]
  assign(nam,info)
}

CementImp <- rbind(CBAMdataImp1,CBAMdataImp2,CBAMdataImp3,CBAMdataImp4)

ElectricityImp <- CBAMdataImp5

FertilisersImp <- rbind(CBAMdataImp6,CBAMdataImp7,CBAMdataImp8,CBAMdataImp9,CBAMdataImp10) # Note that 11 is subtracted from the total ,CBAMdataImp11

FertilisersImp <- filter(FertilisersImp, !(Harmonised.System.Code %in% CBAMdataImp11$Harmonised.System.Code))

IronSteelImp <- rbind(CBAMdataImp12,CBAMdataImp15,CBAMdataImp16,CBAMdataImp17,CBAMdataImp18,CBAMdataImp19,CBAMdataImp20,CBAMdataImp21,CBAMdataImp22,CBAMdataImp23,CBAMdataImp24,CBAMdataImp25) # Note that CBAMdataImp13, CBAMdataImp14

IronSteelImp <- filter(IronSteelImp, !(Harmonised.System.Code %in% CBAMdataImp13$Harmonised.System.Code))
IronSteelImp <- filter(IronSteelImp, !(Harmonised.System.Code %in% CBAMdataImp14$Harmonised.System.Code))

AluminiumImp <- rbind(CBAMdataImp26,CBAMdataImp27,CBAMdataImp28,CBAMdataImp29,CBAMdataImp30,CBAMdataImp31,CBAMdataImp32,CBAMdataImp33)

CementExp <- rbind(CBAMdataExp1,CBAMdataExp2,CBAMdataExp3,CBAMdataExp4)

ElectricityExp <- CBAMdataExp5

FertilisersExp <- rbind(CBAMdataExp6,CBAMdataExp7,CBAMdataExp8,CBAMdataExp9,CBAMdataExp10) # Note that 11 is subtracted from the total ,CBAMdataExp11

FertilisersExp <- filter(FertilisersExp, !(Harmonised.System.Code %in% CBAMdataExp11$Harmonised.System.Code))

IronSteelExp <- rbind(CBAMdataExp12,CBAMdataExp15,CBAMdataExp16,CBAMdataExp17,CBAMdataExp18,CBAMdataExp19,CBAMdataExp20,CBAMdataExp21,CBAMdataExp22,CBAMdataExp23,CBAMdataExp24,CBAMdataExp25) # Note that CBAMdataExp13, CBAMdataExp14

IronSteelExp <- filter(IronSteelExp, !(Harmonised.System.Code %in% CBAMdataExp13$Harmonised.System.Code))
IronSteelExp <- filter(IronSteelExp, !(Harmonised.System.Code %in% CBAMdataExp14$Harmonised.System.Code))

AluminiumExp <- rbind(CBAMdataExp26,CBAMdataExp27,CBAMdataExp28,CBAMdataExp29,CBAMdataExp30,CBAMdataExp31,CBAMdataExp32,CBAMdataExp33)

