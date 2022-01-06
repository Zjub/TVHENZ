# Purpose of this is to pull in a variety of New Zealand data sources and create a few useful visualizations

rm(list=ls()) # Clear prior memory

library(dplyr)
library(tidyr)
library(readxl)

## MSD 

# Job seeker numbers

#url <- paste0(
#  "https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources",
#  "/statistics/benefit/2015/quarterly-benefit-fact-sheets-national-benefit-tables-june-2015.xls"
#)

#download.file(url = url, destfile = "benefits.xls", mode = "wb")

#sheetnames <- read_excel("benefits.xls")[3:10,1]

#bendata <- read_excel("benefits.xls",sheet=as.character(sheetnames[3,1]))

# MSD stuff I can't get it to read the sheet name.  Come back to, do Stats NZ now.

## Stats NZ
  
# For the latest csv's go here https://www.stats.govt.nz/large-datasets/csv-files-for-download/

# For API, base on this:  https://github.com/StatisticsNZ/open-data-api/blob/main/Example-R-requests.md

# Business employment data

url <- "https://www.stats.govt.nz/assets/Uploads/Business-employment-data/Business-employment-data-September-2021-quarter/Download-data/business-employment-data-september-2021-quarter-csv.zip"

download.file(url,"data.zip")
unzip("data.zip")
a <- read.csv("Machine-readable-business-employment-data-sep-2021-quarter.csv")

# Attempt showing that we can't do this without downloading first:  a <- read.csv(unz("https://www.stats.govt.nz/assets/Uploads/Business-employment-data/Business-employment-data-September-2021-quarter/Download-data/business-employment-data-september-2021-quarter-csv.zip","Machine-readable-business-employment-data-sep-2021-quarter.csv"),row.names = NULL)

# Business Operations Survey

b <- read.csv("https://www.stats.govt.nz/assets/Uploads/Business-operations-survey/Business-operations-survey-2020/Download-data/business-operations-survey-2020-covid-19-csv.csv",row.names = NULL)

## RBNZ

# library(RBNZ) - this doesn't work directly anymore due to extra security. https://github.com/rntq472/RBNZ

