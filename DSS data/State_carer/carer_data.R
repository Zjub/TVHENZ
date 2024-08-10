## Pulling public carer data to look for changes in the trend in carer numbers around the introduction of NDIS
# Date: 9/08/2024

rm(list=ls())

#library(devtools)

# devtools::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

library(cli)
library(tidyverse)
library(data.table)
library(theme61)
library(tidyr)
library(readxl)
library(gghighlight)
library(readabs)
library(OECD)
library(jsonlite)
library(httr)
library(Synth)
library(zoo)

# We are taking the permanent address for the June 2024 cut of the data - we would need to update this to get later data. This is on purpose for version control - the general hyperlink is here: https://data.gov.au/dataset/ds-dga-6ed2d8c0-0162-46da-bbfe-d493f6190af8/details
url <- "https://data.gov.au/data/dataset/6ed2d8c0-0162-46da-bbfe-d493f6190af8/resource/abb9cd50-9611-4a13-bf5b-c21af0f3e060/download/dss-income-support-recipient-monthly-time-series-june-2024.xlsx"
local_file <- tempfile(fileext = ".xlsx") 
download.file(url, destfile = local_file, mode = "wb")

DSS_data <- read_xlsx(local_file, 
                      sheet = "Carer Payment", 
                      col_names = TRUE)
setDT(DSS_data)

DSS_data <- DSS_data[5:nrow(DSS_data), c(1, 33:41)]
colnames(DSS_data) <- c("Date","ACT","NSW","NT","QLD","SA","TAS","VIC","WA","Unknown")

DSS_data$Date <- seq.Date(as.Date("2012-01-01"),as.Date("2024-06-01"),by="month")

#DSS_data[,(names(DSS_data)[-1]) := lapply(.SD, function(x) x/x[1]), .SDcols = names(DSS_data)[-1]]

# Get the initial date data, and make sure all the observations are numeric
initial  <- as.numeric(DSS_data[Date == DSS_data$Date[1]][,2:10])

DSS_data[, (names(DSS_data)[-1]) := lapply(.SD, as.numeric), .SDcols = names(DSS_data)[-1]]

# Change the data to proportional.
DSS_data[, (names(DSS_data)[-1]) := Map(`/`, .SD, initial), .SDcols = names(DSS_data)[-1]]

DSS_data <- DSS_data[,!"Unknown"]

graph_raw_data <- melt(DSS_data,id.vars = "Date")

ggplot(graph_raw_data,aes(y=value,x=Date,colour=variable)) + 
  geom_line() +
  theme_e61(legend = "bottom") +
  geom_vline(xintercept=as.Date("2014-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2017-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2018-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2019-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2020-07-01"),linetype = "dashed")

# Look at data pre-COVID, here Queensland is the only fully non-eligible state.

ggplot(graph_raw_data[Date < as.Date("2020-01-01")],aes(y=value,x=Date,colour=variable)) + 
  geom_line() +
  theme_e61(legend = "bottom") +
  geom_vline(xintercept=as.Date("2014-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2017-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2018-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2019-07-01"),linetype = "dashed") 

ggplot(graph_raw_data[Date < as.Date("2020-01-01") & variable %in% c("QLD","ACT","NSW","SA")],aes(y=value,x=Date,colour=variable)) + 
  geom_line() +
  theme_e61(legend = "bottom") +
  geom_vline(xintercept=as.Date("2014-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2016-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2017-07-01"),linetype = "dashed") # Between 2016 and 2017 was the rollout period

### Useful to think about this relative to population growth.
# Grab populations
population_data <- read_abs(cat_no = "3101.0")
setDT(population_data)

unique(population_data$series)

population_data <- population_data[series %in% c("Estimated Resident Population ;  Persons ;  New South Wales ;","Estimated Resident Population ;  Persons ;  Victoria ;","Estimated Resident Population ;  Persons ;  Queensland ;","Estimated Resident Population ;  Persons ;  South Australia ;","Estimated Resident Population ;  Persons ;  Western Australia ;","Estimated Resident Population ;  Persons ;  Tasmania ;","Estimated Resident Population ;  Persons ;  Northern Territory ;","Estimated Resident Population ;  Persons ;  Australian Capital Territory ;","Estimated Resident Population ;  Persons ;  Australia ;")]

population_data <- population_data[, state := trimws(sub(".*;\\s*(\\w+\\s*\\w+\\s*\\w*)\\s*;\\s*$", "\\1", series))][,.(state,Date = date,value)]

unique(graph_raw_data$variable)

population_data <- population_data[!state == "Australia"]

population_data[state == "New South Wales",state := "NSW"]
population_data[state == "Victoria",state := "VIC"]
population_data[state == "Queensland",state := "QLD"]
population_data[state == "South Australia",state := "SA"]
population_data[state == "Western Australia",state := "WA"]
population_data[state == "Tasmania",state := "TAS"]
population_data[state == "Northern Territory",state := "NT"]
population_data[state == "Australian Capital Territory",state := "ACT"]

colnames(population_data) <- c("variable","Date","population")

population_data[order(variable)]

# Merge with our base dataset
population_data <- population_data[graph_raw_data,on=.(variable,Date)]

# Interpolate to monthly values, as currently every three months.

population_data <- population_data[, population := na.approx(population, x = Date, rule = 2),by=.(variable)][Date >= as.Date("2012-01-01") & Date < as.Date("2020-01-01")]

population_data[,prop := value/population*population[1],by=.(variable)]

ggplot(population_data,aes(y=prop,x=Date,colour=variable)) + 
  geom_line() +
  theme_e61(legend = "bottom") +
  geom_vline(xintercept=as.Date("2014-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2016-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2017-07-01"),linetype = "dashed") # Between 2016 and 2017 was the rollout period

ggplot(population_data[variable %in% c("QLD","ACT","NSW","SA","VIC")],aes(y=prop,x=Date,colour=variable)) + 
  geom_line() +
  theme_e61(legend = "bottom") +
  geom_vline(xintercept=as.Date("2014-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2016-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2017-07-01"),linetype = "dashed") + # Between 2016 and 2017 was the rollout period
  labs_e61(title = "Carer payment per capita",subtitle = "Normalised to 1 in Jan-2012",y="",x="") + 
  scale_y_continuous_e61()


# It is suspicious how similar NSW and VIC look.

ggplot(graph_raw_data[Date < as.Date("2020-01-01") & variable %in% c("VIC","NSW")],aes(y=value,x=Date,colour=variable)) + 
  geom_line() +
  theme_e61(legend = "bottom") +
  geom_vline(xintercept=as.Date("2014-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2016-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2017-07-01"),linetype = "dashed")

ggplot(population_data[Date < as.Date("2020-01-01") & variable %in% c("VIC","NSW")],aes(y=prop,x=Date,colour=variable)) + 
  geom_line() +
  theme_e61(legend = "bottom") +
  geom_vline(xintercept=as.Date("2014-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2016-07-01"),linetype = "dashed") +
  geom_vline(xintercept=as.Date("2017-07-01"),linetype = "dashed")
