# Long-term summary GDP figures

rm(list=ls())

.libPaths(new = 'C:/Rpackage')

library(readabs)
library(tidyverse)
library(theme61)
library(tidyr)
library(readxl)
library(gghighlight)

#For this we should use the annual national accounts. For productivity we will need to use a heads based measure to get a long run measure. For labour income we should use compensation of employees (or just wages and salaries) divided by the number of employees.

# COE per person long term.

myCols <- as.character(read_excel("5204036_Household_Income_Account.xls", sheet = "Data1", n_max = 1, col_names = FALSE))
HIdf <- read_excel("5204036_Household_Income_Account.xls", sheet = "Data1",col_names = FALSE, skip = 10)
colnames(HIdf) <- c("date",myCols)

myColsdeflator <- as.character(read_excel("5204004_Expenditure_on_GDP_IPD.xls", sheet = "Data1", n_max = 1, col_names = FALSE))
Deflators <- read_excel("5204004_Expenditure_on_GDP_IPD.xls",sheet = "Data1", col_names = FALSE, skip = 10)
colnames(Deflators) <- c("date",myColsdeflator)

myColsGDP <- as.character(read_excel("5204001_Key_National_Aggregates.xls", sheet = "Data1", n_max = 1, col_names = FALSE))
GDP <- read_excel("5204001_Key_National_Aggregates.xls",sheet = "Data1", col_names = FALSE, skip = 10)
colnames(GDP) <- c("date",myColsGDP)

Population <- GDP$`GROSS DOMESTIC PRODUCT: Current prices ;`/GDP$`GDP per capita: Current prices ;` # Population in millions

dates <- HIdf$date
year <- substr(dates,1,4)

COE <- HIdf$`Compensation of employees ;`
PCE <- Deflators$`Households ;  Final consumption expenditure ;`

RCOE <- COE/PCE

RCOEPC <- RCOE/(Population)

df <- as.tibble(cbind(as.numeric(year),RCOEPC)) %>% mutate(year10 = lag(RCOEPC,n=10)) %>% mutate(growth = (RCOEPC - year10)/year10) %>% mutate(anngrowth = (RCOEPC - year10)^(1/10)/year10) %>% filter(V1 %in% c(1970,1980,1990,2000,2010,2020))

graphdf <- as.tibble(cbind(c(1960,1970,1980,1990,2000,2010),df$anngrowth))
colnames(graphdf) <- c("Year","COE Per Capita")

ggplot(graphdf, aes(x=Year,y=`COE Per Capita`)) + geom_bar(stat="identity")

dim(HIdf)



# GDP per person long term.