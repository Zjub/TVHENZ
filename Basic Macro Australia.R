# Construct a simple outline of key macroeconomic variables in Australia, and simple time series models.

rm(list=ls()) # Clear prior memory

library(tidyverse)
library(readxl)
library(readabs) # A ABS specific R package.

Sys.getenv("R_READABS_PATH")
#Sys.setenv(R_READABS_PATH  = <path>) # Code for setting working directory if we want to keep using the readabs package.

# Real GDP, and expenditure components (C, I, G, X, M) - Quarterly 5206.0 (5204.0 for annual "finalized" measures)
# Nominal GDP, and expenditure components (C, I, G, X, M) - Quarterly 5206.0
# Hours worked, employment, participation - Monthly 6202.0 (detailed data in 6291.0.55.001 and 6291.0.55.003)
# Average weekly earnings - Six monthly 6302.0
# Average wage
# WPI - Quarterly 6345.0 https://www.abs.gov.au/websitedbs/D3310114.nsf/home/Wage+Price+Indexes+FAQs
# Payroll numbers and wages - Monthly (for a week of data, based on payroll software) 6160.0.55.001
# CPI - 6401.0

wpi <- read_abs("6345.0")
head(wpi)
colnames(wpi)
unique(wpi$frequency)

annualindexdata <- wpi[wpi$series_id == "A2704396F" & wpi$table_no == "634502a",]

indexdata <- wpi[wpi$frequency == "Quarter" & wpi$series_id == "A2603609J" & wpi$table_no == "634501",] # Quarterly all sectors WPI 

indexdata2 <- indexdata %>% mutate(logchange = c(NA,diff(log(value)))) %>% mutate(annlogchange = c(NA,NA,NA,NA,diff(log(value),lag=4)))

ggplot(indexdata2, aes(x=date, y=annlogchange,colour = "Annual")) + geom_line(size = 1.5) + geom_line(aes(x=date, y=(logchange+1)^4-1, colour="Quarterly"))

# Quarterly issue could be due to seasonal factors. So lets do the same for the SA figures

indexdataSA <- wpi[wpi$frequency == "Quarter" & wpi$series_id == "A2713849C" & wpi$table_no == "634501",]

indexdataSA2 <- indexdataSA %>% mutate(logchangeSA = c(NA,diff(log(value)))) %>% mutate(annlogchangeSA = c(NA,NA,NA,NA,diff(log(value),lag=4)))

ggplot(indexdataSA2, aes(x=date, y=annlogchangeSA,colour = "Annual")) + geom_line(size = 1.5) + geom_line(aes(x=date, y=(logchangeSA+1)^4-1, colour="Quarterly"))

# Now annual SA measures are a bit imprecise, so we want to use the annual on the original data, and the quarterly annualised on the SA data.

indexdataboth <- cbind(indexdata2,indexdataSA2[,13:14])

ggplot(indexdataboth, aes(x=date, y=annlogchange,colour = "Annual")) + geom_line(size = 1.5) + geom_line(aes(x=date, y=(logchangeSA+1)^4-1, colour="Quarterly"))

# Now bring in CPI as a comparison

cpi <- read_abs("6401.0")

CPIprice <- cpi[cpi$table_no == "640101" & cpi$series_id == "A2325846C",] %>% filter(date >= as.Date("1997-09-01"))
colnames(CPIprice) <- paste("CPI", colnames(CPIprice), sep = "_")

PI <- cbind(indexdataboth,CPIprice) 

Priceindex <- PI %>% mutate(CPIannlogchange = c(NA,NA,NA,NA,diff(log(CPI_value),lag=4)))

ggplot(Priceindex, aes(x=date, y=annlogchange,colour = "Annual WPI")) + geom_line(size = 1.5) + geom_line(aes(x=date, y=CPIannlogchange, colour="Annual CPI"))

# GDP figures now

GDP <- read_abs("5206.0")

GDPactual <- GDP[GDP$table_no == "5206001_key_aggregates" & GDP$series_id == "A2302459A",] %>% mutate(GDPannlogchange = c(NA,NA,NA,NA,diff(log(value),lag=4))) %>% filter(date >= as.Date("1995-03-01"))

GDPSA <- GDP[GDP$table_no == "5206001_key_aggregates" & GDP$series_id == "A2304402X",]  %>% mutate(GDPlogchange = c(NA,diff(log(value)))) %>% filter(date >= as.Date("1995-03-01"))

ggplot(GDPSA, aes(x=date, y=value)) + geom_line(size = 1.5) + geom_smooth(aes(colour="Trendline"),se=FALSE,linetype = "dashed")

GDPdata <- cbind(GDPactual,GDPSA[,ncol(GDPSA)])

ggplot(GDPdata, aes(x=date, y=GDPannlogchange, colour = "Annual change")) + geom_line(size = 1.5) #+ geom_col(aes(x=date, y=(GDPlogchange+1)^4-1, colour="Quarterly change (ann)"),size=0.5)

# To think about longer term want to look at annual values and trends

library(RcppRoll)



