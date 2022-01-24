# Construct a simple outline of key macroeconomic variables in New Zealand, and simple time series models.

rm(list=ls()) # Clear prior memory

library(tidyverse)
library(readxl)
library(cowplot)

setwd("c:/Data")

# Import the following datasets as at 29/12/2021 (note series names will change as new data is released):

### For the TVHE video on GDP and income statistics - pull some base GDP figures for discussing in the video.

# The base GDP, real and nominal, as well as the consumption of fixed capital figures are sourced directly from INFOSHARE.

years <- seq(2000,2020,1)

GDPnom <- read.csv("GDPE nominal.csv",header=TRUE)
GDPnom <- GDPnom[GDPnom$Year %in% years,]

GDPreal <- read.csv("GDPE real.csv",header=TRUE)
GDPreal <- GDPreal[GDPreal$Year %in% years,]

CFKnom <- read.csv("CFK nominal.csv",header=TRUE)
CFKnom <- CFKnom[CFKnom$Year %in% years,]

Pop <- read.csv("Population.csv",header=TRUE)
Pop <- Pop[Pop$Year %in% years,]

GDPnpc <- (GDPnom[,2]/Pop[,2])*1000000
GDPrpc <- (GDPreal[,2]/Pop[,2])*1000000

deflator <- GDPnom[,2]/GDPreal[,2]

NGDPnom <- GDPnom[,2] - CFKnom[,2]

NGDPreal <- NGDPnom/deflator

NGDPrpc <- (NGDPreal/Pop[,2])*1000000

GDPdata1 <- cbind(GDPnom,GDPreal[,2],GDPnpc,GDPrpc,NGDPnom,NGDPreal,NGDPrpc)
colnames(GDPdata1) <- c("Year","GDPnom","GDPreal","GDPnpc","GDPrpc","NGDPnom","NGDPreal","NGDPrpc")

g1 <- ggplot(GDPdata1, aes(x=Year, y=GDPnom/1000,colour="Nominal GDP")) + geom_line(size =1) + geom_line(aes(x=Year,y=GDPreal/1000,colour="Real GDP"),size=2) + theme(legend.position = c(0.5, 0.8))

g1 <- g1 + ggtitle("Current and chain linked GDP") + labs(y="NZ Dollars ($bn)", x = "Year") + scale_y_continuous(labels=scales::dollar_format(),limits=c(100,400))

g1

########

g2 <- ggplot(GDPdata1, aes(x=Year, y=GDPnpc,colour="Nominal GDP PC")) + geom_line(size =1) + geom_line(aes(x=Year,y=GDPrpc,colour="Real GDP PC"),size=2) + theme(legend.position = c(0.5, 0.8))

g2 <- g2 + ggtitle("Current and chain linked GDP, per capita") + labs(y="NZ Dollars", x = "Year") + scale_y_continuous(labels=scales::dollar_format(),limits=c(20000,80000))

g2

plot_grid(g1,g2,labels="AUTO")

png(file="C:/Data/g2.png",
    width=600, height=350)
plot_grid(g1,g2,labels="AUTO")
dev.off()

GDPdata1$GDPrpc[nrow(GDPdata1)]/GDPdata1$GDPrpc[1]-1

##########

g3 <- ggplot(GDPdata1, aes(x=Year, y=GDPrpc,colour="Real GDP PC")) + geom_line(size =1) + geom_line(aes(x=Year,y=NGDPrpc,colour="Real NDP PC"),size=2)

g3 <- g3 + ggtitle("Chain linked GDP and NDP, per capita") + labs(y="NZ Dollars", x = "Year") + scale_y_continuous(labels=scales::dollar_format())

g3

png(file="C:/Data/g3.png",
    width=600, height=350)
g3
dev.off()

GDPdata1$NGDPrpc[nrow(GDPdata1)]/GDPdata1$NGDPrpc[1]-1


#####

# For the latest csv's go here https://www.stats.govt.nz/large-datasets/csv-files-for-download/

# These are the nominal income account statistics - as a result we will have to append the consumption of fixed capital figures to this to generate net values.

#url <- "https://www.stats.govt.nz/assets/Uploads/National-accounts-income-and-expenditure/National-accounts-income-and-expenditure-Year-ended-March-2021/Download-data/national-accounts-income-and-expenditure-year-ended-march-2021-csv.zip"

#download.file(url,"data.zip")
#unzip("data.zip")
#NatAcdata <- read.csv("na-nov2021-gdp-breakdown-csv.csv")
#NatAcdataPC <- read.csv("na-nov2021-percapita-csv.csv")

#colnames(NatAcdata)
#unique(NatAcdata$Series_title_1)

#GDPdata <- NatAcdata[NatAcdata$Series_title_1 == "Gross domestic product - income measure",]

#GNIdata <- NatAcdata[NatAcdata$Series_title_1 == "National Income - gross",]

#GNDIdata <- NatAcdata[NatAcdata$Series_title_1 =="Disposable Income - gross",]

# Shorter experimental consolidated accounts

NatAcdata <- read.csv("https://www.stats.govt.nz/assets/Uploads/Experimental/National-accounts-income-savings-assets-and-liabilities-June-2021-quarter/Download-data/National-accounts-income-saving-assets-and-liabilities-June-2021-quarter-consolidated-accounts-csv.csv")

colnames(NatAcdata)
unique(NatAcdata$Transaction_Label)

GDP2 <- NatAcdata[NatAcdata$Transaction_Label == "Gross domestic product - income measure" & NatAcdata$seasonality == "S",]

GNI <- NatAcdata[NatAcdata$Transaction_Label == "Gross natioal income" & NatAcdata$seasonality == "S",]

GNDI <- NatAcdata[NatAcdata$Transaction_Label == "Gross national disposable income" & NatAcdata$seasonality == "S",]

NDI <- NatAcdata[NatAcdata$Transaction_Label == "National disposable income" & NatAcdata$seasonality == "S",]


GDPdata2 <- as.data.frame(cbind(GDP2$period,GDP2$value,GNI$value,GNDI$value,NDI$value))
colnames(GDPdata2) <- c("Period","GDP","GNI","GNDI","NDI")

library(ggplot2)
library(reshape2)

graphdata <- GDPdata2 %>% melt(id.var="Period") %>% arrange(Period, variable)

g4 <- ggplot(graphdata, aes(Period,value)) + geom_line(aes(colour = variable))

g4 <- g4 + ggtitle("Nominal income measures (experimental)") + labs(y="NZ Dollars", x = "Year") + scale_y_continuous(labels=scales::dollar_format())

g4

png(file="C:/Data/g4.png",
    width=600, height=350)
g4
dev.off()

# Longer term real per capita from INFOSHARE

GDPdata3 <- read.csv("GDPandGNDI.csv")
colnames(GDPdata3) <- c("Year","GDP","GNDI")


graphdata2 <- GDPdata3 %>% melt(id.var="Year") %>% arrange(Year, variable)

g5 <- ggplot(graphdata2, aes(Year,value)) + geom_line(aes(colour = variable))

g5 <- g5 + ggtitle("Real Per Capita income measures") + labs(y="NZ Dollars", x = "Year") + scale_y_continuous(labels=scales::dollar_format())

g5

png(file="C:/Data/g5.png",
    width=600, height=350)
g5
dev.off()

GDPdata3$GDP[nrow(GDPdata3)]/GDPdata3$GDP[1] - 1

GDPdata3$GNDI[nrow(GDPdata3)]/GDPdata3$GNDI[1] - 1 





# Pull the following variables
# Real GDP, and expenditure components (C, I, G, X, M)
# Nominal GDP, and expenditure components (C, I, G, X, M)
# Hours worked
