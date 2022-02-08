# Price data for class (do second file comparing to other price indices)
# Data collected on 6th of February 2022. Indicies without a Dec 2021 figure used Sep 2021.

rm(list=ls()) # Clear prior memory

library(tidyverse)
library(readxl)
library(cowplot)
library(plotly)
library(reshape2)
library(ggpubr)
library(readxl)
library(viridis)
#library(gganimate)

#setwd("C:\Users\OEM\Documents\GitHub\TVHENZ\NZ CPI")

url <- "https://www.stats.govt.nz/assets/Uploads/Consumers-price-index/Consumers-price-index-December-2021-quarter/Download-data/consumers-price-index-december-2021-index-numbers.csv"

CPIdata <- read.csv(url) %>% mutate(date = as.numeric(substr(Period,1,4))) 

# Haircut info

#dates <- c(2006.12,2007.12,2008.12,2009.12,2010.12,2011.12,2012.12,2013.12,2014.12,2015.12,2016.12,2017.12,2018.12,2019.12,2020.12,2021.12)

#CPI0621 <- CPIdata %>% filter(Period %in% dates)

#groups <- c("All groups", "Hairdressing and personal grooming services")

#CPIvid <- CPI0621 %>% filter(Series_title_1 %in% groups) 

#CPIvid <- CPIvid %>% mutate(deflator = c(rep(CPIvid[CPIvid$Period == 2006.12 & CPIvid$Series_title_1 == "Hairdressing and personal grooming services",]$Data_value,length(dates)),rep(CPIvid[CPIvid$Period == 2006.12 & CPIvid$Series_title_1 == "All groups",]$Data_value,length(dates))))

#CPIvid <- CPIvid %>% mutate(Value = Data_value/deflator*1000)
#colnames(CPIvid)[colnames(CPIvid) == "Series_title_1"] <- "Consumer_product"

#ggplot(CPIvid,aes(x=Period,y=Value,group=Consumer_product,colour=Consumer_product)) + geom_line() + ggtitle("CPI measures") + labs(y="Index value (1000 in 2016)", x = "Year")

### CPI vid
## Base CPI plus trends

CPIdat <- CPIdata %>% filter(Series_title_1 == "All groups") %>% filter(as.numeric(substr(Period,6,7)) == 12) %>% filter(Period > 1995.12) 

CPItotal <- CPIdat %>% mutate(Twoperline = cumprod(c(CPIdat[CPIdat$date == "1996",]$Data_value,rep(1.02,25))))

g1 <- ggplotly(ggplot(CPItotal,aes(date,Data_value,colour="CPI data")) + geom_line(size = 1.5) + geom_smooth(aes(colour="Trendline"),se=FALSE,linetype = "dashed") + geom_line(aes(date,Twoperline,colour="2% since 1995"),size=1.1) + ggtitle("CPI vs trend") + labs(y="Index value", x = "Year") + theme_minimal())
g1

## CPI comparisons

CPIcomp <- CPIdata %>% filter(Series_title_1 == "All groups") %>% filter(as.numeric(substr(Period,6,7)) == 12) %>% filter(Period >= 1975.12) %>% mutate(episode = ifelse(Period <= 1984.12,1,ifelse(Period >= 2009.12 & Period <= 2018.12,2,ifelse(Period >= 2019.12,3,0)))) %>% mutate(episodename = ifelse(episode == 1,"1975-1984",ifelse(episode == 2, "2009-2018", ifelse(episode == 3, "2019-Now","")))) %>% mutate(index = ifelse(episode > 0,1,0)) %>% filter(index > 0) %>% mutate(YearInx = as.character(c(rep(seq(0,9,1),2),0,1,2))) %>% mutate(Value = ifelse(episode == 1, Data_value/Data_value[1],ifelse(episode == 2, Data_value/Data_value[11],ifelse(episode == 3, Data_value/Data_value[21],0)))) %>% mutate(Twoper = c(rep(cumprod(c(1,rep(1.02,9))),2),cumprod(c(1,rep(1.02,2)))))

g2 <- ggplotly(ggplot(CPIcomp,aes(YearInx,Value,colour=episodename,group=episode)) + geom_line(size = 1.5) + ggtitle("Comparing price shocks") + labs(y="Index value", x = "Year's from shock") + theme_minimal() + scale_y_continuous(
  labels = scales::number_format(accuracy = 0.01)))
g2

g2a <- ggplotly(ggplot(CPIcomp[11:23,],aes(YearInx,Value,colour=episodename,group=episode)) + geom_line(size = 1.5) + geom_line(aes(YearInx,Twoper,colour="2% growth"),size=1.1) + ggtitle("Comparing price shocks") + labs(y="Index value", x = "Year's from shock") + theme_minimal() + scale_y_continuous(
  labels = scales::number_format(accuracy = 0.01)))
g2a

## Broad measures

# Have to import the non-standard measures manually - a copy will be saved in the GitHub folder.
# Table: CPI Non-standard Trimmed Means and Weighted Percentiles for New Zealand (Qrtly-Mar/Jun/Sep/Dec) - note, the annuals have changing weights so rely on taking an index of the quarterly to back out annual.

TrimweightQ <- read.csv(file="TrimweightQ2.csv",head=TRUE)[2:85,] 
colnames(TrimweightQ) <- c("Period","Trim5","Trim10","Trim10T","Trim10NT","Trim15","Trim20","Trim25","Trim30","QW10th","QW25th","QW50th","QW50thT","QW50thNT","QW75th","QW90th")
TrimweightQ <- TrimweightQ %>% mutate(Year = as.numeric(substr(Period,1,4)), Quarter = as.numeric(substr(Period,6,6)))

Trimweight <- TrimweightQ %>% transmute(Period = Period, Quarter = Quarter, Trim10index = cumprod(1+as.numeric(TrimweightQ$Trim10)/100),Trim30index = cumprod(1+as.numeric(TrimweightQ$Trim30)/100),WMedindex = cumprod(1+as.numeric(TrimweightQ$QW50th)/100)) %>% filter(Quarter == 4) %>% select("Period","Trim10index","Trim30index","WMedindex") %>% melt(id.var="Period") %>% arrange(Period, variable) %>% group_by(variable) %>% mutate(growth = (value - lag(value))/lag(value))
Trimweight$value <- as.numeric(Trimweight$value)

# Have to import the non-standard measures manually - a copy will be saved in the GitHub folder.
# Table: CPI Non-standard All Groups Less/Plus Selected Groupings for New Zealand (Qrtly-Mar/Jun/Sep/Dec)

Exitems <- read.csv(file="Excategories.csv",head=TRUE)[1:134,]
colnames(Exitems) <- c("Period",Exitems[1,2:27])
Exitems<- Exitems %>% mutate(Year = as.numeric(substr(Period,1,4)), Quarter = as.numeric(substr(Period,6,6)))

Exinf <- Exitems[2:134,] 

Exinf <- Exinf %>% filter(Quarter == 4) %>% select("Period","All groups less food group, household energy subgroup, vehicle fuels, & government charges","All groups less rentals for housing subgroup") 
colnames(Exinf) <- c("Period","Ex food, energy, govt charges","Ex rent")

Exinf <- Exinf %>% melt(id.var="Period") %>% arrange(Period, variable) %>% group_by(variable) %>% mutate(growth = (as.numeric(value) - lag(as.numeric(value)))/lag(as.numeric(value)))
Exinf$value <- as.numeric(Exinf$value)


# RBNZ data for the sectoral models

RBNZprices <- read_excel("RBNZprices.xlsx",col_names = TRUE)[5:140,] %>% mutate(Year = rep(1988:2021, each=4)) %>% mutate(Quarter = rep(c("Q1","Q2","Q3","Q4"),34)) %>% unite('Period',c("Year","Quarter"),remove=FALSE,sep="")

RBNZinf <- RBNZprices %>% filter(Quarter == "Q4") %>% filter(Year >= 1995) %>% select("Period","Consumers price index (CPI)...4","Factor model","Sectoral factor model") 
colnames(RBNZinf) <- c("Period","CPI","Factor model","Sectoral factor model")

RBNZinf <- RBNZinf %>% melt(id.var="Period") %>% arrange(Period, variable) %>% group_by(variable) %>% mutate(growth = as.numeric(value)/100)
RBNZinf$value <- as.numeric(RBNZinf$value)


# Combine the series

Core <- rbind(Trimweight,Exinf,RBNZinf) %>% mutate(Year = as.numeric(substr(Period,1,4))) %>% filter(Year >= 2000)


g3 <- ggplotly(ggplot(Core,aes(Year,growth,colour=variable)) + geom_line(aes(size=variable)) + scale_size_manual(values = c(rep(0.25,5), 1,rep(0.25,2))) + scale_y_continuous(labels = scales::percent) + ggtitle("Core inflation measures") + labs(y="Annual growth", x = "Year") + theme_minimal() + theme(legend.position = "bottom")  + geom_hline(yintercept = 0.03, color = "red",linetype="dashed"))  # Different measures of "core" inflation!
g3

g3short <- ggplot(Core,aes(Year,growth,colour=variable)) + geom_line(aes(size=variable)) + scale_size_manual(values = c(rep(0.25,5), 1,rep(0.25,2))) + scale_y_continuous(labels = scales::percent) + ggtitle("Core inflation measures") + labs(y="Annual growth", x = "Year") + theme_minimal() + theme(legend.position = "bottom")  + geom_hline(yintercept = 0.03, color = "red",linetype="dashed")  # Different measures of "core" inflation!
g3short

# + scale_color_viridis(discrete = TRUE, option = "D") # Colourblind colours - preferable with fewer variables

url <- "https://www.stats.govt.nz/assets/Uploads/Consumers-price-index/Consumers-price-index-December-2021-quarter/Download-data/consumers-price-index-december-2021-tradables-and-nontradables.csv"

CPIdata2 <- read.csv(url) %>% mutate(date = as.numeric(substr(Period,1,4))) %>% filter(Period >= 1975.12) %>% filter(as.numeric(substr(Period,6,7)) == 12) %>% filter(Group %in% c("CPI All Groups for New Zealand","CPI Non-standard Tradable & Non-tradable Component Series","CPI Level 1 Groups Tradables and Non-tradables"))

unique(CPIdata2$Series_reference) # SE9 tells us it is New Zealand data. NS is then "non-standard" (is the aggregate index for both - 6000 is tradable, 6500 is non-tradable), TN is non-tradable, TT is tradable, and the number refers to the LVL1 category.

TNT <- CPIdata2 %>% filter(Series_reference %in% c("CPIQ.SE9NS6000","CPIQ.SE9NS6500")) 
TNT$Series_reference[TNT$Series_reference == "CPIQ.SE9NS6000"] <- "Tradable"
TNT$Series_reference[TNT$Series_reference == "CPIQ.SE9NS6500"] <- "Non-tradable"

g4 <- ggplotly(ggplot(TNT, aes(date,Data_value,colour=Series_reference)) + geom_line() + ggtitle("Tradable vs Non-tradable prices") + labs(y="Annual Index", x = "Year") + theme_minimal()) # Tradable v non-tradable
g4

Category <- c("Food","Alcohol and tobacco","Clothing and footwear","Housing utilities","Household contents","Health","Transport","Communication","Recreation","Education","Other")

Index <- seq(1,11,by=1)

Lvl1Cats <- cbind.data.frame(Category,Index)

TNT1 <- CPIdata2 %>% filter(!Series_reference %in% c("CPIQ.SE9A","CPIQ.SE9NS6000","CPIQ.SE9NS6500")) %>% group_by(Series_reference) %>% mutate(growth = (Data_value - lag(Data_value))/lag(Data_value)) %>% filter(Period >= 2015.12) %>% mutate(Series = as.numeric(substr(Series_reference,11,12))) %>% mutate(Period = as.numeric(substr(Period,1,4)))

TNT1$Series <- lapply(TNT1$Series,function(x) Lvl1Cats$Category[match(x,Index)])

g5 <- ggplotly(ggplot(TNT1,aes(Period,growth,colour=Series_title_1)) + geom_point()) # Level 1 groups for both compared
g5 # Skip this for now as I can't get it to print with the appropriate series, easy fix would just be to append NT and T onto the series names to make them unique :)

## Clustering of the CPI classes

dates <- c(2021.12)

CPIcat <- CPIdata %>% filter(Period %in% dates)

CPIcat1 <- unique(CPIcat[CPIcat$Group == "CPI Level 1 Groups for New Zealand",]$Series_title_1)

CPIcat3 <- unique(CPIcat[CPIcat$Group == "CPI Level 3 Classes for New Zealand",]$Series_title_1)

CPIL3 <- CPIdata %>% filter(Group == "CPI Level 3 Classes for New Zealand") %>% filter(as.numeric(substr(Period,6,7)) == 12) %>% filter(Period > 1998.12) 

CPIL3 <- CPIL3 %>% mutate(Category = ifelse(Series_title_1 %in% CPIcat3[1:18],"Food and drink",ifelse(Series_title_1 %in% CPIcat3[19:27],"Clothing and Footwear",ifelse(Series_title_1 %in% CPIcat3[28:38],"Housing utilities",ifelse(Series_title_1 %in% CPIcat3[39:49],"Housing durables and services",ifelse(Series_title_1 %in% CPIcat3[50:56],"Health",ifelse(Series_title_1 %in% CPIcat3[57:70],"Transport",ifelse(Series_title_1 %in% CPIcat3[71:76],"Communication",ifelse(Series_title_1 %in% CPIcat3[77:89],"Recreation",ifelse(Series_title_1 %in% CPIcat3[90:93],"Education",ifelse(Series_title_1 %in% CPIcat3[94:108],"Other","Missing"))))))))))) 

cats <- unique(CPIL3$Category)

g6a <- ggplotly(ggplot(CPIL3,aes(Period,Data_value,colour=Category,group=Series_title_1))+geom_point() + ggtitle("Level 3 prices (by Level 1 group)") + labs(y="Annual Index", x = "Year") + theme_minimal() + geom_hline(yintercept = 1000.00, color = "black")) #+ geom_smooth(aes(colour="Trendline"),se=FALSE,linetype = "dashed")
g6a

g6b <- ggplotly(ggplot(CPIL3[CPIL3$Category != "Communication",],aes(Period,Data_value,colour=Category,group=Series_title_1))+geom_point() + ggtitle("Level 3 prices (by Level 1 group)") + labs(y="Annual Index", x = "Year") + theme_minimal() + geom_smooth(aes(colour="Trendline"),se=FALSE,linetype = "dashed",size=0.25) + geom_hline(yintercept = 1000.00, color = "black"))
g6b

CPIgrowth <- CPIL3 %>% group_by(Series_title_1) %>% mutate(growth = (Data_value - lag(Data_value))/lag(Data_value)) %>% mutate(avg10growth = (lag(Data_value,n=2)/lag(Data_value,n=22))^(1/20)-1) # The 10 year growth rate is based on pre-COVID year!

g6 <- ggplotly(ggplot(CPIgrowth,aes(Period,growth,colour=Category,group=Series_title_1))+ geom_point() + scale_y_continuous(labels = scales::percent) + ggtitle("Level 3 price changes (by Level 1 group)") + labs(y="Annual growth", x = "Year") + theme_minimal() + theme(legend.position = "bottom") + geom_hline(yintercept = 0, color = "black"))

CPIgrowth2021 <- CPIgrowth %>% filter(date %in% c(2021)) 
CPIgrowth202021 <- CPIgrowth %>% filter(date %in% c(2020, 2021)) 

g7base <- ggplot(CPIgrowth202021,aes(Category,growth,colour=as.factor(date),group=Series_title_1))+ geom_point() + scale_y_continuous(labels = scales::percent) + ggtitle("Level 3 price changes (by Level 1 group)") + labs(y="Annual growth", x = "Category") + theme_minimal() + theme(legend.position = "bottom") + geom_hline(yintercept = 0, color = "black") + theme(axis.text.x = element_text(angle = 45)) 

g7 <- ggplotly(g7base)
g7

# Have a permissions issue I can't seem to fix - have the images for the animation in GitHub folder, but it won't take them to render.  Come back to this for a future video - only have a few hours to write this up.
#g7a <- g7base + transition_time(date) +
#  labs(title = "Year: {frame_time}") + ease_aes('linear')
#g7a

# + scale_x_discrete(guide = guide_axis(n.dodge=3))  

CPIgrowth202021unmelt <- dcast(CPIgrowth202021, Series_title_1 ~ Period,value.var = "growth") %>% mutate(Category = ifelse(Series_title_1 %in% CPIcat3[1:18],"Food and drink",ifelse(Series_title_1 %in% CPIcat3[19:27],"Clothing and Footwear",ifelse(Series_title_1 %in% CPIcat3[28:38],"Housing utilities",ifelse(Series_title_1 %in% CPIcat3[39:49],"Housing durables and services",ifelse(Series_title_1 %in% CPIcat3[50:56],"Health",ifelse(Series_title_1 %in% CPIcat3[57:70],"Transport",ifelse(Series_title_1 %in% CPIcat3[71:76],"Communication",ifelse(Series_title_1 %in% CPIcat3[77:89],"Recreation",ifelse(Series_title_1 %in% CPIcat3[90:93],"Education",ifelse(Series_title_1 %in% CPIcat3[94:108],"Other","Missing")))))))))))
colnames(CPIgrowth202021unmelt) <- c("Series_title_1","Y1","Y2","Category")

g8 <- ggplotly(ggplot(CPIgrowth202021unmelt,aes(Y1,Y2,colour=Category,group=Series_title_1))+ geom_point() + scale_y_continuous(labels = scales::percent) + ggtitle("Level 3 price changes (by Level 1 group)") + labs(y="Annual growth (2021)", x = "Annual growth (2020)") + theme_minimal() + theme(legend.position = "bottom") + geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = 0, color = "black") + geom_hline(yintercept = 0.03, color = "red",linetype="dashed") + geom_vline(xintercept = 0.03, color = "red",linetype="dashed")) 
g8

# Add comparing growth to 10 year average

CPIgrowth2021ten <- CPIgrowth %>% filter(date %in% c(2021))

g9 <- ggplotly(ggplot(CPIgrowth2021ten,aes(avg10growth,growth,colour=Category,group=Series_title_1))+ geom_point() + scale_y_continuous(labels = scales::percent) + ggtitle("Level 3 price changes (by Level 1 group)") + labs(y="Annual growth (2021)", x = "Avg annual growth (1999-2019)") + theme_minimal() + theme(legend.position = "bottom") + geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = 0, color = "black") + geom_abline(intercept=0,color="red",linetype="dashed")) 
g9

# Growth to contribution graph
# Contribution data has to be downloaded manually
# Table: Contributions to classes. Index, Percentage, and percentage point (Qrtly-Mar/Jun/Sep/Dec) is the table, data used will be included in GitHub

Contribute <- read.csv(file="Contributionyear.csv")
colnames(Contribute) <- c("Period",Contribute[1,2:ncol(Contribute)])
Contribute <- Contribute[3:64,] %>% filter(Period == "2021Q4")
Contribution <- rbind(Contribute,colnames(Contribute)) 

Contmatch <- as.data.frame(t(Contribution[2:ncol(Contribution)]))
colnames(Contmatch) <- c("value","Index")

Cont <- as.numeric(Contmatch$value[match(CPIgrowth2021ten$Series_title_1,Contmatch$Index)])

CPI2021twenty <- cbind(CPIgrowth2021ten,Cont) %>% drop_na() # Dropping those without 10 year growth rates, and without estimated contributions
colnames(CPI2021twenty)[13] <-"IndexContribution"

g10 <- ggplotly(ggplot(CPI2021twenty,aes(avg10growth,growth,colour=Category,group=Series_title_1))+ geom_point(aes(size=IndexContribution)) + scale_y_continuous(labels = scales::percent) + ggtitle("Level 3 price changes (by Level 1 group)") + labs(y="Annual growth (2021)", x = "Avg annual growth (1999-2019)") + theme_minimal() + theme(legend.position = "bottom") + geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = 0, color = "black") + geom_hline(yintercept = 0.03, color = "red",linetype="dashed") + geom_vline(xintercept = 0.03, color = "red",linetype="dashed"))  # Size is the 2021 contribution to the total change
g10

g10short <- ggplot(CPI2021twenty,aes(avg10growth,growth,colour=Category,group=Series_title_1))+ geom_point(aes(size=IndexContribution)) + scale_y_continuous(labels = scales::percent) + ggtitle("Level 3 price changes (by Level 1 group)") + labs(y="Annual growth (2021)", x = "Avg annual growth (1999-2019)") + theme_minimal() + geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = 0, color = "black") + geom_abline(intercept=0,color="red",linetype="dashed")   # Size is the 2021 contribution to the total change

g10short 

# Good discussion of colours here https://www.r-graph-gallery.com/ggplot2-color.html

## Sticky price measures
# Pre-defined category basis
# Paper is here https://www.atlantafed.org/-/media/documents/research/inflationproject/stickyprice/sticky-price-cpi-supplemental-reading.pdf
# Of the 101 remaining categories, the 8 items we are taking from that are:
# Actual rentals for housing
# Purchase of housing
# Recreational and sporting services
# Primary and secondary education
# Tertiary and other post school education
# Dwelling insurance
# Contents insurance
# Hairdressing and personal grooming services

Stickycat <- c("Actual rentals for housing","Purchase of housing"," Recreational and sporting services","Primary and secondary education","Tertiary and other post school education","Dwelling insurance","Contents insurance","Hairdressing and personal grooming services")

StickyCPI <- CPIL3 %>% filter(Series_title_1 %in% Stickycat) %>% mutate(growth = Data_value/lag(Data_value)-1) %>% filter(Period >= 2005.12)
colnames(StickyCPI)[8] <- "Sticky_categories"

g11 <- ggplotly(ggplot(StickyCPI, aes(Period, growth, colour=Sticky_categories)) + geom_line() + geom_hline(yintercept = 0.03, color = "red",linetype="dashed") + scale_y_continuous(labels = scales::percent) + ggtitle("Example sticky prices") + geom_hline(yintercept = 0, color = "black") + labs(y="Annual growth", x = "Year") + theme_minimal())
g11

CPIgrowthunmelt <- dcast(CPIgrowth, Period ~ Series_title_1,value.var = "growth")

# Volatility based
# Take the coefficient of variation for each growth rate.

sd <- unlist(lapply(CPIgrowthunmelt,sd,na.rm=TRUE))
mean <- unlist(lapply(CPIgrowthunmelt,mean,na.rm=TRUE))
COVname <- names(abs(sd/mean))
COV <- as.tibble(cbind(as.numeric(abs(sd/mean)),COVname,COVname,COVname))[2:length(COVname),] %>% arrange(V1)
COV$V3 <- lapply(COV$COVname,function(x) CPIgrowth2021$growth[match(x,CPIgrowth2021$Series_title_1)])
COV$V4 <- lapply(COV$COVname,function(x) CPIgrowth2021$Category[match(x,CPIgrowth2021$Series_title_1)])
colnames(COV) <- c("COV","Item","Growth","Category")
COV <- COV %>% mutate(Category = as.character(Category))

g12 <- ggplotly(ggplot(COV, aes(log(as.numeric(COV)),as.numeric(Growth),colour=Category)) + geom_point() + geom_hline(yintercept = 0.03, color = "red",linetype="dashed") + geom_hline(yintercept = 0.00, color = "black") + ggtitle("Price growth by COV") + scale_y_continuous(labels = scales::percent) + labs(y="Annual growth", x = "COV (in logs)") + theme_minimal()) # Measure of volatility to growth
g12

COVLR <- as.tibble(cbind(COV,COVname[2:length(COVname)]))
colnames(COVLR) <- c("COV","Item","Growth","Category","avgGrowth")
COVLR$avgGrowth <- lapply(COVLR$Item,function(x) CPIgrowth2021$avg10growth[match(x,CPIgrowth2021$Series_title_1)]) 
COVLR <- COVLR %>% mutate(growthdiff = as.numeric(Growth) - as.numeric(avgGrowth)) %>% drop_na

g13 <- ggplotly(ggplot(COVLR, aes(log(as.numeric(COV)),growthdiff,colour=Category)) + geom_point() + geom_hline(yintercept = 0.03, color = "red",linetype="dashed") + geom_hline(yintercept = 0.00, color = "black") + ggtitle("Price growth swing by COV") + scale_y_continuous(labels = scales::percent) + labs(y="Difference from average growth", x = "COV (in logs)") + theme_minimal()) # Growth in low volatility prices compared to 20 year average to 2019. 
g13

## Alternative measures

# Import LCI
# Is in zipped form so have to download and extract from https://www.stats.govt.nz/assets/Uploads/Labour-market-statistics/Labour-market-statistics-December-2021-quarter/Download-data/labour-market-statistics-december-2021-quarter-csv.zip 

LCIdata <- read.csv(file="lci-dec21qtr-csv.csv") %>% filter(Group %in% c("All Sectors Combined, All Salary and Wage Rates (Base: June 2017 qtr (=1000))","Analytical Unadjusted Index (Base: June 2009 quarter (=1000))")) %>% filter(Series_title_1 == "All Sectors Combined") %>% filter(as.numeric(substr(Period,6,7)) == 12) %>% mutate(Period = as.numeric(substr(Period,1,4))) %>% select("Period","Group","Data_value") %>% mutate(growth = Data_value/lag(Data_value)-1)

# %>% mutate(Period = paste(as.numeric(substr(Period,1,4)),"Q4",sep=""))

LCIdata$Group[LCIdata$Group == "All Sectors Combined, All Salary and Wage Rates (Base: June 2017 qtr (=1000))"] <- "LCI"
LCIdata$Group[LCIdata$Group == "Analytical Unadjusted Index (Base: June 2009 quarter (=1000))"] <- "Unadjusted LCI"
colnames(LCIdata) <- c("Period","variable","value","growth")

# Import CGPI
# In in zipped form, so have to download and extract from https://www.stats.govt.nz/assets/Uploads/Business-price-indexes/Business-price-indexes-September-2021-quarter/Download-data/business-price-indexes-september-2021-quarter-csv.zip

CGPIdata <- read.csv(file="business-price-indexes-september-2021-quarter-csv.csv") %>% filter(Series_title_1 == "All Groups") %>% filter(Series_title_2 == "Percentage change from same period previous year")  %>% filter(UNITS == "Percent") %>% filter(as.numeric(substr(Period,6,7)) == 12) %>% mutate(Period = as.numeric(substr(Period,1,4))) %>% select("Period","Data_value") 
colnames(CGPIdata) <- c("Period","CGPI")

CGPIdata <- CGPIdata %>% melt(id.var="Period") %>% arrange(Period, variable) %>% group_by(variable) %>% mutate(growth = as.numeric(value)/100) %>% mutate(Period = as.numeric(substr(Period,1,4)))
CGPIdata$value <- as.numeric(CGPIdata$value)

addCGPI <- read.csv(file="business-price-indexes-september-2021-quarter-csv.csv") %>% filter(Series_title_1 == "All Groups") %>% filter(Series_title_2 == "Percentage change from same period previous year")  %>% filter(UNITS == "Percent") %>% filter(as.numeric(substr(Period,6,7)) == 9) %>% filter(as.numeric(substr(Period,1,4)) == 2021) %>% transmute(Period = 2021,variable = "CGPI",value = Data_value,growth=as.numeric(Data_value)/100)

CGPIdata <- rbind(CGPIdata,addCGPI)

#Early RBNZ series has the PPI and GDP deflator data

RBNZinf2 <- RBNZprices %>% filter(Quarter == "Q4") %>% filter(Year >= 1995) %>% select("Period","Consumers price index (CPI)...4","GDP deflator...39","Producers price index (PPI) inputs...42","Producers price index (PPI) outputs...45") 
colnames(RBNZinf2) <- c("Period","CPI","GDP deflator","PPI inputs","PPI outputs")

AddRBNZinf2 <- RBNZprices %>% filter(Quarter == "Q3") %>% filter(Year == 2021) %>% select("Period","Consumers price index (CPI)...4","GDP deflator...39","Producers price index (PPI) inputs...42","Producers price index (PPI) outputs...45") 
colnames(AddRBNZinf2) <- c("Period","CPI","GDP deflator","PPI inputs","PPI outputs")

RBNZinf2 <- RBNZinf2 %>% melt(id.var="Period") %>% arrange(Period, variable) %>% group_by(variable) %>% mutate(growth = as.numeric(value)/100) %>% mutate(Period = as.numeric(substr(Period,1,4)))
RBNZinf2$value <- as.numeric(RBNZinf2$value)

# Don't have Dec numbers for GDP deflator, or the PPIs, so pop in September.

RBNZinf2$growth[106] <- as.numeric(AddRBNZinf2$`GDP deflator`[1])/100
RBNZinf2$growth[107] <- as.numeric(AddRBNZinf2$`PPI inputs`[1])/100
RBNZinf2$growth[108] <- as.numeric(AddRBNZinf2$`PPI outputs`[1])/100

# Survey of expectations data available here https://www.rbnz.govt.nz/statistics/m14

# Combine the series

Altinf <- rbind(LCIdata,CGPIdata,RBNZinf2) %>% filter(Period >= 2000)

g14 <- ggplotly(ggplot(Altinf,aes(Period,as.numeric(growth),colour=variable)) + geom_line(aes(size=variable)) + scale_size_manual(values = c(rep(0.25,1), 1,rep(0.25,5))) + scale_y_continuous(labels = scales::percent) + ggtitle("Other inflation measures") + labs(y="Annual growth", x = "Year") + theme_minimal() + theme(legend.position = "right")  + geom_hline(yintercept = 0.03, color = "red",linetype="dashed") + geom_hline(yintercept = 0.00, color = "black"))
g14

g14short <- ggplot(Altinf,aes(Period,as.numeric(growth),colour=variable)) + geom_line(aes(size=variable)) + scale_size_manual(values = c(rep(0.25,1), 1,rep(0.25,5))) + scale_y_continuous(labels = scales::percent) + ggtitle("Other inflation measures") + labs(y="Annual growth", x = "Year") + theme_minimal() + theme(legend.position = "right")  + geom_hline(yintercept = 0.03, color = "red",linetype="dashed") + geom_hline(yintercept = 0.00, color = "black")
g14

### Vid list

I1<-g1 # CPI measure since 1995, with a 2% trend.

I2<-g2 # Comparison of CPI growth over three periods 197X, 2009, 2019

I3<-g2a # Remove 1975

I4<-g3 # CPI, trimmed mean, etc

I5<-g4 # Non-tradable, tradable

I6<-g6a # Categories through time

I7<-g6b # Categories Without communication

I8<-g6 # Price growth by category by year from 2005.

I9<-g7 # Price growth across categories, comparing 2020 and 2021 range

I10<-g8 # Comparing 2020 and 2021 growth in categories

I11<-g9 # Comparing growth, looking for the large contributors to the total

I12<-g10 # Pandemic average vs pre-pandemic average by category

I13<-g11 # Sticky price items growth, 20 years.

I14<-g12 # 2020, 2021, and joint 2020-21 growth vs 20 variance in price growth per item.

I15<-g13 # 2020, 2021, and joint 2020-21 growth vs average price change of item in last twenty years (low variance items)

I16<-g14 # Comparison to other inflation measures

#png(file="short1.png",
#    width=1800, height=1050)
#g3short
#dev.off()

#png(file="short2.png",
#    width=1800, height=1050)
#g10short
#dev.off()

#png(file="short3.png",
#    width=1800, height=1050)
#g14short
#dev.off()

ggsave("S1.png", width = 20, height = 10, units = 'cm')
g3short
dev.off()

ggsave("S2.png", width = 25, height = 10, units = 'cm')
g10short +
  annotate("text", x = 0.055, y = 0.3, label = "Petrol",size=3.5,colour="deeppink") +
  annotate("text", x = -0.04, y = 0.68, label = "International air travel", size = 3.5,colour="deeppink") +
  annotate("text", x = 0.01, y = 0.18, label = "Purchase of housing", size = 3.5,colour="deepskyblue3")
dev.off()

ggsave("S3.png", width = 25, height = 10, units = 'cm')
g14short
dev.off()

