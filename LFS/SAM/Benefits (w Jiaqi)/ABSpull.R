# Generating a single process to pull the data and then calculate the relevant summary stats.
# Data generated to largely match process of Fujita and Ramey 2012, but for Australia.
# NOT adjusted for margin error and time aggregation error - that will be an possible extension.

###### For discussion on Monday:
# 1) Seasonally adjusted vs actuals - which to estimate from (literature quite unclear, have found situations where SA is used on the same method but sounds like actuals used in given paper. However, ACs suggest SA) [use seasonal]
# 2) HP filter component - purely cyclical (what is applied) or cyclical + trend. [happy]
# 3) Scale of the elasticity (multiply by 100) [happy]
# 4) Whether the signs seem reasonable (vacancies look weird in AR terms, so maybe used ABS values) [The U and V relationship with LP looks funny - decided need to try to SA figure from ABS instead of our own adjustment.] [also try looking at different hp filters, and a longer time period.]
# 5) Thoughts on measuring "vacancy posting cost"

rm(list=ls())

.libPaths(new = 'C:/Rpackage')

library(tidyverse)
library(data.table)
library(collapse)
library(readabs)
library(readr)
library(readxl)
library(theme61)
library(lubridate)
library(mFilter)
library(zoo)

# First download the series needed. labour_force,	unemployed,	unemp_rate,	GDP,	UE,	EU,	E,	U,	HRS,	HWI
# LP, unemployment rate, job finding rate, UE, EU,v, and v/u

## Get LP and UR

Hours <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Original") %>% filter(series == "Hours worked: Index ;") %>% filter(grepl('Table 1',table_title)) %>% drop_na %>% filter(unit == "Index Numbers") %>% select("date","value")

colnames(Hours) <- c("date","Hours")

GDP <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Original") %>% filter(series == "Gross domestic product: Chain volume measures ;") %>% filter(table_title == "Table 1. Key National Accounts Aggregates") %>% select("date","series","value")

LS <- read_abs(cat_no = "6202.0") %>% filter(series_type == "Original") %>% filter(series == "Employed total ;  Persons ;" | series == "Unemployed total ;  Persons ;"  | series == "Unemployment rate ;  Persons ;") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original") %>% select("date","series","value")

LS2 <- pivot_wider(LS,id_cols = "date",names_from = "series",values_from = "value") 
colnames(LS2) <- c("date","Employed","Unemployed","UR_ABS")

LS2 <- LS2 %>% mutate(UR_Derived = Unemployed/(Employed + Unemployed),QUnemp = rollmean(Unemployed,k=3,fill=NA,align="right"),QEmp =rollmean(Employed,k=3,fill=NA,align="right"),UR = QUnemp/(QEmp+QUnemp))

LSQ <- LS2[seq(5,nrow(LS2),3),] %>% select(date,QUnemp,QEmp,UR)

# LP <- left_join(LSQ,GDP,by="date") %>% mutate(Labour_Prod = value/QEmp) %>% select(date,Labour_Prod) %>% drop_na

LP <- left_join(Hours,GDP,by="date" )%>% mutate(Labour_Prod = value/Hours) %>% select(date,Labour_Prod) %>% drop_na

UR <- LSQ %>% select(date,UR) 

# The sensitivity series
 
# LPhour <- read_abs(cat_no = "5206.0") %>% filter(series == "GDP per hour worked: Index ;") %>% filter(series_type == "Original") %>% filter(!is.na(value)) %>% select("date","series","value") # This isn't how Fijita and Ramey define it, as they go by worker.

## Get UE and EU and JFR and SR [Note, they make simple quarterly averages of monthly data - rather than calculating quarters, so we use that as our core number]

dfgf <- read_lfs_grossflows(weights = "previous") %>% group_by(date,lfs_current,lfs_previous) %>% summarise(number = sum(persons))

EmpTrans <- dfgf %>% filter((lfs_current == "Unemployed") & (lfs_previous == "Employed full-time" | lfs_previous == "Employed part-time")) %>% group_by(date) %>% summarise(URin = sum(number))

UEm <- data.frame(date = EmpTrans$date, UE = EmpTrans$URin) %>% mutate(UEQ =rollmean(UE,k=3,fill=NA,align="right"))

UE <- UEm[seq(3,nrow(UEm),3),] %>% select(date,UEQ)

UnTrans <- dfgf %>% filter((lfs_current == "Employed full-time" | lfs_current == "Employed part-time") & (lfs_previous == "Unemployed")) %>% group_by(date) %>% summarise(URout = sum(number)) # lfs_previous == "Not in the labour force (NILF)") & - NILF excluded

EUm <- data.frame(date = UnTrans$date, EU = UnTrans$URout) %>% mutate(EUQ =rollmean(EU,k=3,fill=NA,align="right"))

EU <- EUm[seq(3,nrow(UEm),3),] %>% select(date,EUQ)

pEmployed <- dfgf %>% filter((lfs_previous == "Employed full-time" | lfs_previous == "Employed part-time")) %>% group_by(date) %>% summarise(number = sum(number))

Employed <- dfgf %>% filter((lfs_current == "Employed full-time" | lfs_current == "Employed part-time")) %>% group_by(date) %>% summarise(number = sum(number))

JFRm <- data.frame(date = pEmployed$date,JFR = EmpTrans$URin/pEmployed$number) %>% mutate(JFRQ =rollmean(JFR,k=3,fill=NA,align="right"))

SRm <- data.frame(date = pEmployed$date,SR = UnTrans$URout/pEmployed$number) %>% mutate(SRQ =rollmean(SR,k=3,fill=NA,align="right"))

JFR <- JFRm[seq(3,nrow(JFRm),3),] %>% select(date,JFRQ)

SR <- SRm[seq(3,nrow(SRm),3),] %>% select(date,SRQ)

# Sensitivity
# Actual quarterly rate

# pEmployedQ <- pEmployed %>% transmute(date=date,Emp = rollmean(number,k=3,fill=NA,align="right"))
# 
# pEmployedQ <- pEmployedQ[seq(3,nrow(pEmployedQ),3),]
# 
# EmpTransQ <- EmpTrans %>% transmute(date=date,EmpTrans = rollmean(URin,k=3,fill=NA,align="right"))
# 
# EmpTransQ <- EmpTransQ[seq(3,nrow(EmpTransQ),3),]
# 
# UnTransQ <- UnTrans %>% transmute(date=date,UnTrans = rollmean(URout,k=3,fill=NA,align="right"))
# 
# UnTransQ <- UnTransQ[seq(3,nrow(UnTransQ),3),]
# 
# JFRQ <- data.frame(date = pEmployed$date,JFR = EmpTransQ$EmpTrans/pEmployedQ$Emp)
# 
# SRQ <- data.frame(date = pEmployed$date,SR = UnTransQ$UnTrans/pEmployedQ$Emp)


### Get v and v/u

## Use the national skills commission data
# 
# library(rvest)
# library(rio)
# 
# url <- "https://www.nationalskillscommission.gov.au/topics/internet-vacancy-index"
# page <- xml2::read_html(url)
# xlsx_urls <- html_nodes(page, "a") %>%
#   html_attr("href") %>%
#   grep("\\.xlsx$", ., value = TRUE)
# 
# urls <- paste("https://www.nationalskillscommission.gov.au/",xlsx_urls[2],sep="")
# 
# InternetVacancy <- rio::import(urls,which="Seasonally Adjusted")
# 
# ivdates <- seq.Date(as.Date("2006-01-01"),by="month",length.out = (ncol(InternetVacancy)-4))
# 
# IV <- tibble(t(InternetVacancy[1,5:ncol(InternetVacancy)]))
# 
# vac <- tibble(ivdates,IV)
# colnames(vac) <- c("date","vacancy")
# 
# vu <- left_join(vac,LS2, by="date") %>% mutate(vacancyQ =rollmean(vacancy,k=3,fill=NA,align="right"), UnemployedQ=rollmean(Unemployed,k=3,fill=NA,align="right"))
# 
# vu <- vu[seq(3,nrow(vu),by=3),]
# 
# v <- vu %>% transmute(date=date,v=vacancyQ)
# 
# vu <- vu %>% transmute(date=date,vu = vacancyQ/(UnemployedQ*1000))
# 
# plot(vu)

## Use the ABS data - this is quarterly

vac <- read_abs(cat_no = "6354.0") %>% filter(series == "Job Vacancies ;  Australia ;") %>% filter(grepl('TABLE 1',table_title)) %>% filter(series_type == "Original")

vac$date <- vac$date %m+% months(1) -14

vac <- vac %>% select(date,value)
colnames(vac) <- c("date","vacancy")

# Need to impute the data that was not collected for August 2008 to August 2009 based on a regression of the log difference in vacancies against the change in the unemployment rate from pre-May 2008. Note: Probably better to just use a package than experiment https://cran.r-project.org/web/packages/imputeTS/vignettes/imputeTS-Time-Series-Missing-Value-Imputation-in-R.pdf

library(imputeTS)

a <- na_seadec(vac, algorithm = "interpolation") # Issue is that this doesn't make use of variables we know are correlated with this to do the imputation!

# ALternatives: https://datascienceplus.com/imputing-missing-data-with-r-mice-package/ , https://cran.r-project.org/web/packages/naniar/vignettes/exploring-imputed-values.html

library(Hmisc)

vacdf <- left_join(vac,LSQ,by="date") %>% select(date,vacancy,QUnemp) %>% mutate(time = seq(1,nrow(vac),by=1))

vac_imp <- aregImpute(~vacancy + QUnemp + time,data=vacdf %>% filter(date <= "2020-01-01"))

vacdf[is.na(vacdf$vacancy),]$vacancy <- vac_imp$imputed$vacancy[nrow(vac_imp$imputed$vacancy),]

plot(vacdf$vacancy)
plot(vacdf$QUnemp)

vu <- left_join(vacdf,LSQ, by="date")

v <- vu %>% transmute(date=date,v=vacancy)

vu <- vu %>% transmute(date=date,vu = vacancy/(QUnemp.x*1000))

plot(vu)

# Hmisc doesn't take into account time series properties, so try mice.
# Although another way of looking at it is that a simple time model shows little variation - while using unemployment leads to a collapse in vacancies because "unemployment" numbers themselves have stayed at a similar level through time.  As a result, the issue stems from v/u ratios rising over time, and hence this time trend in v being missed - just adding the time trend appears to clean that up!
# 
# vacdf <- left_join(vac,LSQ,by="date") %>% select(date,vacancy,QUnemp)
# 
# library(mice)



# Sensitivity

# LM <- read_abs(cat_no = "6150.0.55.003")


### The core figures now need to be logged and then HP filtered, prior to the summary statistics being constructed.
# LP, unemployment rate, job finding rate, separation rate, UE, EU,v, and v/u
# Check whether the value desired is cycle or cycle + trend - impression from literature is that these are models that are fitting the cyclical relationships.



# HPfilter number

hpf <- 10000


LP2 <- LP %>% mutate(lLP = log(Labour_Prod)) %>% drop_na %>% mutate(lLP_HP = hpfilter(lLP,freq=hpf,type="lambda")$cycle)

# LP2 <- LP %>% mutate(lLP = log(Labour_Prod)) %>% drop_na
# 
# lLP_HP = as.vector(hpfilter(LP2$lLP,freq=hpf,type="lambda"))

UR2 <- UR %>% mutate(lUR = log(UR)) %>% drop_na %>% mutate(lUR_HP = hpfilter(lUR,freq=hpf,type="lambda")$cycle)

JFR2 <- JFR %>% mutate(lJFR = log(JFRQ)) %>% drop_na %>% mutate(lJFR_HP = hpfilter(lJFR,freq=hpf,type="lambda")$cycle)

SR2 <- SR %>% mutate(lSR = log(SRQ)) %>% drop_na %>% mutate(lSR_HP = hpfilter(lSR,freq=hpf,type="lambda")$cycle)

UE2 <- UE %>% mutate(lUE = log(UEQ)) %>% drop_na %>% mutate(lUE_HP = hpfilter(lUE,freq=hpf,type="lambda")$cycle)

EU2 <- EU %>% mutate(lEU = log(EUQ)) %>% drop_na %>% mutate(lEU_HP = hpfilter(lEU,freq=hpf,type="lambda")$cycle)

v2 <- v %>%  mutate(lv = log(v)) %>% drop_na %>% mutate(lv_HP = hpfilter(lv,freq=hpf,type="lambda")$cycle)

vu2 <- vu %>% mutate(lvu = log(vu)) %>% drop_na %>% mutate(lvu_HP = hpfilter(lvu,freq=hpf,type="lambda")$cycle)

df <- left_join(UR2,JFR2,by="date",fill=NA) %>% select(date,contains("_HP"))
df <- left_join(df,SR2,by="date",fill=NA) %>% select(date,contains("_HP"))
df <- left_join(df,UE2,by="date",fill=NA) %>% select(date,contains("_HP"))
df <- left_join(df,EU2,by="date",fill=NA) %>% select(date,contains("_HP"))
df <- left_join(df,v2,by="date",fill=NA) %>% select(date,contains("_HP"))
df <- left_join(df,vu2,by="date",fill=NA) %>% select(date,contains("_HP"))
df <- left_join(df,LP2,by="date",fill=NA) %>% select(date,contains("_HP"))

### Construct the statistics for 2014-2019, 2020-2022, and 2014-2022
# Note: If the numbers seem strange check the scale - as the standard deviation is not scale invariant, and scaling choices were not made clear in the paper.

df_14_19 <- df %>% filter(date >= "2014-01-01" & date <= "2019-12-01")

ggplot(df_14_19 %>% pivot_longer(!date,names_to = "variable",values_to = "value"), aes(x=date,y=value,colour=variable)) + geom_line() # Clearly shows a significant seasonal pattern.

# Calculations on actual data 

std <- sapply(df_14_19,sd,na.rm=TRUE)

correlation <- cor(df_14_19[2:ncol(df_14_19)])

elasticitytoP <- data.frame(cov(df_14_19[2:ncol(df_14_19)])*100/std[2:ncol(df_14_19)]) %>% select(tail(colnames(df_14_19),1))

# ac <- acf(df_14_19[2:ncol(df_14_19)],pl=FALSE)[1,] # Think these are conditional autocorrelations, so do this the simple way
# 
# ac1 <- unlist(ac)[seq(1,by=8,length.out=7)] 

### Seasonally adjusted version of above - involves running x13 on the above series and reconstructing the filtered values.

library(seasonal)

URS <- tibble(date = UR$date,URS = seas(ts(UR$UR,start=c(1978,2),end=c(2022,4),freq=4))$series$s11) %>% mutate(lURS = log(URS)) %>% drop_na %>% mutate(lURS_HP = hpfilter(lURS,freq=hpf,type="lambda")$cycle) # s10 is the seasonally adjusted data, s12 is the trend

JFRS <- tibble(date = JFR$date, JFRS = seas(ts(JFR$JFRQ,start=c(2003,1),end=c(2022,4),freq=4))$series$s11) %>% mutate(lJFRS = log(JFRS)) %>% drop_na %>% mutate(lJFRS_HP = hpfilter(lJFRS,freq=hpf,type="lambda")$cycle)

SRS <- tibble(date = SR$date, SRS = seas(ts(SR$SRQ,start=c(2003,1),end=c(2022,4),freq=4))$series$s11) %>% mutate(lSRS = log(SRS)) %>% drop_na %>% mutate(lSRS_HP = hpfilter(lSRS,freq=hpf,type="lambda")$cycle)

UES <- tibble(date = UE$date,UES = seas(ts(UE$UEQ,start=c(2003,1),end=c(2022,4),freq=4))$series$s11) %>% mutate(lUES = log(UES)) %>% drop_na %>% mutate(lUES_HP = hpfilter(lUES,freq=hpf,type="lambda")$cycle)

EUS <- tibble(date = EU$date,EUS = seas(ts(EU$EUQ,start=c(2003,1),end=c(2022,4),freq=4))$series$s11) %>% mutate(lEUS = log(EUS)) %>% drop_na %>% mutate(lEUS_HP = hpfilter(lEUS,freq=hpf,type="lambda")$cycle)
# 
# vS <- tibble(date = v$date,vS = seas(ts(v$v,start=c(2006,1),end=c(2022,3),freq=4))$series$s11) %>%  mutate(lvS = log(vS)) %>% drop_na %>% mutate(lvS_HP = hpfilter(lvS,freq=hpf,type="lambda")$cycle)
# 
# vuS <- tibble(date = vu$date,vuS = seas(ts(vu$vu,start=c(2006,1),end=c(2022,3),freq=4))$series$s11) %>% mutate(lvuS = log(vuS)) %>% drop_na %>% mutate(lvuS_HP = hpfilter(lvuS,freq=hpf,type="lambda")$cycle)


vS <- tibble(date = v$date,vS = seas(ts(v$v,start=c(1979,2),end=c(2022,4),freq=4))$series$s11) %>%  mutate(lvS = log(vS)) %>% drop_na %>% mutate(lvS_HP = hpfilter(lvS,freq=hpf,type="lambda")$cycle)

vuS <- tibble(date = vu$date,vuS = seas(ts(vu$vu,start=c(1979,2),end=c(2022,4),freq=4))$series$s11) %>% mutate(lvuS = log(vuS)) %>% drop_na %>% mutate(lvuS_HP = hpfilter(lvuS,freq=hpf,type="lambda")$cycle)

LPS <- tibble(date = LP$date,LPS = seas(ts(LP$Labour_Prod,start=c(1978,3),end=c(2022,3),freq=4))$series$s11) %>% mutate(lLPS = log(LPS)) %>% drop_na %>% mutate(lLPS_HP = hpfilter(lLPS,freq=hpf,type="lambda")$cycle)

dfS <- left_join(URS,JFRS,by="date",fill=NA) %>% select(date,contains("_HP"))
dfS <- left_join(dfS,SRS,by="date",fill=NA) %>% select(date,contains("_HP"))
dfS <- left_join(dfS,UES,by="date",fill=NA) %>% select(date,contains("_HP"))
dfS <- left_join(dfS,EUS,by="date",fill=NA) %>% select(date,contains("_HP"))
dfS <- left_join(dfS,vS,by="date",fill=NA) %>% select(date,contains("_HP"))
dfS <- left_join(dfS,vuS,by="date",fill=NA) %>% select(date,contains("_HP"))
dfS <- left_join(dfS,LPS,by="date",fill=NA) %>% select(date,contains("_HP"))





########## Set periodisation


datestart = "2009-01-01"
dateend = "2019-12-01"

dfS_14_19 <- dfS %>% filter(date >= datestart & date <= dateend)

ggplot(dfS_14_19 %>% pivot_longer(!date,names_to = "variable",values_to = "value"), aes(x=date,y=value,colour=variable)) + geom_line() 

# Calculations on seasonally adjusted data

stdS <- sapply(dfS_14_19,sd,na.rm=TRUE)

correlationS <- cor(dfS_14_19[2:ncol(dfS_14_19)]) 

elasticitytoPS <- data.frame(cov(dfS_14_19[2:ncol(dfS_14_19)])*100/stdS[ncol(dfS_14_19)]) %>% select(tail(colnames(dfS_14_19),1)) # Believe the elasticty figures are scaled up by 100 - but need to check this

# acS <- acf(dfS_14_19[2:ncol(dfS_14_19)],pl=FALSE)[1,] # Think these are conditional autocorrelations, so do this the simple way
#  
# acS1 <- unlist(acS)[seq(1,by=8,length.out=7)]

b <- dfS_14_19[2:ncol(dfS_14_19)]

a<- dfS %>% filter(date >= (ymd(datestart) %m-% months(3)) & date <= (ymd(dateend) %m-% months(3))) %>% select(!date)
colnames(a) <- paste("lag_",colnames(a),sep="")

acS1 <- numeric(length=8)

for (i in 1:8){
  acS1[i] <- cor(a[,i],b[i])
}
names(acS1) <- colnames(b)
    
    
### Export results and save sets of dated replication data (above code will always give us latest data, so need to save a replication set to go with any working paper)

std
correlation # Have kept full right now for check, but only want last column
elasticitytoP
ac1

stdS
correlationS[1:7,ncol(correlationS)] 
elasticitytoPS
acS1


