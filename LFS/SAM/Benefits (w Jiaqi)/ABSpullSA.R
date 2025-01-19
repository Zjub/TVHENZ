# Using externally SA data where possible.

rm(list=ls())

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

### Assumptions
# HPfilter number

hpf <- 1600 # 100000

#### Set periodisation for moments

datestart = "2006-01-01"
dateend = "2019-12-01"
dateendtrend = "2019-12-01"

# First download the series needed. labour_force,	unemployed,	unemp_rate,	GDP,	UE,	EU,	E,	U,	HRS,	HWI
# LP, unemployment rate, job finding rate, UE, EU,v, and v/u

## Get LP and UR

Hours <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(series == "Hours worked: Index ;") %>% filter(grepl('Table 1',table_title)) %>% drop_na %>% filter(unit == "Index Numbers") %>% select("date","value") # National accounts hours information give us greater autocorrelation in productivity than Chindamo and Uren (2010) find over the same period.

colnames(Hours) <- c("date","Hours")

# Hours <- read_abs(cat_no = "6202.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(grepl("Table 19",table_title)) %>% filter(series == "Monthly hours worked in all jobs ;  Persons ;") %>% select(date,value) %>% mutate(rollmean(value,k=3,fill=NA,align="right")) %>% select(!value)
# colnames(Hours) <- c("date","Hours") # Hours from labour market data as cross-cehck
# 
# Hours <- Hours[seq(3,nrow(Hours),3),]

GDP <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(series == "Gross domestic product: Chain volume measures ;") %>% filter(table_title == "Table 1. Key National Accounts Aggregates") %>% select("date","series","value")

LS <- read_abs(cat_no = "6202.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original") %>% filter(series == "Employed total ;  Persons ;" | series == "Unemployed total ;  Persons ;"  | series == "Unemployment rate ;  Persons ;") %>% select("date","series","value")

LS2 <- pivot_wider(LS,id_cols = "date",names_from = "series",values_from = "value")
colnames(LS2) <- c("date","Employed","Unemployed","UR_ABS")

LS2 <- LS2 %>% mutate(UR_Derived = Unemployed/(Employed + Unemployed),QUnemp = rollmean(Unemployed,k=3,fill=NA,align="right"),QEmp =rollmean(Employed,k=3,fill=NA,align="right"),UR = UR_ABS) # UR = QUnemp/(QEmp+QUnemp)

LSQ <- LS2[seq(5,nrow(LS2),3),] %>% select(date,QUnemp,QEmp,UR)

#### Test using "The not employed rate" instead of the unemployment rate.

# LS <- read_abs(cat_no = "6202.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original") %>% filter(series == "Employed total ;  Persons ;" | series == "Unemployed total ;  Persons ;"   | series == "Participation rate ;  Persons ;"  | series == "Unemployment rate ;  Persons ;") %>% select("date","series","value")
# 
# LS2 <- pivot_wider(LS,id_cols = "date",names_from = "series",values_from = "value")
# colnames(LS2) <- c("date","Employed","Unemployed","UR_ABS","PR")
# 
# LS2 <- LS2 %>% mutate(UR_Derived = Unemployed/(Employed + Unemployed),QUnemp = rollmean(Unemployed,k=3,fill=NA,align="right"),QEmp =rollmean(Employed,k=3,fill=NA,align="right"),UR = (1-PR*(1-UR_ABS))) # UR is not the unemployment rate - it is the not employed rate
# 
# LSQ <- LS2[seq(5,nrow(LS2),3),] %>% select(date,QUnemp,QEmp,UR)

#####

UR <- LSQ %>% select(date,UR) 

# Now labour productivity alternatives

LP <- left_join(LSQ,GDP,by="date") %>% mutate(Labour_Prod = value/QEmp) %>% select(date,Labour_Prod) %>% drop_na # Derived output per worker - the definition in FR12.

# LP <- left_join(Hours,GDP,by="date" )%>% mutate(Labour_Prod = value/Hours) %>% select(date,Labour_Prod) %>% drop_na # Output per hour

# LP <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Key National Accounts Aggregates") %>% filter(series == "GDP per hour worked: Index ;") %>% select("date","value") %>% drop_na # Provided output per hour
# colnames(LP) <- c("date","Labour_Prod")

# LP <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Key National Accounts Aggregates") %>% filter(series == "Gross value added per hour worked market sector: Index ;") %>% select("date","value") %>% drop_na # Provided output per hour
# colnames(LP) <- c("date","Labour_Prod")

ggplot(LP,aes(x=date,y=Labour_Prod)) + geom_line()

# Additional alternative

#6202.0 only had overall hours, and 6291.0.55.001 did not have hours worked by industry. This provides a great run down of the labour market data: https://www.abs.gov.au/statistics/understanding-statistics/guide-labour-statistics/industry-employment-guide.  Suggests using the labour account series.

# Hours <- read_abs(cat_no = "6150.0.55.003") %>% filter(series == "Volume; Labour Account hours actually worked in all jobs ;  Australia ;  Total all industries ;") %>% filter(series_type == "Seasonally Adjusted") %>% filter(grepl("Table 1",table_title))
# 
# HoursMining <- read_abs(cat_no = "6150.0.55.003") %>% filter(series == "Volume; Labour Account hours actually worked in all jobs ;  Australia ;  Mining (B) ;") %>% filter(series_type == "Seasonally Adjusted") %>% filter(grepl("Table 3",table_title))
# 
# HoursXMine <- tibble(date = Hours$date,HoursXM = Hours$value - HoursMining$value,Hours = Hours$value)
# 
# GVA <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(grepl("Table 45",table_title)) %>% filter(series == "Total all industries ;  Gross value added at basic prices ;" | series == "Mining (B) ;  Gross value added at basic prices ;") %>% select(date,series,value) %>% pivot_wider(names_from = series,values_from = value) %>% transmute(date=date,GVAxM = `Total all industries ;  Gross value added at basic prices ;` - `Mining (B) ;  Gross value added at basic prices ;`,GVA= `Total all industries ;  Gross value added at basic prices ;`)
# 
# LP <- left_join(GVA,HoursXMine,by="date") %>% transmute(date=date,LPxM = GVAxM/HoursXM,LP=GVA/Hours) # LP in terms of GVA by non-mining industries, divided by hours worked outside of mining. The time series is much shorter
# 
# ggplot(LP %>% pivot_longer(!date,names_to = "variable",values_to = "value"),aes(x=date,y=value,colour=variable)) + geom_line()



## Get UE and EU and JFR and SR [Note, they make simple quarterly averages of monthly data - rather than calculating quarters, so we use that as our core number]

dfgf <- read_lfs_grossflows(weights = "previous") %>% group_by(date,lfs_current,lfs_previous) %>% summarise(number = sum(persons))

EmpTrans <- dfgf %>% filter((lfs_current == "Unemployed") & (lfs_previous == "Employed full-time" | lfs_previous == "Employed part-time")) %>% group_by(date) %>% summarise(URin = sum(number))

UEm <- data.frame(date = EmpTrans$date, UE = EmpTrans$URin) %>% mutate(UEQ =rollmean(UE,k=3,fill=NA,align="right"))

UE <- UEm[seq(3,nrow(UEm),3),] %>% select(date,UEQ)

UnTrans <- dfgf %>% filter((lfs_current == "Employed full-time" | lfs_current == "Employed part-time") & (lfs_previous == "Unemployed")) %>% group_by(date) %>% summarise(URout = sum(number)) # lfs_previous == "Not in the labour force (NILF)") & - NILF excluded

EUm <- data.frame(date = UnTrans$date, EU = UnTrans$URout) %>% mutate(EUQ =rollmean(EU,k=3,fill=NA,align="right"))

EU <- EUm[seq(3,nrow(UEm),3),] %>% select(date,EUQ)

pEmployed <- dfgf %>% filter((lfs_previous == "Employed full-time" | lfs_previous == "Employed part-time")) %>% group_by(date) %>% summarise(number = sum(number))

pUnemployed <- dfgf %>% filter(lfs_previous == "Unemployed") %>% group_by(date) %>% summarise(number = sum(number))

Employed <- dfgf %>% filter((lfs_current == "Employed full-time" | lfs_current == "Employed part-time")) %>% group_by(date) %>% summarise(number = sum(number))

# JFR is the proportion of previously unemployed who become employed as a percent of previously unemployed

JFRm <- data.frame(date = pUnemployed$date,JFR = UnTrans$URout/pUnemployed$number) %>% mutate(JFRQ =rollmean(JFR,k=3,fill=NA,align="right"))
setDT(JFRm)

mean(JFRm[date >= datestart & date <= dateend]$JFR) # The monthly average for the paper


# SR is the proportion of the previously employed who become unemployed

SRm <- data.frame(date = pEmployed$date,JFR = EmpTrans$URin/pEmployed$number) %>% mutate(SRQ =rollmean(JFR,k=3,fill=NA,align="right"))
setDT(SRm)

mean(SRm[date >= datestart & date <= dateend]$JFR) # The monthly average for the paper

JFR <- JFRm[seq(3,nrow(JFRm),3),] %>% select(date,JFRQ)

SR <- SRm[seq(3,nrow(SRm),3),] %>% select(date,SRQ)

## Use the ABS data - this is quarterly

vac <- read_abs(cat_no = "6354.0") %>% filter(series == "Job Vacancies ;  Australia ;") %>% filter(grepl('TABLE 1',table_title)) %>% filter(series_type == "Seasonally Adjusted")

vac$date <- vac$date %m+% months(1) -14

vac <- vac %>% select(date,value)
colnames(vac) <- c("date","vacancy")

# Need to impute the data that was not collected for August 2008 to August 2009 based on a regression of the log difference in vacancies against the change in the unemployment rate from pre-May 2008. Note: Probably better to just use a package than experiment https://cran.r-project.org/web/packages/imputeTS/vignettes/imputeTS-Time-Series-Missing-Value-Imputation-in-R.pdf

library(Hmisc)

vacdf <- left_join(vac,LSQ,by="date") %>% select(date,vacancy,QUnemp) %>% mutate(time = seq(1,nrow(vac),by=1))

vac_imp <- aregImpute(~vacancy + QUnemp + time,data=vacdf %>% filter(date <= "2020-01-01"))

vacdf[is.na(vacdf$vacancy),]$vacancy <- vac_imp$imputed$vacancy[nrow(vac_imp$imputed$vacancy),]

plot(vacdf$vacancy)
plot(vacdf$QUnemp)

vu <- left_join(vacdf,LSQ, by="date")

v <- vu %>% transmute(date=date,v=vacancy)

vu <- vu %>% transmute(date=date,vu = vacancy/(QUnemp.x*1000))

plot(vu %>% filter(date >= "2000-01-01"))

ggplot(vu,aes(x=date,y=vu*1000)) + geom_line()+ geom_vline(xintercept = as.Date("2020-03-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("2008-09-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("1987-09-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("1997-06-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("1990-03-01"),colour="black",linetype = "dashed") + labs(
  title = "Tightness measures",
  subtitle = "Vacancies per unemployed person",
  y="",
  source = "ABS National Accounts.
Source: ABS, e61.",
)  + theme_e61(base_family = "Quattrocento Sans", legend = "bottom")

#save_e61(filename = 'C:/MN personal planning/Income_Support/plots/TightnessCrisis.svg')


### The core figures now need to be logged and then HP filtered, prior to the summary statistics being constructed.
# LP, unemployment rate, job finding rate, separation rate, UE, EU,v, and v/u
# Check whether the value desired is cycle or cycle + trend - impression from literature is that these are models that are fitting the cyclical relationships.


### Seasonally adjusted version of above - involves running x13 on the above series and reconstructing the filtered values.

library(seasonal)

# URS <- tibble(date = LSQ$date,URS = LSQ$QUnemp) %>% mutate(lURS = log(URS)) %>% drop_na %>% mutate(lURS_HP = hpfilter(lURS,freq=hpf,type="lambda",drift=TRUE)$cycle) # This is not the rate, but the level - for checking against Chindamo and Uren
# Add "dateendtrend" here as we only want to do the filtered series up to 2019 - so that the swings during COVID don't mess up trend estimates.

URS <- tibble(date = UR$date,URS = UR$UR) %>% mutate(lURS = log(URS)) %>% drop_na %>% filter(date <= dateendtrend) %>% mutate(lURS_HP = hpfilter(lURS,freq=hpf,type="lambda")$cycle) 
setDT(URS)

mean(URS[date >= datestart & date <= dateend]$URS) # The monthly average for the paper

JFRS <- tibble(date = JFR$date, JFRS = seas(ts(JFR$JFRQ,start=c(2003,1),end=c(2022,4),freq=4))$series$s11) %>% filter(date <= dateendtrend)  %>% mutate(lJFRS = log(JFRS)) %>% drop_na %>% mutate(lJFRS_HP = hpfilter(lJFRS,freq=hpf,type="lambda")$cycle)

SRS <- tibble(date = SR$date, SRS = seas(ts(SR$SRQ,start=c(2003,1),end=c(2022,4),freq=4))$series$s11)%>% filter(date <= dateendtrend)  %>% mutate(lSRS = log(SRS)) %>% drop_na %>% mutate(lSRS_HP = hpfilter(lSRS,freq=hpf,type="lambda")$cycle)

UES <- tibble(date = UE$date,UES = seas(ts(UE$UEQ,start=c(2003,1),end=c(2022,4),freq=4))$series$s11)%>% filter(date <= dateendtrend)  %>% mutate(lUES = log(UES)) %>% drop_na %>% mutate(lUES_HP = hpfilter(lUES,freq=hpf,type="lambda",drift=TRUE)$cycle)

EUS <- tibble(date = EU$date,EUS = seas(ts(EU$EUQ,start=c(2003,1),end=c(2022,4),freq=4))$series$s11)%>% filter(date <= dateendtrend)  %>% mutate(lEUS = log(EUS)) %>% drop_na %>% mutate(lEUS_HP = hpfilter(lEUS,freq=hpf,type="lambda",drift=TRUE)$cycle)

vS <- tibble(date = v$date,vS = v$v) %>%  mutate(lvS = log(vS)) %>% drop_na%>% filter(date <= dateendtrend)  %>% mutate(lvS_HP = hpfilter(lvS,freq=hpf,type="lambda",drift=TRUE)$cycle)

vuS <- tibble(date = vu$date,vuS = vu$vu) %>% mutate(lvuS = log(vuS)) %>% drop_na%>% filter(date <= dateendtrend)  %>% mutate(lvuS_HP = hpfilter(lvuS,freq=hpf,type="lambda")$cycle)

LPS <- tibble(date = LP$date,LPS = LP$Labour_Prod) %>% mutate(lLPS = log(LPS)) %>% drop_na%>% filter(date <= dateendtrend)  %>% mutate(lLPS_HP = hpfilter(lLPS,freq=hpf,type="lambda",drift=TRUE)$cycle)

# LPSND <- tibble(date = LP$date,LPS = LP$Labour_Prod) %>% mutate(lLPS = log(LPS)) %>% drop_na %>% mutate(lLPS_HP = hpfilter(lLPS,freq=hpf,type="lambda")$cycle)

GDPS <- tibble(date = GDP$date,GDPS = GDP$value) %>% mutate(lGDPS = log(GDPS)) %>% drop_na%>% filter(date <= dateendtrend)  %>% mutate(lGDPS_HP = hpfilter(lGDPS,freq=hpf,type="lambda",drift=TRUE)$cycle)

# Check cyclicality of productivity

econCheck <- left_join(LPS,GDPS,by="date") %>% select(date,contains("_HP")) %>% filter(date <= dateend & date >= datestart)

cor(econCheck[2:3])

ggplot(econCheck %>% pivot_longer(!date,names_to = "variable",values_to = "value"),aes(x=date,y=value,colour=variable)) + geom_line() # Countercyclical in this period - but it is procyclical over a longer period (back to the 1980s)

dfS <- left_join(URS,JFRS,by="date",fill=NA) %>% select(date,contains("_HP"))
dfS <- left_join(dfS,SRS,by="date",fill=NA) %>% select(date,contains("_HP"))
dfS <- left_join(dfS,UES,by="date",fill=NA) %>% select(date,contains("_HP"))
dfS <- left_join(dfS,EUS,by="date",fill=NA) %>% select(date,contains("_HP"))
dfS <- left_join(dfS,vS,by="date",fill=NA) %>% select(date,contains("_HP"))
dfS <- left_join(dfS,vuS,by="date",fill=NA) %>% select(date,contains("_HP"))
dfS <- left_join(dfS,LPS,by="date",fill=NA) %>% select(date,contains("_HP"))


dfS_14_19 <- dfS %>% filter(date >= datestart & date <= dateend)

ggplot(dfS_14_19 %>% pivot_longer(!date,names_to = "variable",values_to = "value"), aes(x=date,y=value,colour=variable)) + geom_line() 

ggplot(dfS_14_19 %>% select(date,lURS_HP,lEUS_HP,lUES_HP) %>% pivot_longer(!date,names_to = "variable",values_to = "value"), aes(x=date,y=value,colour=variable)) + geom_line() 

# Calculations on seasonally adjusted data

stdS <- sapply(dfS_14_19,sd,na.rm=TRUE)

correlationS <- cor(dfS_14_19[2:ncol(dfS_14_19)],method="pearson") 
c <- ccf(dfS_14_19[,2],dfS_14_19[,9]) # UR and LP cyclical components CCF

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

stdSF <- data.frame(variable = names(stdS),stdS) %>% filter(variable != "date")
colnames(stdSF) = c("variable","Std Deviation")
fullcorrelations <- data.frame(colnames(correlationS)[1:7],correlationS[1:7,])
corrF <- data.frame(colnames(correlationS)[1:7],correlationS[1:7,ncol(correlationS)])
colnames(corrF) = c("variable","Correlation with Labour Productivity")
elasticitytoPSF <- data.frame(names(elasticitytoPS),elasticitytoPS)
colnames(elasticitytoPSF) = c("variable","Elasticity")
acS1F <- data.frame(names(acS1),acS1)
colnames(acS1F) <- c("variable","Autocorrelation")
datadictionary <- data.frame(names(acS1),c("Unemployment Rate","Job Finding Rate","Separation Rate","U to E Transition Flow","E to U Transition Flow","Vacancies","Vacancy to Unemployment Ratio","Labour Productivity"))
colnames(datadictionary) <- c("variable","Description")

exportdf <- list(stdSF,corrF,elasticitytoPSF,acS1F,datadictionary)
names(exportdf) <- c("Standard Deviations","Correlations with Productivity","Elasticities","Autocorrelations","Data Dictionary")

exportdf

library(openxlsx)

# write.xlsx(exportdf,"Aussie_Targets_v2.xlsx") # The last file exported

## Testing productivity smoothing

