## Plots and data for Monash presentation in May 2024
# Initial: 14/05/2024
# Author: Matt Nolan
# Last update: 15/05/2024

rm(list=ls())

library(remotes)
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
library(Hmisc)
library(seasonal)

#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

#setwd("C:/MN personal planning/LFS/Plots")

# HPfilter number

hpf <- 1600 

# Trend dates

datestart = "2004-03-01"
dateend = "2019-12-01"
dateendtrend = "2019-12-01"

## Macro aggregates ----

Hours <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(series == "Hours worked: Index ;") %>% filter(grepl('Table 1',table_title)) %>% drop_na %>% filter(unit == "Index Numbers") %>% select("date","value")

colnames(Hours) <- c("date","Hours")

GDP <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(series == "Gross domestic product: Chain volume measures ;") %>% filter(table_title == "Table 1. Key National Accounts Aggregates") %>% select("date","series","value")

LS <- read_abs(cat_no = "6202.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original") %>% filter(series == "Employed total ;  Persons ;" | series == "Unemployed total ;  Persons ;"  | series == "Unemployment rate ;  Persons ;") %>% select("date","series","value")

LS2 <- pivot_wider(LS,id_cols = "date",names_from = "series",values_from = "value")
colnames(LS2) <- c("date","Employed","Unemployed","UR_ABS")

LS2 <- LS2 %>% mutate(UR_Derived = Unemployed/(Employed + Unemployed),QUnemp = rollmean(Unemployed,k=3,fill=NA,align="right"),QEmp =rollmean(Employed,k=3,fill=NA,align="right"),UR = UR_ABS) 

LSQ <- LS2[seq(5,nrow(LS2),3),] %>% select(date,QUnemp,QEmp,UR)

UR <- LSQ %>% select(date,UR) 

LP <- left_join(LSQ,GDP,by="date") %>% mutate(Labour_Prod = value/QEmp) %>% select(date,Labour_Prod) %>% drop_na
LP$Labour_Prod <- LP$Labour_Prod/LP[LP$date == "1980-03-01",]$Labour_Prod

LP_hour <- left_join(Hours,GDP,by="date")%>% mutate(Hour_Labour_Prod = value/Hours) %>% select(date,Hour_Labour_Prod) %>% drop_na
LP_hour$Hour_Labour_Prod <- LP_hour$Hour_Labour_Prod/LP_hour[LP_hour$date == "1980-03-01",]$Hour_Labour_Prod

hp_lp_result <- hpfilter(LP$Labour_Prod, freq = 1600)

LP$Trend <- hp_lp_result$trend

hp_lph_result <- hpfilter(LP_hour$Hour_Labour_Prod, freq = 1600)

LP_hour$Hour_Trend <- hp_lph_result$trend

LP_full <- left_join(LP,LP_hour,by="date")

LP_full <- LP_full %>% pivot_longer(!date)

ggplot(LP_full,aes(x=date,y=value[,1],colour=name)) + geom_line() + plot_label("Hourly Productivity",y=1.8,x="1985-03-01",colour=palette_e61(4)[2]) + plot_label("Output per employee",y=1.6,x="1985-03-01",colour=palette_e61(4)[3]) + labs_e61(title = "Labour Productivity", subtitle = "Index, 1980 = 1, HP smoothing parameter = 1600",y="",x="") + scale_y_continuous_e61(limits = c(0.9,2,0.2),y_top = FALSE)

save_e61("Productivity.png",chart_type = "PPT",res=2,pad_width = 1,auto_scale = FALSE)

# LFP
LFS_gender <- read_abs(cat_no = "6202.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original")  %>% filter(series == "Participation rate ;  Persons ;" | series == "Participation rate ;  > Males ;"  | series == "Participation rate ;  > Females ;") %>% select("date","series","value")

ggplot(LFS_gender,aes(x=date,y=value/100,colour=series)) + geom_line() + scale_y_continuous_e61(limits=c(0.40,0.85,0.10),y_top=FALSE,labels=scales::percent_format()) + labs_e61(title="Labour Force Participation",subtitle="By reported gender",y="",x="") + plot_label("Male",y=0.78,x="1993-03-01",colour = palette_e61(3)[2]) + plot_label("Total",y=0.68,x="1993-03-01",colour = palette_e61(3)[3]) + plot_label("Female",y=0.58,x="1993-03-01",colour = palette_e61(3)[1])

save_e61("LFPgender.png",chart_type = "PPT",res=2,pad_width = 1,auto_scale = FALSE)



## Flow data ----

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

# SR is the proportion of the previously employed who become unemployed

SRm <- data.frame(date = pEmployed$date,JFR = EmpTrans$URin/pEmployed$number) %>% mutate(SRQ =rollmean(JFR,k=3,fill=NA,align="right"))

JFR <- JFRm[seq(3,nrow(JFRm),3),] %>% select(date,JFRQ)

SR <- SRm[seq(3,nrow(SRm),3),] %>% select(date,SRQ)

## Labour market tightnesxs: Use the ABS data - this is quarterly ----

vac <- read_abs(cat_no = "6354.0") %>% filter(series == "Job Vacancies ;  Australia ;") %>% filter(grepl('TABLE 1',table_title)) %>% filter(series_type == "Seasonally Adjusted")

vac$date <- vac$date %m+% months(1) -14

vac <- vac %>% select(date,value)
colnames(vac) <- c("date","vacancy")

# Need to impute the data that was not collected for August 2008 to August 2009 based on a regression of the log difference in vacancies against the change in the unemployment rate from pre-May 2008. Note: Probably better to just use a package than experiment https://cran.r-project.org/web/packages/imputeTS/vignettes/imputeTS-Time-Series-Missing-Value-Imputation-in-R.pdf

vacdf <- left_join(vac,LSQ,by="date") %>% select(date,vacancy,QUnemp) %>% mutate(time = seq(1,nrow(vac),by=1))

vac_imp <- aregImpute(~vacancy + QUnemp + time,data=vacdf %>% filter(date <= "2020-01-01"))

vacdf[is.na(vacdf$vacancy),]$vacancy <- vac_imp$imputed$vacancy[nrow(vac_imp$imputed$vacancy),]

plot(vacdf$vacancy)
plot(vacdf$QUnemp)

vu <- left_join(vacdf,LSQ, by="date")

v <- vu %>% transmute(date=date,v=vacancy)

vu <- vu %>% transmute(date=date,vu = vacancy/(QUnemp.x*1000))

plot(vu %>% filter(date >= "2000-01-01"))

ggplot(vu,aes(x=date,y=vu*1000)) + geom_line()+ geom_vline(xintercept = as.Date("2020-03-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("2008-09-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("1987-09-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("1997-06-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("1990-03-01"),colour="black",linetype = "dashed") + labs_e61(
  title = "Tightness measures",
  subtitle = "Vacancies per unemployed person",
  y="",
  sources = c("ABS, e61"),
) + scale_y_continuous_e61(limits=c(0,0.95,0.25),y_top=FALSE)

ggplot(vu,aes(x=date,y=vu*1000)) + geom_line() + labs_e61(
  title = "Tightness measures",
  subtitle = "Vacancies per unemployed person",
  y="",
  sources = c("ABS, e61"),
) + scale_y_continuous_e61(limits=c(0,0.95,0.25),y_top=FALSE)

save_e61("TightnessCrisis.png",res=2,pad_width = 1,chart_type = "PPT",auto_scale = FALSE)

ggplot()

## Seasonally adjust cyclical information

URS <- tibble(date = UR$date,URS = UR$UR) %>% mutate(lURS = log(URS)) %>% drop_na %>% filter(date <= dateendtrend) %>% mutate(lURS_HP = hpfilter(lURS,freq=hpf,type="lambda")$cycle) 

JFRS <- tibble(date = JFR$date, JFRS = seas(ts(JFR$JFRQ,start=c(2003,1),end=c(2022,4),freq=4))$series$s11) %>% filter(date <= dateendtrend)  %>% mutate(lJFRS = log(JFRS)) %>% drop_na %>% mutate(lJFRS_HP = hpfilter(lJFRS,freq=hpf,type="lambda")$cycle)

SRS <- tibble(date = SR$date, SRS = seas(ts(SR$SRQ,start=c(2003,1),end=c(2022,4),freq=4))$series$s11)%>% filter(date <= dateendtrend)  %>% mutate(lSRS = log(SRS)) %>% drop_na %>% mutate(lSRS_HP = hpfilter(lSRS,freq=hpf,type="lambda")$cycle)

UES <- tibble(date = UE$date,UES = seas(ts(UE$UEQ,start=c(2003,1),end=c(2022,4),freq=4))$series$s11)%>% filter(date <= dateendtrend)  %>% mutate(lUES = log(UES)) %>% drop_na %>% mutate(lUES_HP = hpfilter(lUES,freq=hpf,type="lambda",drift=TRUE)$cycle)

EUS <- tibble(date = EU$date,EUS = seas(ts(EU$EUQ,start=c(2003,1),end=c(2022,4),freq=4))$series$s11)%>% filter(date <= dateendtrend)  %>% mutate(lEUS = log(EUS)) %>% drop_na %>% mutate(lEUS_HP = hpfilter(lEUS,freq=hpf,type="lambda",drift=TRUE)$cycle)

vS <- tibble(date = v$date,vS = v$v) %>%  mutate(lvS = log(vS)) %>% drop_na%>% filter(date <= dateendtrend)  %>% mutate(lvS_HP = hpfilter(lvS,freq=hpf,type="lambda",drift=TRUE)$cycle)

vuS <- tibble(date = vu$date,vuS = vu$vu) %>% mutate(lvuS = log(vuS)) %>% drop_na%>% filter(date <= dateendtrend)  %>% mutate(lvuS_HP = hpfilter(lvuS,freq=hpf,type="lambda")$cycle)

LPS <- tibble(date = LP$date,LPS = LP$Labour_Prod) %>% mutate(lLPS = log(LPS)) %>% drop_na%>% filter(date <= dateendtrend)  %>% mutate(lLPS_HP = hpfilter(lLPS,freq=hpf,type="lambda",drift=TRUE)$cycle)

GDPS <- tibble(date = GDP$date,GDPS = GDP$value) %>% mutate(lGDPS = log(GDPS)) %>% drop_na%>% filter(date <= dateendtrend)  %>% mutate(lGDPS_HP = hpfilter(lGDPS,freq=hpf,type="lambda",drift=TRUE)$cycle)

econCheck <- left_join(LPS,GDPS,by="date") %>% select(date,contains("_HP")) %>% filter(date <= dateend & date >= datestart)


ggplot(econCheck %>% pivot_longer(!date,names_to = "variable",values_to = "value"),aes(x=date,y=value,colour=variable)) + geom_line() # Countercyclical in this period - but it is procyclical over a longer period (back to the 1980s)

dfS <- left_join(URS,JFRS,by="date") %>% select(date,contains("_HP"))
dfS <- left_join(dfS,SRS,by="date") %>% select(date,contains("_HP"))
dfS <- left_join(dfS,UES,by="date") %>% select(date,contains("_HP"))
dfS <- left_join(dfS,EUS,by="date") %>% select(date,contains("_HP"))
dfS <- left_join(dfS,vS,by="date",) %>% select(date,contains("_HP"))
dfS <- left_join(dfS,vuS,by="date") %>% select(date,contains("_HP"))
dfS <- left_join(dfS,LPS,by="date") %>% select(date,contains("_HP"))


dfS_14_19 <- dfS %>% filter(date >= datestart & date <= dateend)

ggplot(dfS_14_19 %>% pivot_longer(!date,names_to = "variable",values_to = "value"), aes(x=date,y=value,colour=variable)) + geom_line() 

ggplot(dfS_14_19 %>% select(date,lURS_HP,lEUS_HP,lUES_HP) %>% pivot_longer(!date,names_to = "variable",values_to = "value"), aes(x=date,y=value,colour=variable)) + geom_line() 


# Mobility plots

dfgf2 <- read_lfs_grossflows(weights = "previous")

COVIDdf <- dfgf2 %>% group_by(date,lfs_current,lfs_previous) %>% summarise(number = sum(persons))

years <- as.numeric(substr(COVIDdf$date, 1, 4))
COVIDdf <- cbind(COVIDdf,year = years) %>% filter(date > "2004-12-01" & date < "2024-01-01")

# Unemp - Emp
Prev_Unem <- COVIDdf %>% group_by(date) %>% filter(lfs_previous == "Unemployed") %>% summarise(year=mean(year),pUNEMP = sum(number))

UnemptoEmp <- COVIDdf %>% group_by(date) %>% filter(lfs_previous == "Unemployed" & (lfs_current == "Employed full-time" | lfs_current == "Employed part-time")) %>% summarise(year=mean(year),TransUNEMP = sum(number))

endmonth <- 12 ## Note "10" refers to last month of data, so needs to be updated

ggplot(merge(Prev_Unem,UnemptoEmp,by="date") %>% mutate(TUErate = TransUNEMP/pUNEMP,month=c(rep(seq(1,12,by=1),times=18),seq(1,endmonth,by=1))),aes(x=month,y=TUErate,colour=as.factor(year.x))) + geom_line() 

UnemptoEmpFull <- merge(Prev_Unem,UnemptoEmp,by="date") %>% mutate(TUErate = TransUNEMP/pUNEMP,month=c(rep(seq(1,12,by=1),times=18),seq(1,endmonth,by=1)))

length(unique(Prev_Unem$date))
length(unique(UnemptoEmp$date))

avgyear = c(seq(2005,2019,by=1))

avgUEtran <- UnemptoEmpFull %>% filter(year.x %in% avgyear) %>% group_by(month) %>% summarise(TUErate = mean(TUErate)) %>% mutate(year.x = "Avg 2005-2019")

COVIDyearUEtran <- UnemptoEmpFull %>% filter(year.x == 2020 | year.x == 2021 | year.x == 2022 | year.x == 2023) %>% select(month, TUErate, year.x)

UEtrandf <- rbind(avgUEtran,COVIDyearUEtran)

ggplot(UEtrandf %>% filter(year.x != 2021),aes(x=month,y=TUErate,colour=year.x)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%")) + labs(title = "Monthly transition from unemployed to employed") + scale_x_continuous(breaks = unique(UEtrandf$month))

ggplot(UEtrandf,aes(x=month,y=TUErate,colour=year.x)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%")) + labs_e61(
  title = "Unemployment to employment",
  subtitle = "Monthly Transitions",
  y="",
  sources = c("ABS, e61"),
)  + scale_x_continuous(breaks = unique(UEtrandf$month)) + theme_e61(legend = "right")

ggplot(UEtrandf,aes(x=month,y=TUErate,colour=year.x)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + scale_y_continuous_e61(labels=scales::percent_format(),limits = c(0.1,0.37,0.05),y_top=FALSE) + labs_e61(
  title = "Unemployment to employment",
  subtitle = "Monthly Transitions",
  y="",
  sources = c("ABS, e61"),
)  + scale_x_continuous(breaks = unique(UEtrandf$month)) + plot_label("2020",y=0.34,x=6,colour=palette_e61(5)[1]) + plot_label("2021",y=0.33,x=6,colour=palette_e61(5)[2]) + plot_label("2022",y=0.32,x=6,colour=palette_e61(5)[3]) + plot_label("2023",y=0.31,x=6,colour=palette_e61(5)[4]) + plot_label("2005-2019",y=0.29,x=6,colour=palette_e61(5)[5])

save_e61("UE.png",chart_type = "PPT",res=2,pad_width = 1,auto_scale=FALSE)


# Emp - Unemp
Prev_Emp <- COVIDdf %>% group_by(date) %>% filter(lfs_previous == "Employed full-time" | lfs_previous == "Employed part-time") %>% summarise(year=mean(year),pEMP = sum(number))

EmptoUnemp <- COVIDdf %>% group_by(date) %>% filter(lfs_current == "Unemployed" & (lfs_previous == "Employed full-time" | lfs_previous == "Employed part-time")) %>% summarise(year=mean(year),TransEMP = sum(number))

EmptoUnempFull <- merge(Prev_Emp,EmptoUnemp,by="date") %>% mutate(TEUrate = TransEMP/pEMP,month=c(rep(seq(1,12,by=1),times=18),seq(1,endmonth,by=1)))

avgEUtran <- EmptoUnempFull %>% filter(year.x %in% avgyear) %>% group_by(month) %>% summarise(TEUrate = mean(TEUrate)) %>% mutate(year.x = "Avg 2005-2019")

COVIDyearEUtran <- EmptoUnempFull %>% filter(year.x == 2020 | year.x == 2021 | year.x == 2022 | year.x == 2023) %>% select(month, TEUrate, year.x)

EUtrandf <- rbind(avgEUtran,COVIDyearEUtran)

ggplot(EUtrandf %>% filter(year.x != 2021 |year.x != 2022),aes(x=month,y=TEUrate,colour=year.x)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + scale_y_continuous_e61(labels=scales::percent_format(scale=100,suffix="%")) + labs(title = "Monthly transition from employed to unemployed")+ scale_x_continuous(breaks = unique(UEtrandf$month))

ggplot(EUtrandf,aes(x=month,y=TEUrate,colour=year.x)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + scale_y_continuous_e61(labels=scales::percent_format(),limits = c(0.003,0.019,0.003),y_top=FALSE) + labs_e61(
  title = "Employment to unemployment",
  subtitle = "Monthly Transitions",
  y="",
  sources = c("ABS", "e61"),
)  + scale_x_continuous(breaks = unique(UEtrandf$month)) + plot_label("2020",y=0.016,x=6,colour=palette_e61(5)[1]) + plot_label("2021",y=0.014,x=6,colour=palette_e61(5)[2]) + plot_label("2022",y=0.013,x=6,colour=palette_e61(5)[3]) + plot_label("2023",y=0.011,x=6,colour=palette_e61(5)[4]) + plot_label("2005-2019",y=0.01,x=6,colour=palette_e61(5)[5])

save_e61("EU.png",chart_type = "PPT",res=2,pad_width = 1,auto_scale=FALSE)

## Dependency ratios

Dep_ratio <- read_csv("C:/MN personal planning/LFS/DependencyratioWorldBank.csv")
setDT(Dep_ratio)
colnames(Dep_ratio) <- c("year","DR","yDR","oDR")

Dep_dt <- melt(Dep_ratio,id.vars = "year")
setDT(Dep_dt)
ggplot(Dep_dt[!(year == 2023),],aes(x=year,y=value/100,colour=variable)) + geom_line() + labs_e61(title="Dependency Ratio",subtitle = "Dependent per working age individual",y="",x="") + scale_y_continuous_e61(limits=c(0,0.7,0.1)) + plot_label("Total",x=1983,y=0.58,colour=palette_e61(3)[1]) + plot_label("Youth",x=1983,y=0.41,colour=palette_e61(3)[2]) + plot_label("Old age",x=1983,y=0.22,colour=palette_e61(3)[3])

save_e61("Dependency.png",chart_type = "PPT",res=2,pad_width = 1,auto_scale = FALSE)

## J2J PJSM

PJSM <- read_abs(cat_no = "6226.0")


## Age LFPR

LS_full <- read_abs(cat_no = "6291.0.55.001") %>% filter(table_title ==  "Table 01. Labour force status by Age, Social marital status, and Sex" )
setDT(LS_full)


LS_full[str_detect(series, "^> 15-24.*Persons ;$")]

LS_full[series == ">> 15-19 years ;  Participation rate ;  Persons ;"]

LFP_age <- LS_full[str_detect(series, "^> .*years ;  Participation rate ;  Persons ;$")][, age_range := str_extract(series, "^> [0-9]+-[0-9]+")][, age_range := str_remove(age_range, "^> ")][,.(date,age_range,value)]

ggplot(LFP_age,aes(x=date,y=value,colour=age_range)) + geom_line() + plot_label("15-24",x="2010-01-01",y=76,colour=palette_e61(5)[1])+ plot_label("25-34",x="2010-01-01",y=56,colour=palette_e61(5)[2])+ plot_label("35-44",x="2010-01-01",y=52,colour=palette_e61(5)[3])+ plot_label("45-54",x="2010-01-01",y=46,colour=palette_e61(5)[4])+ plot_label("55-64",x="2010-01-01",y=42,colour=palette_e61(5)[5]) + scale_y_continuous_e61(limits=c(30,90,10)) +labs_e61(y="",x="",title="Participation Rate",subtitle="Percent of Working Age Population 15-64")

save_e61("AgeLFP.png",chart_type = "PPT",res=2,pad_width = 1,auto_scale = FALSE)

## Industry and occupation composition


LS_full <- read_abs(cat_no = "6291.0.55.001")

unique(LS_full$table_title)

LS_industry <- read_abs(cat_no = "6291.0.55.001") %>% filter(table_title == "Table 04. Employed persons by Industry division of main job (ANZSIC) - Trend, Seasonally adjusted, and Original") %>% filter(series_type == "Seasonally Adjusted")
setDT(LS_industry)

LS_industry[,Agg_Ind := fcase(series %in% c("Agriculture, Forestry and Fishing ;  Employed total ;","Mining ;  Employed total ;"),"Primary",
                              series %in% c("Manufacturing ;  Employed total ;","Electricity, Gas, Water and Waste Services ;  Employed total ;","Construction ;  Employed total ;","Wholesale Trade ;  Employed total ;"),"Secondary",
                              series %in% c("Retail Trade ;  Employed total ;","Accommodation and Food Services ;  Employed total ;","Transport, Postal and Warehousing ;  Employed total ;"),"Sales",
                              series %in% c("Information Media and Telecommunications ;  Employed total ;","Financial and Insurance Services ;  Employed total ;","Rental, Hiring and Real Estate Services ;  Employed total ;","Professional, Scientific and Technical Services ;  Employed total ;","Administrative and Support Services ;  Employed total ;","Other Services ;  Employed total ;"),"Private Services",
                              series %in% c("Public Administration and Safety ;  Employed total ;","Education and Training ;  Employed total ;","Health Care and Social Assistance ;  Employed total ;","Arts and Recreation Services ;  Employed total ;"),"Public Services",
                              default = "Total"
                              )]

LS_agg_ind <- LS_industry[,.(agg_value = sum(value)),by=.(date,Agg_Ind)]

total_DT <- LS_agg_ind[Agg_Ind == "Total", .(date, total_value = agg_value)]

LS_agg_ind <- merge(LS_agg_ind,total_DT,by="date")

LS_agg_ind[,prop := agg_value/total_value][,year := year(date)]

LS_ann_agg_ind <- LS_agg_ind[,.(agg_value = sum(agg_value),total_value=sum(total_value)),by=.(Agg_Ind,year)][,prop := agg_value/total_value]

LS_ann_agg_ind <- LS_ann_agg_ind[year > 1984 & year < 2024] # remove partial years

LS_ann_agg_ind$Agg_Ind <- factor(LS_ann_agg_ind$Agg_Ind, levels =c("Public Services","Private Services","Sales","Secondary","Primary"))

ggplot(LS_ann_agg_ind[Agg_Ind == "Primary"],aes(x=year,y=prop)) + geom_col()

ggplot(LS_ann_agg_ind[Agg_Ind == "Secondary"],aes(x=year,y=prop)) + geom_col()

ggplot(LS_ann_agg_ind[Agg_Ind != "Total"],aes(x=year,y=prop,fill=Agg_Ind)) + geom_col() + labs_e61(title="Industry share of employment",subtitle="Share of all employees",sources=c("e61","ABS LLFS"),y="",x="") + scale_x_discrete(limits = seq(1985,2020,by = 7),expand = expansion(mult = c(0,0.4))) + plot_label("Public Services",x= 2024,y = 0.8,colour=palette_e61(5)[1]) + plot_label("Private Services",x= 2024,y = 0.6,colour=palette_e61(5)[2]) + plot_label("Sales",x= 2024,y = 0.35,colour=palette_e61(5)[3]) + plot_label("Secondary",x= 2024,y = 0.15,colour=palette_e61(5)[4]) + plot_label("Primary",x= 2024,y = 0.03,colour=palette_e61(5)[5]) +scale_y_continuous_e61(limits=c(0,1.002,0.25),y_top=FALSE)

save_e61("Industry.png",chart_type = "PPT",res=2,pad_width = 1,auto_scale = FALSE)

LS_occ <- read_abs(cat_no = "6291.0.55.001") %>% filter(table_title == "Table 07. Employed persons by Occupation major group of main job (ANZSCO) and Sex")
setDT(LS_occ)

unique(LS_occ$series)

LS_agg_occ <- LS_occ[str_detect(series,"Employed total ;  Persons ;$")][, category_name := str_extract(series, "^[^;]+")]

LS_agg_occ <- LS_agg_occ[,.(agg_value = sum(value)),by=.(date,category_name)]

total_occ <- LS_agg_occ[category_name == "Employed total ", .(date, total_value = agg_value)]

LS_agg_occ <- merge(LS_agg_occ,total_occ,by="date")

LS_agg_occ[,prop := agg_value/total_value][,year := year(date)]

LS_ann_agg_occ <- LS_agg_occ[,.(agg_value = sum(agg_value),total_value=sum(total_value)),by=.(category_name,year)][,prop := agg_value/total_value]

LS_ann_agg_occ <- LS_ann_agg_occ[year > 1986 & year < 2024][category_name != "Employed total "] # remove partial years

ggplot(LS_ann_agg_occ,aes(x=year,y=prop,fill=category_name)) + geom_col() + labs_e61(title="Occupation share of employment",subtitle="Share of all employees",sources=c("e61","ABS LLFS"),y="",x="") + scale_x_discrete(limits = seq(1985,2020,by = 7),expand = expansion(mult = c(0,0.4))) + plot_label("Administrative",x= 2024,y = 0.95,colour=palette_e61(8)[1]) + plot_label("Personal Service",x= 2024,y = 0.82,colour=palette_e61(8)[2]) + plot_label("Labourer",x= 2024,y = 0.73,colour=palette_e61(8)[3]) + plot_label("Machine Operator",x= 2024,y = 0.64,colour=palette_e61(8)[4]) + plot_label("Managers",x= 2024,y = 0.53,colour=palette_e61(8)[5]) + plot_label("Professionals",x= 2024,y = 0.35,colour=palette_e61(8)[6]) + plot_label("Sales",x= 2024,y = 0.18,colour=palette_e61(8)[7]) + plot_label("Trade",x= 2024,y = 0.05,colour=palette_e61(8)[8])+scale_y_continuous_e61(limits=c(0,1.002,0.25),y_top=FALSE)

save_e61("Occupation.png",chart_type = "PPT",res=2,pad_width = 1,auto_scale = FALSE)


WPI <- read_csv("C:/MN personal planning/LFS/WPI.csv")
setDT(WPI)

WPI[,dates := seq.Date(as.Date("2004-03-01"),as.Date("2024-03-01"),by="3 months")]

ggplot(WPI,aes(x=dates,y=RWG/100)) + geom_line() + geom_hline(yintercept = 0) + scale_y_continuous_e61(label=scales::percent_format(),limits=c(-0.04,0.02,0.01)) + labs_e61(title="Real Wage growth",subtitle = "Quality adjusted WPI minus CPI, annual",y="",x="")

save_e61("WPI.png",chart_type = "PPT",res=2,pad_width = 1,auto_scale = FALSE)

## Compare WPI to productivity growth



