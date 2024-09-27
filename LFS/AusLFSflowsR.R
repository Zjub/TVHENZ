# Unemployment risk from ABS website

rm(list=ls())
gc()

.libPaths(new = 'C:/Rpackage')

#library(devtools)

# devtools::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

library(tidyverse)
library(theme61)
library(tidyr)
library(readxl)
library(gghighlight)
library(readabs)
library(seasonal)

### Set last month of the data analysed.

endmonth <- 8 ## Note "12" refers to last month of data, so needs to be updated
years_data <- 18

### Transitions

## Check weights - have gone with "previous"

dfgf <- read_lfs_grossflows(weights = "previous")

colnames(dfgf)

## Look for COVID transitions

unique(dfgf$lfs_current)
unique(dfgf$lfs_previous)

# Employed to unemployed transition

COVIDdf <- dfgf %>% group_by(date,lfs_current,lfs_previous) %>% summarise(number = sum(persons))

ggplot(COVIDdf %>% filter(date > "2018-12-01" & lfs_current == "Unemployed" & (lfs_previous == "Employed full-time" | lfs_previous == "Employed part-time")), aes(x=date,y=number,colour=lfs_previous)) + geom_line()

ggplot(COVIDdf %>% filter(date > "2018-12-01" & lfs_current == "Not in the labour force (NILF)" & (lfs_previous == "Employed full-time" | lfs_previous == "Employed part-time")), aes(x=date,y=number,colour=lfs_previous)) + geom_line()

ggplot(COVIDdf %>% filter(date > "2018-12-01" & (lfs_current == "Not in the labour force (NILF)" | lfs_current == "Unemployed" | lfs_current == "Employed part-time") & (lfs_previous == "Employed full-time")), aes(x=date,y=number,colour=lfs_previous)) + geom_line()

FullTrans <- COVIDdf %>% filter((lfs_current == "Not in the labour force (NILF)" | lfs_current == "Unemployed" | lfs_current == "Employed part-time") & (lfs_previous == "Employed full-time")) %>% group_by(date) %>% summarise(FTout = sum(number))

## Transition from FT

COVIDts <- ts(FullTrans$FTout,frequency = 12,start = c(2005,9))

FullTransdf <- seas(COVIDts)

save <- cbind(SA = final(FullTransdf), trend = trend(FullTransdf))
plot(FullTransdf, trend=TRUE)

dates <- seq(as.Date("2005-09-01"), as.Date("2024-08-01"), "months")

saves <- data.frame(dates,save)

savesdf <- saves %>% pivot_longer(!dates,names_to = "variable",values_to = "value") ## Continue here, trying to make the SA and the trend variables here

ggplot(savesdf %>% filter(dates > "2017-12-01"),aes(x=dates,y=value,colour=variable)) + geom_line() + geom_vline(xintercept = as.Date("2020-04-01"),colour="red") + geom_vline(xintercept = as.Date("2020-09-01"),colour="blue") + geom_vline(xintercept = as.Date("2021-03-01"),colour="blue") + labs(title = "Full-time to other labour market status")

PriorEmpTotal <- COVIDdf %>% filter((lfs_previous == "Employed full-time")) %>% group_by(date) %>% summarise(Emp = sum(number))

Rateb <- merge(FullTrans,PriorEmpTotal,by="date") %>% summarise(date= date,proportion=FTout/Emp)

ggplot(Rateb, aes(x=date,y=proportion)) + geom_line() + geom_vline(xintercept = as.Date("2020-04-01"),colour="red") + geom_vline(xintercept = as.Date("2020-09-01"),colour="blue") + geom_vline(xintercept = as.Date("2021-03-01"),colour="blue") + labs(title = "FT to other status rate")

COVIDtsb <- ts(Rateb$proportion,frequency = 12,start = c(2005,9))

FullTransdfb <- seas(COVIDtsb)

saveb <- cbind(SA = final(FullTransdfb), trend = trend(FullTransdfb))
plot(FullTransdfb, trend=TRUE)

dates <- seq(as.Date("2005-09-01"), as.Date("2024-08-01"), "months")

savesb <- data.frame(dates,saveb)

savesdfb <- savesb %>% pivot_longer(!dates,names_to = "variable",values_to = "value") ## Continue here, trying to make the SA and the trend variables here

ggplot(savesdfb %>% filter(dates > "2017-12-01"),aes(x=dates,y=value,colour=variable)) + geom_line() + geom_vline(xintercept = as.Date("2020-04-01"),colour="red") + geom_vline(xintercept = as.Date("2020-09-01"),colour="blue") + geom_vline(xintercept = as.Date("2021-03-01"),colour="blue") + labs(title = "FT to other status rate")

## Transition to EMP to UNEMP

URTrans <- COVIDdf %>% filter((lfs_current == "Unemployed") & (lfs_previous == "Employed full-time" | lfs_previous == "Employed part-time")) %>% group_by(date) %>% summarise(URout = sum(number))

COVIDts2 <- ts(URTrans$URout,frequency = 12,start = c(2005,9))

URTransdf <- seas(COVIDts2)

save2 <- cbind(SA = final(URTransdf), trend = trend(URTransdf))
plot(URTransdf, trend=TRUE)

dates <- seq(as.Date("2005-09-01"), as.Date("2024-08-01"), "months")

saves2 <- data.frame(dates,save2)

savesdf2 <- saves2 %>% pivot_longer(!dates,names_to = "variable",values_to = "value") ## Continue here, trying to make the SA and the trend variables here

ggplot(savesdf2 %>% filter(dates > "2017-12-01"),aes(x=dates,y=value,colour=variable)) + geom_line() + geom_vline(xintercept = as.Date("2020-04-01"),colour="red") + geom_vline(xintercept = as.Date("2020-09-01"),colour="blue") + geom_vline(xintercept = as.Date("2021-03-01"),colour="blue") + labs(title = "Employed to Unemployed")

# Transition to fulltime

FullTrans2 <- COVIDdf %>% filter((lfs_previous == "Not in the labour force (NILF)" | lfs_previous == "Unemployed" | lfs_previous == "Employed part-time") & (lfs_current == "Employed full-time")) %>% group_by(date) %>% summarise(FTin = sum(number))

COVIDts3 <- ts(FullTrans2$FTin,frequency = 12,start = c(2005,9))

FullTransdf2 <- seas(COVIDts3)

save3 <- cbind(SA = final(FullTransdf2), trend = trend(FullTransdf2))
plot(FullTransdf2, trend=TRUE)

dates <- seq(as.Date("2005-09-01"), as.Date("2024-08-01"), "months")

saves3 <- data.frame(dates,save3)

savesdf3 <- saves3 %>% pivot_longer(!dates,names_to = "variable",values_to = "value") ## Continue here, trying to make the SA and the trend variables here

ggplot(savesdf3 %>% filter(dates > "2017-12-01"),aes(x=dates,y=value,colour=variable)) + geom_line() + geom_vline(xintercept = as.Date("2020-04-01"),colour="red") + geom_vline(xintercept = as.Date("2020-09-01"),colour="blue") + geom_vline(xintercept = as.Date("2021-03-01"),colour="blue") + labs(title = "Other labour market status to FT")

# Part time to full time

FullTrans4 <- COVIDdf %>% filter((lfs_previous == "Employed part-time") & (lfs_current == "Employed full-time")) %>% group_by(date) %>% summarise(FTin = sum(number))

COVIDts4 <- ts(FullTrans4$FTin,frequency = 12,start = c(2005,9))

FullTransdf4 <- seas(COVIDts4)

save4 <- cbind(SA = final(FullTransdf4), trend = trend(FullTransdf4))
plot(FullTransdf4, trend=TRUE)

dates <- seq(as.Date("2005-09-01"), as.Date("2024-08-01"), "months")

saves4 <- data.frame(dates,save4)

savesdf4 <- saves4 %>% pivot_longer(!dates,names_to = "variable",values_to = "value") ## Continue here, trying to make the SA and the trend variables here

ggplot(savesdf4 %>% filter(dates > "2017-12-01"),aes(x=dates,y=value,colour=variable)) + geom_line() + geom_vline(xintercept = as.Date("2020-04-01"),colour="red") + geom_vline(xintercept = as.Date("2020-09-01"),colour="blue") + geom_vline(xintercept = as.Date("2021-03-01"),colour="blue") + labs(title = "PT to FT")


# Unemp to full time


FullTrans5 <- COVIDdf %>% filter((lfs_previous == "Unemployed") & (lfs_current == "Employed full-time")) %>% group_by(date) %>% summarise(FTin = sum(number))

COVIDts5 <- ts(FullTrans5$FTin,frequency = 12,start = c(2005,9))

FullTransdf5 <- seas(COVIDts5)

save5 <- cbind(SA = final(FullTransdf5), trend = trend(FullTransdf5))
plot(FullTransdf5, trend=TRUE)

dates <- seq(as.Date("2005-09-01"), as.Date("2024-08-01"), "months")

saves5 <- data.frame(dates,save5)

savesdf5 <- saves5 %>% pivot_longer(!dates,names_to = "variable",values_to = "value") ## Continue here, trying to make the SA and the trend variables here

ggplot(savesdf5 %>% filter(dates > "2017-12-01"),aes(x=dates,y=value,colour=variable)) + geom_line() + geom_vline(xintercept = as.Date("2020-04-01"),colour="red") + geom_vline(xintercept = as.Date("2020-09-01"),colour="blue") + geom_vline(xintercept = as.Date("2021-03-01"),colour="blue") + labs(title = "UNEMP to FT")

TotalTrans <- COVIDdf %>% group_by(date) %>% summarise(TotalTrans = sum(number)) # Total population in labour market

Rate5 <- merge(FullTrans5,TotalTrans,by="date") %>% mutate(pnumber = lag(TotalTrans)) %>% summarise(date = date,lproportion = FTin/pnumber) # proportion = FTin/TotalTrans,

ggplot(Rate5, aes(x=date,y=lproportion)) + geom_line()

PriorUnemTotal <- COVIDdf %>% filter((lfs_previous == "Unemployed")) %>% group_by(date) %>% summarise(Unem = sum(number))

Rate5b <- merge(FullTrans5,PriorUnemTotal,by="date") %>% summarise(date= date,proportion=FTin/Unem)

ggplot(Rate5b, aes(x=date,y=proportion)) + geom_line() + geom_vline(xintercept = as.Date("2020-04-01"),colour="red") + geom_vline(xintercept = as.Date("2020-09-01"),colour="blue") + geom_vline(xintercept = as.Date("2021-03-01"),colour="blue") + labs(title = "UNEMP to FT rate")

COVIDts5b <- ts(Rate5b$proportion,frequency = 12,start = c(2005,9))

FullTransdf5b <- seas(COVIDts5b)

save5b <- cbind(SA = final(FullTransdf5b), trend = trend(FullTransdf5b))
plot(FullTransdf5b, trend=TRUE)

dates <- seq(as.Date("2005-09-01"), as.Date("2024-08-01"), "months")

saves5b <- data.frame(dates,save5b)

savesdf5b <- saves5b %>% pivot_longer(!dates,names_to = "variable",values_to = "value") ## Continue here, trying to make the SA and the trend variables here

ggplot(savesdf5b %>% filter(dates > "2017-12-01"),aes(x=dates,y=value,colour=variable)) + geom_line() + geom_vline(xintercept = as.Date("2020-04-01"),colour="red") + geom_vline(xintercept = as.Date("2020-09-01"),colour="blue") + geom_vline(xintercept = as.Date("2021-03-01"),colour="blue") + labs(title = "UNEMP to FT rate")

# NILF to full time


FullTrans6 <- COVIDdf %>% filter((lfs_previous == "Not in the labour force (NILF)") & (lfs_current == "Employed full-time")) %>% group_by(date) %>% summarise(FTin = sum(number))

COVIDts6 <- ts(FullTrans6$FTin,frequency = 12,start = c(2005,9))

FullTransdf6 <- seas(COVIDts6)

save6 <- cbind(SA = final(FullTransdf6), trend = trend(FullTransdf6))
plot(FullTransdf6, trend=TRUE)

dates <- seq(as.Date("2005-09-01"), as.Date("2024-08-01"), "months")

saves6 <- data.frame(dates,save6)

savesdf6 <- saves6 %>% pivot_longer(!dates,names_to = "variable",values_to = "value") ## Continue here, trying to make the SA and the trend variables here

ggplot(savesdf6 %>% filter(dates > "2017-12-01"),aes(x=dates,y=value,colour=variable)) + geom_line() + geom_vline(xintercept = as.Date("2020-04-01"),colour="red") + geom_vline(xintercept = as.Date("2020-09-01"),colour="blue") + geom_vline(xintercept = as.Date("2021-03-01"),colour="blue") + labs(title = "NILF to FT")


# UNEMP to EMP

FullTrans7 <- COVIDdf %>% filter((lfs_previous == "Not in the labour force (NILF)") & (lfs_current == "Employed full-time" | lfs_current == "Employed part-time")) %>% group_by(date) %>% summarise(FTin = sum(number))

COVIDts7 <- ts(FullTrans7$FTin,frequency = 12,start = c(2005,9))

FullTransdf7 <- seas(COVIDts7)

save7 <- cbind(SA = final(FullTransdf7), trend = trend(FullTransdf7))
plot(FullTransdf7, trend=TRUE)

dates <- seq(as.Date("2005-09-01"), as.Date("2024-08-01"), "months")

saves7 <- data.frame(dates,save7)

savesdf7 <- saves7 %>% pivot_longer(!dates,names_to = "variable",values_to = "value") ## Continue here, trying to make the SA and the trend variables here

ggplot(savesdf7 %>% filter(dates > "2017-12-01" & dates < "2022-01-01"),aes(x=dates,y=value,colour=variable)) + geom_line() + geom_vline(xintercept = as.Date("2020-04-01"),colour="red") + geom_vline(xintercept = as.Date("2020-09-01"),colour="blue") + geom_vline(xintercept = as.Date("2021-03-01"),colour="blue") + labs(title = "UNEMP to EMP")


# NILF to unemployed transition


#### Set up the data to have "year" comparisons - what we are using for meeting

COVIDdf <- dfgf %>% group_by(date,lfs_current,lfs_previous) %>% summarise(number = sum(persons))

years <- as.numeric(substr(COVIDdf$date, 1, 4))
COVIDdf <- cbind(COVIDdf,year = years) %>% filter(date > "2005-12-01")

# Unemp - Emp
Prev_Unem <- COVIDdf %>% group_by(date) %>% filter(lfs_previous == "Unemployed") %>% summarise(year=mean(year),pUNEMP = sum(number))

UnemptoEmp <- COVIDdf %>% group_by(date) %>% filter(lfs_previous == "Unemployed" & (lfs_current == "Employed full-time" | lfs_current == "Employed part-time")) %>% summarise(year=mean(year),TransUNEMP = sum(number))

ggplot(merge(Prev_Unem,UnemptoEmp,by="date") %>% mutate(TUErate = TransUNEMP/pUNEMP,month=c(rep(seq(1,12,by=1),times=years_data),seq(1,endmonth,by=1))),aes(x=month,y=TUErate,colour=as.factor(year.x))) + geom_line() 

UnemptoEmpFull <- merge(Prev_Unem,UnemptoEmp,by="date") %>% mutate(TUErate = TransUNEMP/pUNEMP,month=c(rep(seq(1,12,by=1),times=years_data),seq(1,endmonth,by=1)))

avgyear = c(seq(2003,2019,by=1))

avgUEtran <- UnemptoEmpFull %>% filter(year.x %in% avgyear) %>% group_by(month) %>% summarise(TUErate = mean(TUErate)) %>% mutate(year.x = "Avg 2003-2019")

COVIDyearUEtran <- UnemptoEmpFull %>% filter(year.x == 2020 | year.x == 2021) %>% select(month, TUErate, year.x)

UEtrandf <- rbind(avgUEtran,COVIDyearUEtran)

ggplot(UEtrandf %>% filter(year.x != 2021),aes(x=month,y=TUErate,colour=year.x)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%")) + labs(title = "Monthly transition from unemployed to employed") + scale_x_continuous(breaks = unique(UEtrandf$month))

ggplot(UEtrandf,aes(x=month,y=TUErate,colour=year.x)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%")) + labs(title = "Monthly transition from unemployed to employed") + scale_x_continuous(breaks = unique(UEtrandf$month))


# Emp - unemp
Prev_Emp <- COVIDdf %>% group_by(date) %>% filter(lfs_previous == "Employed full-time" | lfs_previous == "Employed part-time") %>% summarise(year=mean(year),pEMP = sum(number))

EmptoUnemp <- COVIDdf %>% group_by(date) %>% filter(lfs_current == "Unemployed" & (lfs_previous == "Employed full-time" | lfs_previous == "Employed part-time")) %>% summarise(year=mean(year),TransEMP = sum(number))

EmptoUnempFull <- merge(Prev_Emp,EmptoUnemp,by="date") %>% mutate(TEUrate = TransEMP/pEMP,month=c(rep(seq(1,12,by=1),times=years_data),seq(1,endmonth,by=1)))

avgEUtran <- EmptoUnempFull %>% filter(year.x %in% avgyear) %>% group_by(month) %>% summarise(TEUrate = mean(TEUrate)) %>% mutate(year.x = "Avg 2003-2019")

COVIDyearEUtran <- EmptoUnempFull %>% filter(year.x == 2020 | year.x == 2021) %>% select(month, TEUrate, year.x)

EUtrandf <- rbind(avgEUtran,COVIDyearEUtran)

ggplot(EUtrandf %>% filter(year.x != 2021),aes(x=month,y=TEUrate,colour=year.x)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%")) + labs(title = "Monthly transition from employed to unemployed")+ scale_x_continuous(breaks = unique(UEtrandf$month))

# Emp FT - Emp PT
Prev_EmpFT <- COVIDdf %>% group_by(date) %>% filter(lfs_previous == "Employed full-time") %>% summarise(year=mean(year),pEMPFT = sum(number))

EmpFTtoEmpPT <- COVIDdf %>% group_by(date) %>% filter(lfs_current == "Employed part-time" & (lfs_previous == "Employed full-time")) %>% summarise(year=mean(year),TransEMP = sum(number))

EmpFTtoEmpPTFull <- merge(Prev_EmpFT,EmpFTtoEmpPT,by="date") %>% mutate(TEErate = TransEMP/pEMPFT,month=c(rep(seq(1,12,by=1),times=years_data),seq(1,endmonth,by=1)))

avgEEtran <- EmpFTtoEmpPTFull %>% filter(year.x %in% avgyear) %>% group_by(month) %>% summarise(TEErate = mean(TEErate)) %>% mutate(year.x = "Avg 2003-2019")

COVIDyearEEtran <- EmpFTtoEmpPTFull %>% filter(year.x == 2020 | year.x == 2021) %>% select(month, TEErate, year.x)

EEtrandf <- rbind(avgEEtran,COVIDyearEEtran)

ggplot(EEtrandf %>% filter(year.x != 2021),aes(x=month,y=TEErate,colour=year.x)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%")) + labs(title = "Monthly transition from employed FT to employed PT")+ scale_x_continuous(breaks = unique(UEtrandf$month))

ggplot(EEtrandf %>% filter(year.x != 2022),aes(x=month,y=TEErate,colour=year.x)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%")) + labs(title = "Monthly transition from employed FT to employed PT")+ scale_x_continuous(breaks = unique(UEtrandf$month))

# Emp FT - other status (NILF, UNEMP, PT)
Prev_EmpFT <- COVIDdf %>% group_by(date) %>% filter(lfs_previous == "Employed full-time") %>% summarise(year=mean(year),pEMPFT = sum(number))

EmpFTtoOther <- COVIDdf %>% group_by(date) %>% filter((lfs_current == "Unemployed"  | lfs_current == "Employed part-time"  | lfs_current == "Not in the labour force (NILF)") & (lfs_previous == "Employed full-time")) %>% summarise(year=mean(year),TransEMPFT = sum(number))

EmpFTtoOtherFull <- merge(Prev_EmpFT,EmpFTtoOther,by="date") %>% mutate(TEFTOrate = TransEMPFT/pEMPFT,month=c(rep(seq(1,12,by=1),times=years_data),seq(1,endmonth,by=1)))

avgEFTOtran <- EmpFTtoOtherFull %>% filter(year.x %in% avgyear) %>% group_by(month) %>% summarise(TEFTOrate = mean(TEFTOrate)) %>% mutate(year.x = "Avg 2003-2019")

COVIDyearEFTOtran <- EmpFTtoOtherFull %>% filter(year.x == 2020 | year.x == 2021) %>% select(month, TEFTOrate, year.x)

EFTOtrandf <- rbind(avgEFTOtran,COVIDyearEFTOtran)

ggplot(EFTOtrandf %>% filter(year.x != 2021),aes(x=month,y=TEFTOrate,colour=year.x)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%")) + scale_x_continuous(breaks = unique(EFTOtrandf$month)) + labs(title = "Monthly transition from employed FT to other status")

ggplot(EFTOtrandf,aes(x=month,y=TEFTOrate,colour=year.x)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%")) + labs(title = "Monthly transition from employed FT to other status")+ scale_x_continuous(breaks = unique(EFTOtrandf$month))

##### Graphs for the meeting
# Emp FT - Emp PT

colnames(EEtrandf) <- c("Month","Transition","Year")

EE <- ggplot(EEtrandf %>% filter(Year != 2021),aes(x=Month,y=Transition,colour=Year)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + labs_e61(
  title = "Monthly transition from employed FT to employed PT",
  footnotes = "ABS Labour Force Survey Gross Monthly Flows.",
sources = c("ABS", "e61"),
  )  + theme_e61(base_family = "Quattrocento Sans", legend = "bottom") + scale_x_continuous(breaks = unique(EEtrandf$Month)) + scale_y_continuous(limits=c(0.03,0.05),labels=scales::percent_format(scale=100,suffix="%",sec.axis = dup_axis())) 

EE + annotate(x=4,y=+Inf,label="Payment Introduced",vjust=2,geom="label") + annotate(x=9,y=+Inf,label="Payment Reduced",vjust=2,geom="label")

# ggplot(EEtrandf,aes(x=Month,y=Transition,colour=Year)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + labs(
#   title = "Monthly transition from employed FT to employed PT",
#   caption = "ABS Labour Force Survey Gross Monthly Flows.
# Source: ABS, e61.",
# )  + theme_e61(base_family = "Quattrocento Sans", legend = "bottom") + scale_x_continuous(breaks = unique(UEtrandf$month)) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%",sec.axis = dup_axis())) + scale_colour_e61(n=3)

# Emp FT - Other

colnames(EFTOtrandf) <- c("Month","Transition","Year")

EFTO <- ggplot(EFTOtrandf %>% filter(Year != 2021),aes(x=Month,y=Transition,colour=Year)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + labs(
  title = "Monthly transition from employed FT to other status",
  caption = "ABS Labour Force Survey Gross Monthly Flows.
Source: ABS, e61.",
)  + theme_e61(base_family = "Quattrocento Sans", legend = "bottom") + scale_x_continuous(breaks = unique(EUtrandf$Month)) + scale_y_continuous(limits = c(0.04,0.08),labels=scales::percent_format(scale=100,suffix="%",sec.axis = dup_axis())) + scale_colour_e61(n=2)

EFTO + annotate(x=4,y=+Inf,label="Payment Introduced",vjust=2,geom="label") + annotate(x=9,y=+Inf,label="Payment Reduced",vjust=2,geom="label")

# Emp - Unemp

colnames(EUtrandf) <- c("Month","Transition","Year")

EU <- ggplot(EUtrandf %>% filter(Year != 2021),aes(x=Month,y=Transition,colour=Year)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + labs(
  title = "Monthly transition from employed to unemployed",
  caption = "ABS Labour Force Survey Gross Monthly Flows.
Source: ABS, e61.",
)  + theme_e61(base_family = "Quattrocento Sans", legend = "bottom") + scale_x_continuous(breaks = unique(EUtrandf$Month)) + scale_y_continuous(limits = c(0.005,0.02),labels=scales::percent_format(scale=100,suffix="%",sec.axis = dup_axis())) + scale_colour_e61(n=2)

EU + annotate(x=4,y=+Inf,label="Payment Introduced",vjust=2,geom="label") + annotate(x=9,y=+Inf,label="Payment Reduced",vjust=2,geom="label")

# Unemp - Emp

colnames(UEtrandf) <- c("Month","Transition","Year")

UE <- ggplot(UEtrandf %>% filter(Year != 2021),aes(x=Month,y=Transition,colour=Year)) + geom_line() + geom_vline(xintercept = 4,colour="red",linetype = "dashed") + geom_vline(xintercept = 9,colour="blue",linetype = "dashed") + labs_e61(
  title = "Monthly transition from unemployed to employed",
  subtitle = "Percentage per month",
  y="",
  footnotes = "ABS Labour Force Survey Gross Monthly Flows.",
  sources = c("ABS","e61"),
)  + scale_x_continuous(breaks = 1:12) + scale_y_continuous_e61(limits=c(0.08,0.26),labels=scales::percent_format(scale=100,suffix="%",sec.axis = dup_axis())) + plab(c("2020","2006-2019"),x=c(5,5),y=c(0.13,0.11))

UE + annotate(x=4,y=+Inf,label="Payment Introduced",vjust=2,geom="label") + annotate(x=9,y=0.24,label="Payment Reduced",vjust=2,geom="label") 

save_e61(filename = 'plots/U2E.png',res=2,auto_scale = FALSE)
