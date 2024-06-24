
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

hpf <- 16000 #100000

GDP <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(series == "Gross domestic product: Chain volume measures ;") %>% filter(table_title == "Table 1. Key National Accounts Aggregates") %>% select("date","series","value")

LS <- read_abs(cat_no = "6202.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original") %>% filter(series == "Employed total ;  Persons ;" | series == "Unemployed total ;  Persons ;"  | series == "Unemployment rate ;  Persons ;") %>% select("date","series","value")

LS2 <- pivot_wider(LS,id_cols = "date",names_from = "series",values_from = "value")
colnames(LS2) <- c("date","Employed","Unemployed","UR_ABS")

LP <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Key National Accounts Aggregates") %>% filter(series == "GDP per hour worked: Index ;") %>% select("date","value") %>% drop_na # Provided output per hour
colnames(LP) <- c("date","Labour_Prod")

WPI <- read_abs(cat_no = "6345.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(grepl("Quarterly Index ;",series)) %>% select(date,series,value) %>% str_remove(series,"Quarterly Index ;  Total hourly rates of pay excluding bonuses ;  Australia ;  ")

LPS <- tibble(date = LP$date,LPS = LP$Labour_Prod) %>% mutate(lLPS = log(LPS)) %>% drop_na %>% mutate(lLPS_HP = hpfilter(lLPS,freq=hpf,type="lambda")$cycle + hpfilter(lLPS,freq=hpf,type="lambda")$trend,lLPS_trend = hpfilter(lLPS,freq=hpf,type="lambda")$trend)

LPdf <- LPS %>% select(!c("LPS","lLPS_HP")) %>% filter(date > "1986-01-01") %>% pivot_longer(!date,names_to = "variable", values_to = "value")

ggplot(LPdf,aes(x=date,y=value,colour=variable)) + geom_line() + geom_vline(xintercept = as.Date("2020-03-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("2008-09-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("1987-09-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("1990-03-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("1997-06-01"),colour="black",linetype = "dashed") + labs(
  title = "Labour productivity after a crisis",
  subtitle = "Real GDP per hour worked",
  y="",
  caption = "ABS National Accounts.
Source: ABS, e61.",
)  + theme_e61(base_family = "Quattrocento Sans", legend = "bottom")

save_e61(filename = 'C:/MN personal planning/Income_Support/plots/LP_Crisis.svg')

ggplot(LS %>% filter(series == "Unemployment rate ;  Persons ;"), aes(x=date,y=value/100)) + geom_line() + geom_vline(xintercept = as.Date("2020-03-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("2008-09-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("1987-09-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("1997-06-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("1990-03-01"),colour="black",linetype = "dashed") + labs(
  title = "Unemployment rate after a crisis",
  subtitle = "Seasonally adjusted rate",
  y="",
  caption = "ABS National Accounts.
Source: ABS, e61.",
)  + theme_e61(base_family = "Quattrocento Sans", legend = "bottom")

save_e61(filename = 'C:/MN personal planning/Income_Support/plots/UR_Crisis.svg')

WPI$series <- str_remove(WPI$series,"Quarterly Index ;  Total hourly rates of pay excluding bonuses ;  Australia ;  ")
WPI$series <- str_remove(WPI$series," ;  All industries ;")

ggplot(WPI,aes(x=date,y=value,colour=series)) + geom_line()+ geom_vline(xintercept = as.Date("2020-03-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("2008-09-01"),colour="black",linetype = "dashed") + labs(
  title = "Nominal Wages",
  subtitle = "Quality adjusted nominal wages",
  y="",
  caption = "ABS National Accounts.
Source: ABS, e61.",
)  + theme_e61(base_family = "Quattrocento Sans", legend = "bottom")

save_e61(filename = 'C:/MN personal planning/Income_Support/plots/NomWage_Crisis.svg')

CPI <- read_abs(cat_no = "6401.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "TABLE 8. CPI: Analytical Series, Weighted Average of Eight Capital Cities") %>% filter(data_type == "INDEX") %>% filter(date > "1997-07-01") %>% filter(series == "Index Numbers ;  All groups CPI, seasonally adjusted ;  Australia ;") %>% select(date,value)
colnames(CPI) <- c("date","CPI")

WPI <- left_join(WPI,CPI,by="date")
WPI <- WPI %>% mutate(RW = value/CPI)

ggplot(WPI %>% filter(series == "Private and Public"),aes(x=date,y=RW)) + geom_line() + geom_vline(xintercept = as.Date("2020-03-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("2008-09-01"),colour="black",linetype = "dashed") + labs(
  title = "Real Wages",
  subtitle = "Quality adjusted real wages",
  y="",
  caption = "ABS National Accounts.
Source: ABS, e61.",
)  + theme_e61(base_family = "Quattrocento Sans", legend = "bottom")


save_e61(filename = 'C:/MN personal planning/Income_Support/plots/RealWage_Crisis.svg')

LS <- read_abs(cat_no = "6202.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original") %>% filter(series == "Employed total ;  Persons ;" | series == "Unemployed total ;  Persons ;"   | series == "Participation rate ;  Persons ;"  | series == "Unemployment rate ;  Persons ;") %>% select("date","series","value")

LS2 <- pivot_wider(LS,id_cols = "date",names_from = "series",values_from = "value")
colnames(LS2) <- c("date","Employed","Unemployed","UR_ABS","PR")

ggplot(LS2 %>% select(date,PR) %>% filter(date >= "1997-07-01"),aes(x=date,y=PR)) + geom_line() + geom_vline(xintercept = as.Date("2020-03-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("2008-09-01"),colour="black",linetype = "dashed") + labs(
  title = "Participation Rates and crisis",
  subtitle = "Seasonally adjusted rate",
  y="",
  caption = "ABS National Accounts.
Source: ABS, e61.",
)  + theme_e61(base_family = "Quattrocento Sans", legend = "bottom")


save_e61(filename = 'C:/MN personal planning/Income_Support/plots/PR_Crisis.svg')

#

LS2 <- LS2 %>% mutate(UR_Derived = Unemployed/(Employed + Unemployed),QUnemp = rollmean(Unemployed,k=3,fill=NA,align="right"),QEmp =rollmean(Employed,k=3,fill=NA,align="right"),UR = UR_ABS) # UR = QUnemp/(QEmp+QUnemp)

LSQ <- LS2[seq(5,nrow(LS2),3),] %>% select(date,QUnemp,QEmp,UR)

vac <- read_abs(cat_no = "6354.0") %>% filter(series == "Job Vacancies ;  Australia ;") %>% filter(grepl('TABLE 1',table_title)) %>% filter(series_type == "Seasonally Adjusted")

vac$date <- vac$date %m+% months(1) -14

vac <- vac %>% select(date,value)
colnames(vac) <- c("date","vacancy")

library(Hmisc)

vacdf <- left_join(vac,LSQ,by="date") %>% select(date,vacancy,QUnemp) %>% mutate(time = seq(1,nrow(vac),by=1))

vac_imp <- aregImpute(~vacancy + QUnemp + time,data=vacdf %>% filter(date <= "2020-01-01"))

vacdf[is.na(vacdf$vacancy),]$vacancy <- vac_imp$imputed$vacancy[nrow(vac_imp$imputed$vacancy),]

plot(vacdf$vacancy)
plot(vacdf$QUnemp)

vu <- left_join(vacdf,LSQ, by="date")

v <- vu %>% transmute(date=date,v=vacancy)

vu <- vu %>% transmute(date=date,vu = vacancy/(QUnemp.x))

plot(vu %>% filter(date >= "2000-01-01"))

ggplot(vu,aes(x=date,y=vu)) + geom_line()+ geom_vline(xintercept = as.Date("2020-03-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("2008-09-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("1987-09-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("1997-06-01"),colour="black",linetype = "dashed") + geom_vline(xintercept = as.Date("1990-03-01"),colour="black",linetype = "dashed") + labs(
  title = "Tightness measures",
  subtitle = "Vacancies per unemployed person",
  y="",
  caption = "ABS National Accounts.
Source: ABS, e61.",
)  + theme_e61(base_family = "Quattrocento Sans", legend = "bottom")

# Lets create US-Aussie comparisons

USPR <- read.csv("USPR.csv") %>% filter(DATE >= "2003-01-01")
USPR <- as.tibble(USPR)
USPR$DATE <- as.Date(USPR$DATE)
colnames(USPR) <- c("date","USA")
USUNEMPLOY <- read.csv("USUNEMPLOY.csv") %>% filter(DATE >= "2003-01-01")
USUNRATE <- read.csv("USUNRATE.csv") %>% filter(DATE >= "2003-01-01")
USUNRATE <- as.tibble(USUNRATE)
USUNRATE$DATE <- as.Date(USUNRATE$DATE)
colnames(USUNRATE) <- c("date","USA")

USVAC <- read.csv("USVAC.csv") %>% filter(DATE >= "2003-01-01")
USVUR <- inner_join(USUNEMPLOY,USVAC,by="DATE") 
USVUR <- USVUR %>% mutate(VU = LMJVTTUVUSM647S/(UNEMPLOY*1000)) %>% select("DATE","VU")
USVUR <- as.tibble(USVUR)
USVUR$DATE <- as.Date(USVUR$DATE)
colnames(USVUR) <- c("date","USA")
colnames(vu) <- c("date","Australia")

vacancy <- inner_join(USVUR,vu,by="date")

ggplot(pivot_longer(vacancy,!date,names_to = "variable",values_to = "values"),aes(x=date,y=values,colour=variable)) + geom_line() + labs(
  title = "US has larger vacancy cycles",
  subtitle = "Vacancies per unemployed person",
  y="   ",
  x="    ",
  caption = "ABS National Accounts.
Source: ABS, FRED, e61.",
)  + theme_e61_clean() + scale_colour_e61(n=2)

save_e61(filename = 'C:/MN personal planning/Income_Support/SAM/Vacancy.svg')

AUSPR <- LS2 %>% select(date,PR)
colnames(AUSPR) <- c("date","Australia")

PR <- inner_join(USPR,AUSPR,by="date")

ggplot(pivot_longer(PR,!date,names_to = "variable",values_to = "values"),aes(x=date,y=values,colour=variable)) + geom_line() + labs(
  title = "Australia has higher Labour Force Participation",
  subtitle = "Proportion of those aged 15+ who are in work or looking for work",
  y="   ",
  x="    ",
  caption = "ABS National Accounts.
Source: ABS, FRED, e61.",
)  + theme_e61_clean() + scale_colour_e61(n=2) + scale_y_continuous(labels=scales::percent_format(scale=1,suffix="%"))

save_e61(filename = 'C:/MN personal planning/Income_Support/SAM/PR.svg')

AUSUR <- LS2 %>% select(date,UR)
colnames(AUSUR) <- c("date","Australia")

UR <- inner_join(USUNRATE,AUSUR,by="date")
colnames(AUSPR) <- c("date","Australia")

ggplot(pivot_longer(UR,!date,names_to = "variable",values_to = "values"),aes(x=date,y=values,colour=variable)) + geom_line() + labs(
  title = "Australia has smaller unemployment cycles",
  subtitle = "Proportion of those aged 15+ who looking for work, out of labour force",
  y="   ",
  x="    ",
  caption = "ABS National Accounts.
Source: ABS, FRED, e61.",
)  + theme_e61_clean() + scale_colour_e61(n=2) + scale_y_continuous(limits=c(0.,15),labels=scales::percent_format(scale=1,suffix="%"))

save_e61(filename = 'C:/MN personal planning/Income_Support/SAM/UR.svg')