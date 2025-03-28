### Read libraries ----
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


### Productivity ----

GDP <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(series == "Gross domestic product: Chain volume measures ;") %>% filter(table_title == "Table 1. Key National Accounts Aggregates") %>% select("date","series","value")

Hours <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(series == "Hours worked: Index ;") %>% filter(grepl('Table 1',table_title)) %>% drop_na %>% filter(unit == "Index Numbers") %>% select("date","value")

colnames(Hours) <- c("date","Hours")

LS <- read_abs(cat_no = "6202.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original") %>% filter(series == "Employed total ;  Persons ;" | series == "Unemployed total ;  Persons ;"  | series == "Unemployment rate ;  Persons ;") %>% select("date","series","value")

LS2 <- pivot_wider(LS,id_cols = "date",names_from = "series",values_from = "value")
colnames(LS2) <- c("date","Employed","Unemployed","UR_ABS")

LS2 <- LS2 %>% mutate(UR_Derived = Unemployed/(Employed + Unemployed),QUnemp = rollmean(Unemployed,k=3,fill=NA,align="right"),QEmp =rollmean(Employed,k=3,fill=NA,align="right"),UR = UR_ABS) 

LSQ <- LS2[seq(5,nrow(LS2),3),] %>% select(date,QUnemp,QEmp,UR)

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

ggplot(LP_full,aes(x=date,y=value[,1],colour=name)) + geom_line() #+ plot_label("Hourly Productivity",y=1.8,x="1985-03-01",colour=palette_e61(4)[2]) + plot_label("Output per employee",y=1.6,x="1985-03-01",colour=palette_e61(4)[3]) + labs_e61(title = "Labour Productivity", subtitle = "Index, 1980 = 1, HP smoothing parameter = 1600",y="",x="") + scale_y_continuous_e61(limits = c(0.9,2,0.2),y_top = FALSE)

# LP growth

setDT(LP)
LP$Trend <- data.table(LP$Trend)
LP[,lag_Trend := shift(Trend,4)][,LP_trend_growth := Trend/lag_Trend - 1]
LP[,lag_LP := shift(Labour_Prod,4)][,LP_growth := Labour_Prod/lag_LP - 1]

ggplot(LP[date > as.Date("2012-12-30")],aes(x=date,y=LP_trend_growth)) + geom_line()
ggplot(LP[date > as.Date("2012-12-30")],aes(x=date,y=LP_growth)) + geom_line()

ggplot(melt(LP[date > as.Date("2012-12-30"),.(date,LP_growth,LP_trend_growth)],id="date"),aes(x=date,y=value,colour=variable)) + 
  geom_line() +
  labs_e61(title = "Labour productivity",subtitle = "GDP per hour worked",y="") +
  scale_y_continuous_e61(limits = c(-0.03,0.04,0.01),labels = scales::percent_format()) +
  geom_hline(yintercept = 0) +
  plot_label(c("Trend","Quarterly"),x=c(as.Date("2016-03-01"),as.Date("2016-03-01")),y=c(0.025,0.035))


save_e61("Newsletter4.png",res=2)


#### Real wages

wpi <- read_abs("6345.0")
setDT(wpi)

wpi_SA <- wpi[series_type == "Seasonally Adjusted" & data_type == "INDEX"][, series2 := sapply(str_split(series, ";\\s+"), function(x) x[4])]

ggplot(wpi_SA[],aes(x=date,y=value,colour=series2)) + geom_line()

unique(wpi[str_detect(series, regex("year", ignore_case = TRUE)) & 
             str_detect(series, regex("Total", ignore_case = TRUE)) & 
             str_detect(series, regex("All industries", ignore_case = TRUE)) &
             str_detect(series, regex("Australia", ignore_case = TRUE)) &
             str_detect(series, regex("Private and Public", ignore_case = TRUE)) &
             series_type == "Original" & 
             data_type == "PERCENT" & 
             date > as.Date("2012-12-30")]$series)

ann_wpi_growth <- wpi[series %in% c("Percentage Change from Corresponding Quarter of Previous Year ;  Total hourly rates of pay excluding bonuses ;  Australia ;  Private and Public ;  All industries ;","Percentage Change from Corresponding Quarter of Previous Year ;  Total hourly rates of pay excluding bonuses ;  Australia ;  Public ;  All industries ;","Percentage Change from Corresponding Quarter of Previous Year ;  Total hourly rates of pay excluding bonuses ;  Australia ;  Private ;  All industries ;") & series_type == "Original" & 
                        data_type == "PERCENT" & 
                        date > as.Date("2012-12-30") & table_no == "634501"]

ann_wpi_growth <- ann_wpi_growth[, series2 := sapply(str_split(series, ";\\s+"), function(x) x[4])][,.(date,value,series2)]

cpi <- read_abs(cat_no = "6401.0")
setDT(cpi)

cpi_growth <- cpi[table_title == "TABLES 1 and 2. CPI: All Groups, Index Numbers and Percentage Changes" & unit == "Percent" & series == "Percentage Change from Corresponding Quarter of Previous Year ;  All groups CPI ;  Australia ;" & date > as.Date("2012-12-30")][,.(date,cpi_value = value)]

cpi_growth <- cpi_growth[ann_wpi_growth[series2 == "Private and Public "],on=.(date)][,RW := value - cpi_value]

ggplot(cpi_growth,aes(x=date,y=RW/100)) + geom_line() + geom_hline(yintercept = 0) +
  labs_e61(title = "Real Wage Growth",subtitle = "WPI divided by CPI",y="") +
  scale_y_continuous_e61(limits = c(-0.05,0.03,0.01),labels = scales::percent_format())

save_e61("Newsletter5.png",res=2)

unique(wpi$series)
unique(cpi$table_title)

## Quarterly
# wpi_value <- wpi[series %in% c("Quarterly Index ;  Total hourly rates of pay excluding bonuses ;  Australia ;  Private and Public ;  All industries ;" ) & series_type %in% c("Seasonally Adjusted")][,.(date,wpi =value)]
# cpi_value <- cpi[table_title == "TABLES 1 and 2. CPI: All Groups, Index Numbers and Percentage Changes" & unit == "Index Numbers" & series == "Index Numbers ;  All groups CPI ;  Australia ;"][,.(date,cpi = value)]

## Annual
wpi_value <- wpi[series %in% c("Financial Year Index ;  Total hourly rates of pay including bonuses ;  Australia ;  Private and Public ;  All industries ;" )][,.(date,wpi =value)]
wpi_value <-unique(wpi_value)
cpi_value <- cpi[table_title == "TABLES 1 and 2. CPI: All Groups, Index Numbers and Percentage Changes" & unit == "Index Numbers" & series == "Index Numbers ;  All groups CPI ;  Australia ;"][,.(date,cpi = value)]

cpi_value[, year := year(date)]
cpi_value[, quarter := quarter(date)]

cpi_annual <- cpi_value[quarter %in% c(3, 4, 1, 2),
                        .(cpi_avg = mean(cpi, na.rm = TRUE)),
                        by = year]

cpi_annual <- cpi_annual[year %in% cpi_value[quarter == 2, unique(year)]]
cpi_annual[, date := as.Date(paste0(year, "-06-01"))]
setorder(cpi_annual, date)
cpi_annual <- cpi_annual[, .(date, cpi_avg)]

cpi_annual

price_values <- cpi_value[wpi_value,on=.(date)][,Real_Wage := wpi/cpi]

price_values[nrow(price_values),.(Real_Wage)]/max(price_values$Real_Wage) - 1
#price_values[nrow(price_values),.(Real_Wage)]/price_values[date == as.Date("2019-12-01"),.(Real_Wage)] - 1
price_values[nrow(price_values),.(Real_Wage)]/price_values[date == as.Date("2019-06-01"),.(Real_Wage)] - 1

price_values[Real_Wage == max(Real_Wage)]

#price_values$MA12 <- rollmean(price_values$Real_Wage, k = 12, fill = NA, align = "center")
#ggplot(price_values, aes(x = date, y = Real_Wage)) +
#  geom_line() +
#  geom_line(aes(y = MA12), color = "orange", size = 1)

ggplot(price_values,aes(x=date,y=Real_Wage)) + geom_line() + scale_x_date(date_breaks = "4 year", date_labels = "%Y") +
  scale_y_continuous_e61(limits = c(0.9,1.2,0.05)) +
  labs_e61(title = "Wage Price Index","September 1997 = 1",y="",x="")

#save_e61("WPI_full.png",res=2)


### Average weekly earnings

AWE <- read_abs(cat_no = "6302.0")


### Nat accounts income per hours

NAcc <- read_abs("5204.0")
setDT(NAcc)

NAcc_subset <- NAcc[table_title %like% "16"]

NAcc_subset <- NAcc_subset[series %like% "Compensation of employees per hour: Current prices ;"][,.(date,COE = value)]

price2 <- NAcc_subset[price_values,on=.(date)][,Real_COE := COE/cpi]

ref_2012 <- price2[date == "2012-06-01", .(Real_COE_ref = Real_COE, Real_Wage_ref = Real_Wage)]

price2[, `:=`(
  Real_COE = Real_COE / ref_2012$Real_COE_ref,
  Real_Wage = Real_Wage / ref_2012$Real_Wage_ref
)]

ggplot(melt(price2[,.(date,Real_Wage,Real_COE)],id.vars = "date"),aes(x=date,y=value,colour=variable)) + geom_line() +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y") +
  scale_y_continuous_e61(limits = c(0.7,1.2,0.1)) +
  labs_e61(title = "Wage Measures","June 2012 = 1",y="",x="") +
  geom_hline(yintercept = 1) +
  plab(c("WPI","COE per hour"),x=rep(as.Date("2001-06-01")),y=c(1.07,1.12))

save_e61("WPI_COE.png",res=2)

(44/42.5 - 1) - (cpi_value[date %in% c(as.Date("2023-06-01"))]$cpi/cpi_value[date %in% c(as.Date("2021-06-01"))]$cpi - 1) # Real wage change EEH


