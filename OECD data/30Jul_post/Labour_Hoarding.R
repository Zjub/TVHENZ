## For a tvhe post on 13 August on labour hoarding

rm(list=ls())

#library(devtools)

# devtools::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

library(cli)
library(tidyverse)
library(data.table)
library(theme61)
library(tidyr)
library(readxl)
library(gghighlight)
library(readabs)
library(OECD)
library(jsonlite)
library(httr)
library(Synth)
library(seasonal)
library(mFilter)
library(zoo)
library(Hmisc)


## First construct the labour supply and demand plots

labor_market <- data.frame(
  Labor = seq(0, 10, by = 1),
  Demand1 = 12 - 1 * seq(0, 10, by = 1),  # Initial Demand Curve
  Demand2 = 14 - 1 * seq(0, 10, by = 1),  # Increased Demand Curve
  Demand3 = 10 - 1 * seq(0, 10, by = 1),  # Decreased Demand Curve
  Supply1 = 2 + 1 * seq(0, 10, by = 1),  # Initial Supply Curve
  Supply2 = 0 + 1 * seq(0, 10, by = 1)   # Increased Supply Curve
)

# Plot with initial labor demand and supply curves
ggplot(labor_market, aes(x = Labor)) +
  geom_line(aes(y = Demand1, color = "Demand"), size = 1.2) +
  geom_line(aes(y = Supply1, color = "Supply"), size = 1.2) +
  labs(y = "Wage", x = "Labor",
       title = "Labor Market with Initial Demand and Supply",
       color = "Curves") +
  scale_color_manual(values = c("Demand" = "red", "Supply" = "blue")) +
  scale_y_continuous(labels = scales::dollar,limits=c(0,15)) +  
  theme_minimal() +
  theme(plot.title = element_text(size = 20))

ggplot2::ggsave("LDLS.jpg",dpi=100)

ggplot(labor_market, aes(x = Labor)) +
  geom_line(aes(y = Demand1, color = "Initial Demand"), size = 1.2) +
  geom_line(aes(y = Demand3, color = "Reduced Demand"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = Supply1, color = "Supply"), size = 1.2) +
  labs(y = "Wage", x = "Labor",
       title = "Labor Market with Reduced Demand",
       color = "Curves") +
  scale_color_manual(values = c("Initial Demand" = "red", "Reduced Demand" = "orange", "Supply" = "blue")) +
  scale_y_continuous(labels = scales::dollar,limits=c(0,15)) +  
  theme_minimal() +
  theme(plot.title = element_text(size = 20))

ggplot2::ggsave("LDLS-demanddrop.jpg",dpi=100)


ggplot(labor_market, aes(x = Labor)) +
  geom_line(aes(y = Demand1, color = "Initial Demand"), size = 1.2) +
  geom_line(aes(y = Demand2, color = "Increased Demand"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = Supply1, color = "Supply"), size = 1.2) +
  labs(y = "Wage", x = "Labor",
       title = "Labor Market with Increased Demand",
       color = "Curves") +
  scale_color_manual(values = c("Initial Demand" = "red", "Increased Demand" = "green", "Supply" = "blue")) +
  scale_y_continuous(labels = scales::dollar,limits=c(0,15)) +  
  theme_minimal() +
  theme(plot.title = element_text(size = 20))

ggplot2::ggsave("LDLS-demandrise.jpg",dpi=100)


ggplot(labor_market, aes(x = Labor)) +
  geom_line(aes(y = Demand1, color = "Initial Demand"), size = 1.2) +
  geom_line(aes(y = Demand2, color = "Increased Demand"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = Supply1, color = "Initial Supply"), size = 1.2) +
  geom_line(aes(y = Supply2, color = "Increased Supply"), size = 1.2, linetype = "dashed") +
  labs(y = "Wage", x = "Labor",
       title = "Labor Market with Increased Demand and Supply",
       color = "Curves") +
  scale_color_manual(values = c("Initial Demand" = "red", "Increased Demand" = "green", "Initial Supply" = "blue", "Increased Supply" = "purple")) +
  scale_y_continuous(labels = scales::dollar,limits=c(0,15)) +  
  theme_minimal() +
  theme(plot.title = element_text(size = 20))

ggplot2::ggsave("LDLS-twoshock.jpg",dpi=100)


## Pull in ABS data
# Start with wage data
wpi <- read_abs("6345.0")
setDT(wpi)

wpi_SA <- wpi[series_type == "Seasonally Adjusted" & data_type == "INDEX"][, series2 := sapply(str_split(series, ";\\s+"), function(x) x[4])]

ggplot(wpi_SA[],aes(x=date,y=value,colour=series2)) + 
  geom_line() + 
  theme_e61(legend="bottom")

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

ggplot(cpi_growth,aes(x=date,y=RW)) + geom_line() + geom_hline(yintercept = 0)

# Add average RW growth 2004 - 2019. For this get December 2019 WPI and CPI, and December 2004 WPI and CPI, and calculate annualised growth.

cpi_for_avg <- cpi[(date == "2004-12-01" | date == "2019-12-01") & table_title == "TABLES 1 and 2. CPI: All Groups, Index Numbers and Percentage Changes" & series == "Index Numbers ;  All groups CPI ;  Australia ;"][,.(date,cpi_value = value)]
setDT(cpi_for_avg)

wpi_for_avg <- wpi[series == "Quarterly Index ;  Total hourly rates of pay excluding bonuses ;  Australia ;  Private and Public ;  All industries ;" & (date == "2004-12-01" | date == "2019-12-01") & series_type == "Original" & table_title == "Table 1. Total Hourly Rates of Pay Excluding Bonuses: Sector, Original, Seasonally Adjusted and Trend"][,.(date,wpi_value = value)]
setDT(wpi_for_avg)

avg_RW <- cpi_for_avg[wpi_for_avg,on=.(date)][,RW := wpi_value/cpi_value]

avg_RW_growth <- (avg_RW$RW[2]/avg_RW$RW[1])^(1/15)-1

ggplot(cpi_growth,aes(x=date,y=RW/100)) + 
  geom_line() + 
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = avg_RW_growth,linetype = "dashed",colour = "red") +
  plot_label(c("Real wages","2004-2019 avg growth"),x=c(as.Date("2014-03-01"),as.Date("2014-03-01")),y=c(-0.015,-0.025),colour=c(palette_e61(1),"red")) +
  labs_e61(title = "Real wage growth",y="",x="") + 
  scale_y_continuous_e61(labels = scales::percent_format(),limits=c(-0.05,0.025,0.01),y_top=FALSE)


rw <- ggplot(cpi_growth,aes(x=date,y=RW/100)) + 
  geom_line() + 
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = avg_RW_growth,linetype = "dashed",colour = "red") +
  plot_label(c("Real wages","2004-2019 avg growth"),x=c(as.Date("2014-03-01"),as.Date("2014-03-01")),y=c(-0.015,-0.025),colour=c(palette_e61(1),"red")) +
  labs_e61(title = "Real wage growth",y="",x="") + 
  scale_y_continuous_e61(labels = scales::percent_format(),limits=c(-0.05,0.025,0.01),y_top=FALSE)

# Grab labour market data
LS <- read_abs(cat_no = "6202.0") 

LS_mini <- LS %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original") %>% filter(series == "Employed total ;  Persons ;" | series == "Unemployed total ;  Persons ;"  | series == "Unemployment rate ;  Persons ;" | series == "Participation rate ;  Persons ;") %>% dplyr::select("date","series","value")

LS2 <- pivot_wider(LS_mini,id_cols = "date",names_from = "series",values_from = "value")
colnames(LS2) <- c("date","Employed","Unemployed","UR_ABS","PR_ABS")
setDT(LS2)

emp_graph <- ggplot(LS2[date >= as.Date("2014-01-01")],aes(x=date,y=Employed/1000)) + 
  geom_line() +
  labs_e61(title = "Number employed (000s)",y="",x="") +
  scale_y_continuous_e61(limits = c(11,15,1))
  
save_e61("labour_quantity.png",res=2)

pr_graph <- ggplot(LS2[date >= as.Date("2014-01-01")],aes(x=date,y=PR_ABS)) + 
  geom_line()+
  labs_e61(title = "Participation Rate",subtitle = "% of working age population",y="",x="") +
  scale_y_continuous_e61(limits = c(60,68,2))

save_e61("labour_supply.png",res=2)

LS2 <- LS2 %>% mutate(UR_Derived = Unemployed/(Employed + Unemployed),QUnemp = frollmean(Unemployed,n=3,fill=NA,align="right"),QEmp =frollmean(Employed,n=3,fill=NA,align="right"),UR = UR_ABS) 

LSQ <- LS2[seq(5,nrow(LS2),3),] %>% dplyr::select(date,QUnemp,QEmp,UR)

# Add in vacancy rate data from labour accounts (dont add the imputation for this with imputation )
labour_acc_full <- read_abs(cat_no = "6150.0.55.003")
setDT(labour_acc_full)
unique(labour_acc_full$table_title)

labour_acc_full[, industry := str_match(table_title, "Table \\d+\\. (.*?) - Trend, Seasonally Adjusted and Original")[, 2]]
labour_acc_full <- labour_acc_full[!is.na(industry)]

# These accounts include values for Agriculture
labour_acc_full[,agg_ind := fcase(industry %in% c("Agriculture, Forestry and Fishing (A)","Mining (B)","Construction (E)","Manufacturing (C)","Electricity, Gas, Water and Waste Services (D)","Transport, Postal and Warehousing (I)","Wholesale Trade (F)"),"Primary and Secondary",
                                  industry %in% c("Retail Trade (G)","Accommodation and Food Services (H)","Rental, Hiring and Real Estate Services (L)"),"Sales",
                                  industry %in% c("Administrative and Support Services (N)","Financial and Insurance Services (K)" ,"Professional, Scientific and Technical Services (M)","Information Media and Telecommunications (J)"),"Private Services",
                                  industry %in% c("Education and Training (P)","Health Care and Social Assistance (Q)","Public Administration and Safety (O)"),"Public Services",
                                  industry %in% c("Arts and Recreation Services (R)","Other Services (S)"),"Other Services",
                                  industry %in% c("Total All Industries"),"Total",
                                  default = NA)]

labour_acc <- labour_acc_full[!is.na(agg_ind)]

unique(labour_acc[table_title == "Table 1. Total All Industries - Trend, Seasonally Adjusted and Original" & series_type=="Original" & date > as.Date("2012-12-30") & startsWith(series,"Jobs")]$series)

ggplot(labour_acc[table_title == "Table 1. Total All Industries - Trend, Seasonally Adjusted and Original" & series_type=="Seasonally Adjusted" & date > as.Date("2012-12-30") & startsWith(series,"Jobs; Proportion of vacant jobs ;")],aes(x=date,y=value,colour = agg_ind)) + 
  geom_line() + 
  labs_e61(title = "Vacancy rate (labour demand)",subtitle = "Vacancies as a % total jobs",y="",x="") +
  scale_y_continuous_e61(labels=scales::percent_format(scale=1))

save_e61("labour_demand.png",res=2)

#ggplot(labour_acc[table_title == "Table 1. Total All Industries - Trend, Seasonally Adjusted and Original" & series_type=="Seasonally Adjusted" & date > as.Date("1999-12-30") & startsWith(series,"Jobs; Proportion of vacant jobs ;")],aes(x=date,y=value,colour = agg_ind)) + geom_line() + theme_classic()



# Next labour productivity from output per hour (also visualise the employee level as a check)

GDP <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(series == "Gross domestic product: Chain volume measures ;") %>% filter(table_title == "Table 1. Key National Accounts Aggregates") %>% select("date","series","value")

Hours <- read_abs(cat_no = "5206.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(series == "Hours worked: Index ;") %>% filter(grepl('Table 1',table_title)) %>% drop_na %>% filter(unit == "Index Numbers") %>% select("date","value")

colnames(Hours) <- c("date","Hours")

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

LP_hour_graph <- LP_hour %>% pivot_longer(!date)
setDT(LP_hour_graph)

ggplot(LP_hour_graph[date >= as.Date("2014-01-01")],aes(x=date,y=value,colour=name)) + 
  geom_line() +
  labs_e61(title = "Labour productivity",subtitle="GDP per hour worked, 1980=1",y="",x="") +
  plot_label(c("Trend","Productivity"),x=c(as.Date("2014-03-01"),as.Date("2014-03-01")),y=c(1.81,1.83),colour=c(palette_e61(2)[1],palette_e61(2)[2])) +
  scale_y_continuous_e61(limits = c(1.7,1.92,0.05),y_top=FALSE)

lp_graph <- ggplot(LP_hour_graph[date >= as.Date("2014-01-01")],aes(x=date,y=value,colour=name)) + 
  geom_line() +
  labs_e61(title = "Labour productivity",subtitle="GDP per hour worked, 1980=1",y="",x="") +
  plot_label(c("Trend","Productivity"),x=c(as.Date("2014-03-01"),as.Date("2014-03-01")),y=c(1.81,1.83),colour=c(palette_e61(2)[1],palette_e61(2)[2])) +
  scale_y_continuous_e61(limits = c(1.7,1.92,0.05),y_top=FALSE)


save_e61(plotlist = list(rw,lp_graph),"wage_prod_png",res=2)





## Pull in OECD data
# No international comparison, save that for the future