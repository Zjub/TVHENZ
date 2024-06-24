rm(list=ls())
library(zoo)
library(tidyverse)
library(ggplot2)
library(readabs)
library(foreign)
library(theme61)
library(lubridate)
library(data.table)
library(plyr)
library(dplyr)

Sys.setenv(R_READABS_PATH="C:\\Users\\ZacharyHayward\\Documents\\Productivity")
productivity<- read_abs("5206.0") #, tables = c(1))

sep_productivity <- separate_series(productivity)

prod <- sep_productivity %>% filter(frequency == 'Quarter')
# this is the productivity index series 
PROD_IND <- productivity %>% filter(series_id == 'A2302474X')

#Here I am pulling out the hours worker and GDP such that I can calculate this myself (this could be elsewhere in the same place)
hours_worked <- read_abs("6291.0.55.001") %>% separate_series() %>% filter(series_3 == 'Worked 1 hour or more' & series_4 == 'Number of hours actually worked in all jobs' & series_5 == 'Persons')%>% mutate(quarters = zoo::as.yearqtr(date, format = "%Y-%m-%d"))
hours_worked <- hours_worked %>%  subset(select = c(value,quarters)) %>% group_by(quarters) %>% dplyr::summarize(HRS=mean(value))

GDP <- read_abs("5206.0") %>% separate_series() %>% filter(series_3 == 'Gross domestic product: Chain volume measures' & series_type=='Seasonally Adjusted') %>% mutate(quarters = zoo::as.yearqtr(date, format = "%Y-%m-%d"))
GDP <- GDP %>%  subset(select = c(value,quarters)) %>% group_by(quarters) %>% dplyr::summarize(GDP= sum(value))
#colnames(GDP) <- c('GDP', 'quarters')

labour <- read_abs("6202.0") %>% separate_series()
#Unemployment rate
labour_rates <- labour %>% filter(series_3== 'Persons'& series_2 == 'Labour force total'| series_2 == 'Unemployed total') %>% mutate(quarters = zoo::as.yearqtr(date, format = "%Y-%m-%d"))
labour_rates <- labour_rates %>% subset(select = c(quarters, value, series_2))
labour_rates <- labour_rates %>% group_by(quarters, series_2) %>% dplyr::summarize(val_q= sum(value)) 
labour_rates <- labour_rates %>% pivot_wider(id_cols= quarters, names_from = series_2, values_from= val_q)
colnames(labour_rates) <- c('quarters', "labour_force", 'unemployed')
labour_rates <- labour_rates %>%  subset(select = c(quarters, labour_force,unemployed)) %>% mutate(unemp_rate=unemployed*100/labour_force)

#We can read in the aggregated flows data now

flows <- fread('C:\\Users\\ZacharyHayward\\Documents\\Unemployment\\SAM\\Flows_data.csv')
flows_1 <- flows %>% group_by(Month, current, previous) %>% dplyr::summarize(flows= sum(persons_current_month))
flows_1 <- flows_1 %>% mutate(months=as.Date(as.yearmon(Month, "%B-%y")), quarters = zoo::as.yearqtr(months, format = "%Y-%m-%d")) %>% ungroup()
flows_1 <- flows_1 %>% group_by(quarters, current, previous) %>% dplyr::summarize(flows_q= sum(flows))
flows_2 <- flows_1 %>% filter(previous =='Employed full-time'& current == 'Employed part-time'|current == 'Unemployed') %>% group_by(quarters) %>% dplyr::summarize(E=sum(flows_q))
flows_3 <- flows_1 %>% filter(previous =='Unemployed' & current == 'Employed part-time'|current == 'Employed full-time' ) %>% group_by(quarters) %>% dplyr::summarize(U=sum(flows_q))

flows_1$UE <- ifelse(flows_1$current == 'Employed full-time' & flows_1$previous == 'Unemployed', 'UE', 0)
flows_1$EU <- ifelse(flows_1$current == 'Unemployed' & flows_1$previous == 'Employed full-time','EU', 0)

flows_1 <-  flows_1 %>% filter(UE=='UE'|EU=='EU') %>% mutate(STATUS=ifelse(UE=='UE', UE, 'EU'))  
FLOW_DATA <- flows_1 %>% pivot_wider(id_cols= quarters, names_from = STATUS, values_from= flows_q)
FLOW_DATA <- list(FLOW_DATA,flows_2,flows_3)%>% reduce(left_join, by='quarters')


HWI <- fread('C:\\Users\\ZacharyHayward\\Documents\\Unemployment\\SAM\\HWI.csv')
HWI <- data.table(HWI) %>% subset(select = -c(State))
HWI <- HWI %>% pivot_longer(cols = colnames(HWI),names_to = 'HWI')
colnames(HWI) <- c('date','HWI')
HWI <- HWI %>% mutate(months=as.Date(as.yearmon(date, "%B-%y")),quarters = zoo::as.yearqtr(months, format = "%Y-%m-%d")) %>% subset(select=-c(date, months))
HWI$HWI <-as.numeric(gsub(",","",HWI$HWI))
HWI <- HWI %>% group_by(quarters) %>% dplyr::summarize(HWI=sum(HWI))



ABS_DATA <- list(labour_rates, GDP, FLOW_DATA,hours_worked, HWI) %>% reduce(left_join, by='quarters')

write_csv(ABS_DATA,'C:\\Users\\ZacharyHayward\\Documents\\Unemployment\\SAM\\ABS_DATA.csv')


